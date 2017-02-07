#' Set the permissions for the webclient and wildfly folders
#'
#' Set the permissions wildfly:wildfly +rwx and setgid to the webclient and wildfly folders
#'
#' @export
set_permissions <- function()
{
  system("chown wildfly:wildfly /var/www/html -R")
  system("chmod 775 /var/www/html -R")
  system("chmod 775 /opt/wildfly-10.0.0.Final -R")
  system("chmod g+s /var/www/html -R")
  system("chmod g+s /opt/wildfly-10.0.0.Final -R")
}

#' Create system and database admin accounts
#'
#' Create system and database admin accounts
#'
#' Create a system and database admin accounts
#' The name of the account is i2b2admin by default
#' The shared password can be provided, or randomly generated (length = 8 characters by default)
#' The system account belongs in the users and wildfly groups
#'
#' @param admin Name of the admin account to create
#' @param pass An arbitrary password if provided, i2b2admin by default
#' @param pass_length The length of the generated password, 8 characters by default
#' @return The generated password
#' @export
create_admin <- function(admin = "i2b2admin", pass= NULL, pass_length = 8)
{
# Generate a new password of default length 10
  if (is.null(pass))
    pass <- create_password(pass_length)

# Create the system user
  system(stringr::str_c("useradd ", admin, " -g users -G wildfly -m"))
  system(stringr::str_c("echo \"", admin, ":", pass, "\" | chpasswd"))
  print(stringr::str_c(admin, " system account created with password: ", pass))
  
# Connect to the db
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = "127.0.0.1", user = "postgres", password = "demouser")
  
# Create the database user and its database
  RPostgreSQL::dbGetQuery(con, stringr::str_c("create user ", admin, " with superuser createrole createdb password '", pass, "';"))
  RPostgreSQL::dbGetQuery(con, stringr::str_c("create database ", admin, ";"))
  print(stringr::str_c(admin, " postgresql account created with password: ", pass))

# Reset the root account password
  RPostgreSQL::dbGetQuery(con, stringr::str_c("alter user postgres password '", pass, "';"))
  print(stringr::str_c("Changed password for user postgres to: ", pass))

# Disconnect the db
  RPostgreSQL::dbDisconnect(con)

  pass
}

#' Secure the i2b2 databases
#'
#' Secure the i2b2 databases
#'
#' Connect to the database using the admin account credentials, provided as admin and pass,
#' generate pass_length long passwords for each database
#' and update the cells config with the new passwords.
#' 
#' @param admin Name of the database admin account
#' @param pass Password of the database admin account
#' @param pass_length Length of the generated passwords
#' @return A vector for one statistic column
#' @export
secure_db <- function(admin, pass, pass_length = 8)
{
# Connect to the db
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = "127.0.0.1", user = admin, password = pass)

# Generate passwords
  accounts <- c("demodata", "hive", "imdata", "metadata", "pm", "workdata")
  passwords <- accounts %>% sapply(function(x){create_password(pass_length)})

# Change db accounts passwords
  accounts %>%
  sapply(function(x)
         {
           RPostgreSQL::dbGetQuery(con, stringr::str_c("alter user i2b2", x, " password '", passwords[x], "';"))
         })

  path <- "/opt/wildfly-10.0.0.Final/standalone/deployments/" 

  # Modify all passwords in all cells config files accordingly
  stringr::str_c(c("crc", "im", "ont", "pm", "work"), "-ds.xml") %>%
    purrr::map(function(x)
        {
          stringr::str_c(path, x) %>%
            readLines %>%
            stringr::str_c(collapse = "\n") %>%
            {
              config <- .

              for(acc in accounts)
              {
                config %>% 
                  stringr::str_replace_all(stringr::str_c("<user-name>i2b2", acc, "</user-name>\n(\t*)<password>[^<]*</password>"),
                                  stringr::str_c("<user-name>i2b2", acc, "</user-name>\n\\1<password>",passwords[acc],"</password>")) -> config
              }

              config
            } %>%
              write(stringr::str_c(path, x))
        })

  # Disconnect the db
  RPostgreSQL::dbDisconnect(con)

  accounts %>%
    purrr::walk(~print(stringr::str_c("Password for database user i2b2", .x, " set to: ", passwords[.x])))
}

#' Set the domain
#'
#' Set the domain id and domain name of the instance
#'
#' Set the domain id and domain name in the databases
#' and set the domain name in the webclient'
#' 
#' @param admin Name of the database admin account
#' @param pass Password of the database admin account
#' @param domain_id The desired domain_id
#' @param domain_name The desired domain_name
#' @export
set_domain <- function(admin, pass, domain_id, domain_name)
{
# Connect to the db
  hive <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = "127.0.0.1", dbname = "i2b2hive", user = admin, password = pass)
  pm   <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = "127.0.0.1", dbname = "i2b2pm",   user = admin, password = pass)

# Set the domain id to all cells in i2b2hive
  c("crc", "im", "ont", "work") %>%
    purrr::walk(~RPostgreSQL::dbGetQuery(hive, stringr::str_c("UPDATE ", .x, "_db_lookup SET c_domain_id = '", domain_id, "';")))

# Set the domain id and name in pm_hive_data
  RPostgreSQL::dbGetQuery(pm, stringr::str_c("UPDATE pm_hive_data SET domain_id = '", domain_id, "', domain_name = '", domain_id, "';"))

# Set the domain name for the webclient
  "/var/www/html/webclient/i2b2_config_data.js" %>%
    readLines %>%
    stringr::str_c(collapse = "\n") %>%
    stringr::str_replace("domain: *\"[^\"]+\"", stringr::str_c("domain: \"", domain_id, "\"")) %>%
    stringr::str_replace("name: *\"[^\"]+\"",   stringr::str_c("name: \"", domain_name, "\"")) %>%
    write(file = "/var/www/html/webclient/i2b2_config_data.js")

  # Disconnect the db
  RPostgreSQL::dbDisconnect(hive)
  RPostgreSQL::dbDisconnect(pm)
}

#' Set the project
#'
#' Set the project id, project path and project name of the instance
#'
#' Set the project id, project path and project name in the databases
#'
#' @param host Address of the host, defaults to 127.0.0.1
#' @param admin Name of the database admin account
#' @param pass Password of the database admin account
#' @param project_id The desired project id
#' @param project_name The desired project name
#' @export
set_project <- function(host = "localhost", admin, pass, project_id, project_name)
{
# Connect to the db
  hive <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host, dbname = "i2b2hive", user = admin, password = pass)
  pm   <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host, dbname = "i2b2pm",   user = admin, password = pass)

# Set the project id to all cells in i2b2hive
  c("im", "ont", "work") %>%
    purrr::walk(~RPostgreSQL::dbGetQuery(hive, stringr::str_c("UPDATE ", .x, "_db_lookup SET c_project_path = '", project_id, "/';")))
  RPostgreSQL::dbGetQuery(hive, stringr::str_c("UPDATE crc_db_lookup SET c_project_path = '/", project_id, "/';"))

# Set the project id and name in pm_hive_data
  RPostgreSQL::dbGetQuery(pm, stringr::str_c("UPDATE pm_project_data SET project_id = '", project_id, "', project_name = '", project_name, "', project_path = '/", project_id, "';"))
  RPostgreSQL::dbGetQuery(pm, stringr::str_c("UPDATE pm_project_user_roles SET project_id = '", project_id, "' WHERE (project_id = 'Demo');"))

  # Disconnect the db
  RPostgreSQL::dbDisconnect(hive)
  RPostgreSQL::dbDisconnect(pm)
}
