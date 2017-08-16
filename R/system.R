#' Clear the default credentials in the webclient
#'
#' Clear the default credentials in the webclient
#'
#' @export
clear_webclient <- function()
{
  "/var/www/html/webclient/js-i2b2/i2b2_ui_config.js" %>%
    readLines %>%
    stringr::str_c(collapse = "\n") %>%
    stringr::str_replace("demo", "") %>%
    stringr::str_replace("demouser", "") %>%
  write("/var/www/html/webclient/js-i2b2/i2b2_ui_config.js")
}

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

#' Manage services
#'
#' Start/stop/restart services
#'
#' @param service The service to manage (pg for short for postgresql-9.1)
#' @param action The action to perform
#' @export
service <- function(service, action = c("start", "stop", "restart"), use_sudo = T)
{
  ifelse(service == "pg", "postgresql-9.1", service)
  cmd <- ifelse(use_sudo, "sudo service", "service")
  system(stringr::str_c(cmd, service, action, sep = " "))
}

# #' Secure the i2b2 databases
# #'
# #' Secure the i2b2 databases
# #'
# #' Connect to the database using the admin account credentials, provided as admin and pass,
# #' generate pass_length long passwords for each database
# #' and update the cells config with the new passwords.
# #'
# #' @param admin Name of the database admin account
# #' @param pass Password of the database admin account
# #' @param pass_length Length of the generated passwords
# #' @return A vector for one statistic column
# #' @export
# secure_db <- function(admin, pass, pass_length = 8)
# {
#   # Connect to the db
#   con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = "127.0.0.1", user = admin, password = pass)

# # Generate passwords
#   accounts <- c("demodata", "hive", "imdata", "metadata", "pm", "workdata")
#   passwords <- accounts %>% sapply(function(x){create_password(pass_length)})

# # Change db accounts passwords
#   accounts %>%
#   sapply(function(x)
#          {
#            RPostgreSQL::dbGetQuery(con, stringr::str_c("alter user i2b2", x, " password '", passwords[x], "';"))
#          })

#   path <- "/opt/wildfly-10.0.0.Final/standalone/deployments/"

#   # Modify all passwords in all cells config files accordingly
#   stringr::str_c(c("crc", "im", "ont", "pm", "work"), "-ds.xml") %>%
#     purrr::map(function(x)
#         {
#           stringr::str_c(path, x) %>%
#             readLines %>%
#             stringr::str_c(collapse = "\n") %>%
#             {
#               config <- .

#               for(acc in accounts)
#               {
#                 config %>%
#                   stringr::str_replace_all(stringr::str_c("<user-name>i2b2", acc, "</user-name>\n(\t*)<password>[^<]*</password>"),
#                                   stringr::str_c("<user-name>i2b2", acc, "</user-name>\n\\1<password>",passwords[acc],"</password>")) -> config
#               }

#               config
#             } %>%
#               write(stringr::str_c(path, x))
#         })

#   # Disconnect the db
#   RPostgreSQL::dbDisconnect(con)

#   accounts %>%
#     purrr::walk(~print(stringr::str_c("Password for database user i2b2", .x, " set to: ", passwords[.x])))
# }
