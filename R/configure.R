set_permissions <- function()
{
  system("chown wildfly:wildfly /var/www/html -R")
  system("chmod 775 /var/www/html -R")
  system("chmod 775 /opt/wildfly-10.0.0.Final -R")
  system("chmod g+s /var/www/html -R")
  system("chmod g+s /opt/wildfly-10.0.0.Final -R")
}

create_admin <- function(name = "i2b2admin", pass= NULL, pass_length = 8)
{
# Generate a new password of default length 10
  if (is.null(pass))
    pass <- create_password(pass_length)

# Create the system user
  system(stringr::str_c("useradd ", name, " -g users -G wildfly -m"))
  system(stringr::str_c("echo \"", name, ":", pass, "\" | chpasswd"))
  print(stringr::str_c(name, " system account created with password: ", pass))
  
# Connect to the db
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), user = "root", password = "demouser")
  
# Create the database user and its database
  RPostgreSQL::dbGetQuery(con, stringr::str_c("create user ", name, " with superuser createrole createdb password '", pass, "';"))
  RPostgreSQL::dbGetQuery(con, stringr::str_c("create database ", name, ";"))
  print(stringr::str_c(name, " postgresql account created with password: ", pass))

# Reset the root account password
  RPostgreSQL::dbGetQuery(con, stringr::str_c("alter user root password '", pass, "';"))
  print(stringr::str_c("Changed password for user root to: ", pass))

# Disconnect the db
  RPostgreSQL::dbDisconnect(con)

  pass
}

secure_db <- function(name, pass, pass_length = 8)
{
# Connect to the db
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), user = name, password = pass)

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

  # Modify cells config files accordingly
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
