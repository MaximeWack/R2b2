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
  system(str_c("useradd ", name, " -g users -G wildfly -m"))
  system(str_c("echo \"", name, ":", pass, "\" | chpasswd"))
  print(str_c(name, " system account created with password: ", pass))
  
# Connect to the db
  con <- dbConnect(PostgreSQL(), user = "root", password = "demouser")
  
# Create the database user and its database
  dbGetQuery(con, str_c("create user ", name, " with superuser createrole createdb password '", pass, "';"))
  dbGetQuery(con, str_c("create database ", name, ";"))
  print(str_c(name, " postgresql account created with password: ", pass))

# Reset the root account password
  dbGetQuery(con, str_c("alter user root password '", pass, "';"))
  print(str_c("Changed password for user root to: ", pass))

# Disconnect the db
  dbDisconnect(con)

  pass
}

secure_db <- function(name, pass, pass_length = 8)
{
# Connect to the db
  con <- dbConnect(PostgreSQL(), user = name, password = pass)

# Generate passwords
  accounts <- c("demodata", "hive", "imdata", "metadata", "pm", "workdata")
  passwords <- accounts %>% sapply(function(x){create_password(pass_length)})

# Change db accounts passwords
  accounts %>%
  sapply(function(x)
         {
           dbGetQuery(con, str_c("alter user i2b2", x, " password '", passwords[x], "';"))
         })

  path <- "/opt/wildfly-10.0.0.Final/standalone/deployments/" 

  # Modify cells config files accordingly
  str_c(c("crc", "im", "ont", "pm", "work"), "-ds.xml") %>%
    map(function(x)
        {
          str_c(path, x) %>%
            read_file %>%
            {
              config <- .

              for(acc in accounts)
              {
                config %>% 
                  str_replace_all(str_c("<user-name>i2b2", acc, "</user-name>\n(\t*)<password>[^<]*</password>"),
                                  str_c("<user-name>i2b2", acc, "</user-name>\n\\1<password>",passwords[acc],"</password>")) -> config
              }

              config
            } %>%
            write_file(str_c(path, x))
        })

  # Disconnect the db
  dbDisconnect(con)

  accounts %>%
    walk(~print(str_c("Password for database user i2b2", .x, " set to: ", passwords[.x])))
}
