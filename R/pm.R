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
