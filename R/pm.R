#' Set the domain
#'
#' Set the domain id and domain name of the instance
#'
#' Set the domain id and domain name in the databases
#' and set the domain name in the webclient'
#'
#' @param domain_id The desired domain_id
#' @param domain_name The desired domain_name
#' @param host The host to connect to
#' @param admin Name of the database admin account
#' @param pass Password of the database admin account
#' @export
set_domain <- function(domain_id, domain_name, host = "", admin = "", pass = "")
{
  # Connect to the db
  hive <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2hive", user = admin, password = pass)
  pm   <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2pm",   user = admin, password = pass)

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

#' Get the domain
#'
#' Get the domain id of the instance
#'
#' @param host The host to connect to
#' @param admin Name of the database admin account
#' @param pass Password of the database admin account
#' @return The domain id and name
#' @export
get_domain <- function(host = "", admin = "", pass = "")
{
  dplyr::src_postgres("i2b2pm", host = host, user = admin, password = pass) %>%
    dplyr::tbl("pm_hive_data") %>%
    dplyr::collect()
}

#' Add a project
#'
#' Add a project with its id, path and name in the hive
#'
#' Also create a specific database for this project
#'
#' @param project_id The desired project id
#' @param project_name The desired project name
#' @param host Address of the host, defaults to 127.0.0.1
#' @param admin Name of the database admin account
#' @param pass Password of the database admin account
#' @export
add_project <- function(project_id, project_name, host = "", admin = "", pass = "")
{
  # Get the current domain id
  domain_id <- get_domain(host, admin, pass)$domain_id

  # Connect to the db
  hive <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host, dbname = "i2b2hive", user = admin, password = pass)
  pm   <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host, dbname = "i2b2pm",   user = admin, password = pass)

  # Set the project id to all cells in i2b2hive
  RPostgreSQL::dbGetQuery(hive, stringr::str_c("INSERT INTO im_db_lookup (c_domain_id, c_project_path, c_owner_id, c_db_fullschema, c_db_datasource, c_db_servertype, c_db_nicename) VALUES ('", domain_id, "', '", project_id, "/', '@', 'public', 'java:/IMDemoDS', 'POSTGRESQL', 'IM');"))
  RPostgreSQL::dbGetQuery(hive, stringr::str_c("INSERT INTO ont_db_lookup (c_domain_id, c_project_path, c_owner_id, c_db_fullschema, c_db_datasource, c_db_servertype, c_db_nicename) VALUES ('", domain_id, "', '", project_id, "/', '@', 'public', 'java:/OntologyDemoDS', 'POSTGRESQL', 'Metadata');"))
  RPostgreSQL::dbGetQuery(hive, stringr::str_c("INSERT INTO work_db_lookup (c_domain_id, c_project_path, c_owner_id, c_db_fullschema, c_db_datasource, c_db_servertype, c_db_nicename) VALUES ('", domain_id, "', '", project_id, "/', '@', 'public', 'java:/WorkplaceDemoDS', 'POSTGRESQL', 'Workplace');"))
  RPostgreSQL::dbGetQuery(hive, stringr::str_c("INSERT INTO crc_db_lookup (c_domain_id, c_project_path, c_owner_id, c_db_fullschema, c_db_datasource, c_db_servertype, c_db_nicename) VALUES ('", domain_id, "', '/", project_id, "/', '@', 'public', 'java:/QueryTool", project_id, "DS', 'POSTGRESQL', '", project_name,"');"))

  # Set the project id and name in pm_hive_data
  RPostgreSQL::dbGetQuery(pm, stringr::str_c("INSERT INTO pm_project_data (project_id, project_name, project_wiki, project_path) VALUES ('", project_id, "', '", project_name, "', 'http://www.i2b2.org', '/", project_id, "');"))

  RPostgreSQL::dbGetQuery(hive, stringr::str_c("CREATE DATABASE i2b2", project_id, "data WITH TEMPLATE i2b2demodata owner i2b2demodata;"))

  # Disconnect the db
  RPostgreSQL::dbDisconnect(hive)
  RPostgreSQL::dbDisconnect(pm)
}

#' List projects
#'
#' List available projects in the hive
#'
#' @param host Address of the host, defaults to 127.0.0.1
#' @param admin Name of the database admin account
#' @param pass Password of the database admin account
#' @return The list of projects
#' @export
list_projects <- function(host = "", admin = "", pass = "")
{
  dplyr::src_postgres(dbname = "i2b2pm", host = host, user = admin, password = pass) %>%
    dplyr::tbl("pm_project_data") %>%
    dplyr::collect()
}


#' Delete a project
#'
#' Delete a project from the hive
#'
#' @param project_id The project to delete
#' @param host Address of the host, defaults to 127.0.0.1
#' @param admin Name of the database admin account
#' @param pass Password of the database admin account
#' @export
delete_project <- function(project_id, host = "", admin = "", pass = "")
{
  # Connect to the db
  hive <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2hive", user = admin, password = pass)
  pm <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host, dbname = "i2b2pm", user = admin, password = pass)

  c("im", "ont", "work") %>%
    purrr::walk(~RPostgreSQL::dbGetQuery(hive, stringr::str_c("DELETE FROM ", .x, "_db_lookup WHERE c_project_path = '", project_id, "/';")))
  RPostgreSQL::dbGetQuery(hive, stringr::str_c("DELETE FROM crc_db_lookup WHERE c_project_path = '/", project_id, "/';"))

  RPostgreSQL::dbGetQuery(pm, stringr::str_c("DELETE FROM pm_project_data WHERE project_id = '", project_id, "';"))

  RPostgreSQL::dbGetQuery(pm, stringr::str_c("DROP DATABASE i2b2", project_id, "demodata;"))

  # Disconnect the db
  RPostgreSQL::dbDisconnect(hive)
  RPostgreSQL::dbDisconnect(pm)
}

