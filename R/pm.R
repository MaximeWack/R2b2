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
  RPostgreSQL::dbGetQuery(pm, stringr::str_c("INSERT INTO pm_project_data (project_id, project_name, project_wiki, project_path, status_cd) VALUES ('", project_id, "', '", project_name, "', 'http://www.i2b2.org', '/", project_id, "', 'A');"))

  RPostgreSQL::dbGetQuery(hive, stringr::str_c("CREATE SCHEMA i2b2", project_id, "data;"))
  RPostgreSQL::dbGetQuery(hive, stringr::str_c("CREATE DATABASE i2b2", project_id, "data WITH TEMPLATE i2b2demodata owner i2b2demodata;"))

  # Add the AGG_SERVICE_ACCOUNT user to the project
  RPostgreSQL::dbGetQuery(pm, stringr::str_c("INSERT INTO pm_project_user_roles (project_id, user_id, user_role_cd, status_cd) VALUES ('", project_id,"', 'AGG_SERVICE_ACCOUNT', 'USER', 'A'), ('", project_id,"', 'AGG_SERVICE_ACCOUNT', 'MANAGER', 'A'), ('", project_id,"', 'AGG_SERVICE_ACCOUNT', 'DATA_OBFSC', 'A'), ('", project_id,"', 'AGG_SERVICE_ACCOUNT', 'DATA_AGG', 'A');"))

  # Disconnect the db
  RPostgreSQL::dbDisconnect(hive)
  RPostgreSQL::dbDisconnect(pm)

# crc-ds.xml
  xml2::read_html("/opt/wildfly-10.0.0.Final/standalone/deployments/crc-ds.xml") %>%
    rvest::xml_nodes("datasource") %>%
    xml2::as_list() -> crc

  datasource <- list()
  datasource$datasource$`connection-url` <- list(stringr::str_c("jdbc:postgresql://localhost:5432/i2b2", stringr::str_to_lower(project_id), "data"))
  datasource$datasource$`driver-class` <- list("org.postgresql.Driver")
  datasource$datasource$driver <- list("postgresql-9.2-1002.jdbc4.jar")
  datasource$datasource$security$`user-name` <- list("i2b2demodata")
  datasource$datasource$security$password <- list("demouser")
  datasource$datasource$validation$`validate-on-match` <- list("false")
  datasource$datasource$validation$`background-validation` <- list("false")
  datasource$datasource$statement$`share-prepared-statements` <- list("false")
  attr(datasource$datasource, "jta") <- "false"
  attr(datasource$datasource, "jndi-name") <- stringr::str_c("java:/QueryTool", project_id, "DS")
  attr(datasource$datasource, "pool-name") <- stringr::str_c("QueryTool", project_id, "DS")
  attr(datasource$datasource, "enabled") <- "true"
  attr(datasource$datasource, "use-ccm") <- "false"

  c(crc, datasource) %>%
    stats::setNames(rep("datasource", length(.))) -> crc

  new <- list(datasources = crc)
  attr(new$datasources, "xmlns") <- "http://www.jboss.org/ironjacamar/schema"

  xml2::write_xml(new %>% xml2::as_xml_document(), "/opt/wildfly-10.0.0.Final/standalone/deployments/crc-ds.xml")
}

#' List projects
#'
#' List available projects in the hive
#'
#' @param host Address of the hostdefaults to 127.0.0.1
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

  RPostgreSQL::dbGetQuery(pm, stringr::str_c("DROP DATABASE i2b2", project_id, "data;"))

  # Disconnect the db
  RPostgreSQL::dbDisconnect(hive)
  RPostgreSQL::dbDisconnect(pm)

# Change crc-ds.xml
  xml2::read_html("/opt/wildfly-10.0.0.Final/standalone/deployments/crc-ds.xml") %>%
    rvest::xml_nodes(stringr::str_c("datasource[jndi-name!='java:/QueryTool", project_id, "DS']")) %>%
    xml2::as_list() %>%
    stats::setNames(rep("datasource", length(.))) ->
  datasources

  new <- list(datasources = datasources)
  attr(new$datasources, "xmlns") <- "http://www.jboss.org/ironjacamar/schema"

  xml2::write_xml(new %>% xml2::as_xml_document(), "/opt/wildfly-10.0.0.Final/standalone/deployments/crc-ds.xml")
}

