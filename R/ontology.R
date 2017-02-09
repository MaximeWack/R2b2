#' Add an ontology to i2b2
#'
#' Add an ontology into i2b2
#' Add the ontology table and entries in metadata
#' Populate the ontology table
#' Populate the concept and modifier dimensions in demodata
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param name The name of the new ontology
#' @param scheme The scheme to use for this ontology
#' @param description The description of the scheme
#' @param ont The ontology to insert
#' @param modi The modifiers to insert
#' @export
add_ontology <- function(host = "127.0.0.1", admin, pass, name, scheme, description, ont, modi = NULL)
{
  add_ont(host, admin, pass, name, scheme, description)

  populate_ont(host, admin, pass, ont, modi, name, scheme)

  populate_concept(host, admin, pass, ont, modi, name, scheme)
}

#' Delete an ontology from metadata
#'
#' Delete an existing ontology from metadata
#'
#' Delete the corresponding table
#' Delete the scheme in schemes table
#' Delete the entry in table_acess
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param scheme The scheme to use for this ontology
#' @export
delete_ontology <- function(host = "127.0.0.1", admin, pass, scheme)
{
  metadata   <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2metadata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("DROP TABLE ", scheme, ";"))
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("DELETE FROM table_access WHERE (c_table_cd = '", scheme, "');"))
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("DELETE FROM schemes WHERE (c_name = '", scheme, "');"))

  RPostgreSQL::dbDisconnect(metadata)
}
