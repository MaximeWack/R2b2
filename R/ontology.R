#' Add an ontology to i2b2
#'
#' Add an ontology into i2b2
#' Add the ontology table and entries in metadata
#' Populate the ontology table
#' Populate the concept and modifier dimensions in demodata
#'
#' @param name The name of the new ontology
#' @param scheme The scheme to use for this ontology
#' @param ont The ontology to insert
#' @param modi The modifiers to insert
#' @param include_code Whether to include the code in the label or not
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
add_ontology <- function(name, scheme, ont, modi = NULL, include_code = T, host = "", admin = "", pass = "")
{
  add_ont(name, scheme, host, admin, pass)

  populate_ont(ont, modi, name, scheme, include_code, host, admin, pass)
}

#' Delete an ontology from i2b2
#'
#' Delete an existing ontology from metadata
#'
#' Delete the corresponding table
#' Delete the scheme in schemes table
#' Delete the entry in table_acess
#'
#' @param scheme The scheme to use for this ontology
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
delete_ontology <- function(scheme, host = "", admin = "", pass = "")
{
  delete_ont(scheme, host, admin, pass)
}
