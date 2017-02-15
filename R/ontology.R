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
#' @param include_code Whether to include the code in the label or not
#' @export
add_ontology <- function(host = "127.0.0.1", admin, pass, name, scheme, description, ont, modi = NULL, include_code = T)
{
  add_ont(host, admin, pass, name, scheme, description)

  populate_ont(host, admin, pass, ont, modi, name, scheme, include_code)

  populate_concept(host, admin, pass, ont, modi, name, scheme)
}

#' Delete an ontology from i2b2
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
  delete_ont(host, admin, pass, scheme)

  delete_concept(host, admin, pass, scheme)

  delete_modifier(host, admin, pass, scheme)
}
