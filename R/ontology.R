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
add_ontology <- function(host = "127.0.0.1", admin, pass, name, scheme, description, ont, modi)
{
  add_ont(host, admin, pass, name, scheme, description)

  populate_ont(host, admin, pass, ont, modi, name, scheme)

  populate_concept(host, admin, pass, ont, modi, name, scheme)
}
