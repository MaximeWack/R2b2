#' Make a fresh install from the base VM
#'
#' Make a fresh install from the base VM
#'
#' Set the permissions,
#' create the admin account for the system, database, and i2b2
#' Set the domain and project
#' Delete the default users
#' Clear the metadata database, and the *_dimension tables in demodata
#'
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param domain_id The desired domain_id
#' @param domain_name The desired domain_name
#' @export
fresh_install <- function(admin, pass, domain_id, domain_name)
{
  # System
  set_permissions()

  create_admin(admin, pass)

  # Clear the default install
  clear_webclient()

  clear_default_workdata()

  clear_default_metadata()

  clear_default_imdata()

  clear_default_demodata()

  # Set the new domain
  set_domain(domain_id, domain_name)

  # Add new admin
  add_users("i2b2", "demouser", data.frame(id = admin, password = pass, name = admin, email = "", role = "ADMIN", project = "@"))

  # Prepare to clone i2b2demodata
  service("pg", "restart")

  add_project("main", "Main Project")

  add_user_roles("i2b2", "demouser", "admin", "main", c("MANAGER", "USER", "DATA_PROT"))

  # Clean old users
  delete_users(c("i2b2","demo"))

  # Add ontologies
  # add_ont("Name", "Scheme")

  # Populate the ontologies
  # populate_ont(readr::read_csv(".ont"), readr::read_csv(".modi"), "Scheme")

  # Populate the concept/provider tables needed
  # populate_concept(readr::read_csv(".ont", readr::read_csv(".modi", "Scheme", "main")))

  # Restart wildfly
  service("jboss", "restart")
}
