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
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param domain_id The desired domain_id
#' @param domain_name The desired domain_name
#' @param project_id The desired project id
#' @param project_name The desired project name
#' @export
fresh_install <- function(admin, pass, domain_id, domain_name, project_id, project_name)
{
  host <- "127.0.0.1"

  set_permissions()

  create_admin(admin, pass)

  set_domain(admin, pass, domain_id, domain_name)

  set_project(host, admin, pass, project_id, project_name)

  add_users(domain_id, "i2b2", "demouser", data.frame(id = admin, password = pass, name = admin, email = "", role = "ADMIN", project = project_id))

  delete_users(host, admin, pass, c("i2b2", "demo"))

  clear_default_metadata(host, admin, pass)

  clear_concept(host, admin, pass)

  clear_modifier(host, admin, pass)
}
