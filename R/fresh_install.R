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

  add_project("CHRU")

  add_user_roles("i2b2", "demouser", "maxx", "CHRU", c("MANAGER", "USER", "DATA_PROT"))  

  # Clean old users
  delete_users(c("i2b2","demo"))

  # Add ontologies
  add_ont("Diagnostics", "CIM")
  add_ont("Actes", "CCAM")
  add_ont("Patients", "PAT")
  add_ont("Hospitalisations", "HOS")
  add_ont("Services", "STRUCT")
  add_ont("Biologies", "BIO")

  # Populate the ontologies
  populate_ont(readr::read_csv("..inst/cim.ont"), readr::read_csv("..inst/cim.modi"), "CIM")
  populate_ont(readr::read_csv("../inst/ccam.ont"), modi = NULL, "CCAM")
  populate_ont(readr::read_csv("../inst/patients.ont"), modi = NULL, "PAT", include_code = F)
  populate_ont(readr::read_csv("../inst/hospit.ont"), modi = NULL, "HOS", include_code = F)
  populate_ont(readr::read_csv("../inst/struct.ont"), modi = NULL, "STRUCT")
  populate_ont(readr::read_csv("../inst/bio.ont"), modi = NULL, "BIO", include_code = F)

  # Populate the concept/provider tables needed
  populate_concept(readr::read_csv("../inst/cim.ont"), readr::read_csv("../inst/cim.modi"), "CIM", "CHRU")
  populate_concept(readr::read_csv("../inst/ccam.ont"), modi = NULL, "CCAM", "CHRU")
  populate_concept(readr::read_csv("../inst/bio.ont"), modi = NULL, "BIO", "CHRU")
  populate_provider(readr::read_csv("../inst/struct.ont"), "STRUCT", "CHRU")
  populate_concept(readr::read_csv("../inst/hospit.ont"), modi = NULL, "HOS", "CHRU")
  populate_concept(readr::read_csv("../inst/patients.ont"), modi = NULL, "PAT", "CHRU")

  # Restart wildfly
  service("jboss", "restart")
}

pop_chru <- function()
{
  readr::read_csv("~/2016/pims16.csv", col_types = readr::cols(.default = readr::col_character())) -> patients

  patients %>%
  import_patients_visits("CHRU")

  readr::read_csv("~/2016/diags16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_diagnostics("CHRU")

  readr::read_csv("~/2016/actes16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_actes("CHRU")

  readr::read_csv("~/2016/mensurations16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_mensurations(patients, "CHRU")

  readr::read_csv("~/2016/bio16_1.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_bios(patients, "CHRU")

  readr::read_csv("~/2016/bio16_2.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_bios(patients, "CHRU")

  readr::read_csv("~/2016/pims17.csv", col_types = readr::cols(.default = readr::col_character())) -> patients

  patients %>%
  import_patients_visits("CHRU")

  readr::read_csv("~/2016/diags17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_diagnostics("CHRU")

  readr::read_csv("~/2016/actes17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_actes("CHRU")

  readr::read_csv("~/2016/mensurations17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_mensurations(patients, "CHRU")

  readr::read_csv("~/2016/bios17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_bios(patients, "CHRU")
}

