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

  add_project("CHRU", "Tous services")

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

accounts_obgyn <- function()
{
  add_user("i2b2", "demouser", "obgyn", "Gynécologie-Obstétrique", "", "obgyn")
  add_user("i2b2", "demouser", "amp", "AMP Clinique", "", "amp")
  add_user("i2b2", "demouser", "cancero", "Gynécologie et Cancérologie", "", "cancero")
  add_user("i2b2", "demouser", "ortho", "Orthogénie", "", "ortho")
  add_user("i2b2", "demouser", "ante", "Anténatal", "", "ante")
  add_user("i2b2", "demouser", "endoc", "Endocrinologie Maternité", "", "endoc")
  add_user("i2b2", "demouser", "post", "Post-natal", "", "post")
  add_user("i2b2", "demouser", "nn", "Nouveaux-nés", "", "nn")

  add_user_roles("i2b2", "demouser", "maxx", "620", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "maxx", "6040", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "maxx", "6050", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "maxx", "6060", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "maxx", "6070", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "maxx", "6080", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "maxx", "6090", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "maxx", "6100", c("MANAGER", "USER", "DATA_PROT"))

  add_user_roles("i2b2", "demouser", "obgyn", "CHRU", c("USER", "DATA_AGG"))
  add_user_roles("i2b2", "demouser", "obgyn", "620", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "obgyn", "6040", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "obgyn", "6050", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "obgyn", "6060", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "obgyn", "6070", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "obgyn", "6080", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "obgyn", "6090", c("MANAGER", "USER", "DATA_PROT"))
  add_user_roles("i2b2", "demouser", "obgyn", "6100", c("MANAGER", "USER", "DATA_PROT"))

  add_user_roles("i2b2", "demouser", "amp", "CHRU", c("USER", "DATA_OBFSC"))
  add_user_roles("i2b2", "demouser", "amp", "620", c("USER", "DATA_AGG"))
  add_user_roles("i2b2", "demouser", "amp", "6040", c("MANAGER", "USER", "DATA_PROT"))

  add_user_roles("i2b2", "demouser", "cancero", "CHRU", c("USER", "DATA_OBFSC"))
  add_user_roles("i2b2", "demouser", "cancero", "620", c("USER", "DATA_AGG"))
  add_user_roles("i2b2", "demouser", "cancero", "6050", c("MANAGER", "USER", "DATA_PROT"))

  add_user_roles("i2b2", "demouser", "ortho", "CHRU", c("USER", "DATA_OBFSC"))
  add_user_roles("i2b2", "demouser", "ortho", "620", c("USER", "DATA_AGG"))
  add_user_roles("i2b2", "demouser", "ortho", "6060", c("MANAGER", "USER", "DATA_PROT"))

  add_user_roles("i2b2", "demouser", "ante", "CHRU", c("USER", "DATA_OBFSC"))
  add_user_roles("i2b2", "demouser", "ante", "620", c("USER", "DATA_AGG"))
  add_user_roles("i2b2", "demouser", "ante", "6070", c("MANAGER", "USER", "DATA_PROT"))

  add_user_roles("i2b2", "demouser", "endoc", "CHRU", c("USER", "DATA_OBFSC"))
  add_user_roles("i2b2", "demouser", "endoc", "620", c("USER", "DATA_AGG"))
  add_user_roles("i2b2", "demouser", "endoc", "6080", c("MANAGER", "USER", "DATA_PROT"))

  add_user_roles("i2b2", "demouser", "post", "CHRU", c("USER", "DATA_OBFSC"))
  add_user_roles("i2b2", "demouser", "post", "620", c("USER", "DATA_AGG"))
  add_user_roles("i2b2", "demouser", "post", "6090", c("MANAGER", "USER", "DATA_PROT"))

  add_user_roles("i2b2", "demouser", "nn", "CHRU", c("USER", "DATA_OBFSC"))
  add_user_roles("i2b2", "demouser", "nn", "620", c("USER", "DATA_AGG"))
  add_user_roles("i2b2", "demouser", "nn", "6100", c("MANAGER", "USER", "DATA_PROT"))
}


pop_obgyn <- function()
{
  UM <- seq(6040, 6100, 10)

  c(UM, 620) %>%
    map(function(project)
        {
          populate_concept(readr::read_csv("../inst/cim.ont"), readr::read_csv("../inst/cim.modi"), "CIM", project)
          populate_concept(readr::read_csv("../inst/ccam.ont"), modi = NULL, "CCAM", project)
          populate_concept(readr::read_csv("../inst/bio.ont"), modi = NULL, "BIO", project)
          populate_provider(readr::read_csv("../inst/struct.ont"), "STRUCT", project)
          populate_concept(readr::read_csv("../inst/hospit.ont"), modi = NULL, "HOS", project)
          populate_concept(readr::read_csv("../inst/patients.ont"), modi = NULL, "PAT", project)
        }
  )

# 2016
  readr::read_csv("/manip/pims16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide", "encounter_ide", "start_date", "end_date", "sex_cd", "birth_date", "death_date", "rum_start", "rum_end", "provider_id", "project")) %>%
    dplyr::filter(project %in% UM) ->
  patients

  patients %>%
    import_patients_visits(620)

  readr::read_csv("/manip/diags16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide", "encounter_ide", "start_date", "end_date", "provider_id", "concept_cd", "modifier_cd")) %>%
    semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
    import_diagnostics(620)

  readr::read_csv("/manip/actes16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide", "encounter_ide", "provider_id", "concept_cd", "start_date")) %>%
    semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
  import_actes(620)

  readr::read_csv("/manip/mensurations16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide", "encounter_ide", "poids", "taille", "IMC")) %>%
    semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
  import_mensurations(patients, 620)

  readr::read_csv("/manip/bio16_1.csv", col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide", "encounter_ide", "start_date", "concept_cd", "nval_num")) %>%
    semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
  import_bios(patients, 620)

  readr::read_csv("/manip/bio16_2.csv", col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide", "encounter_ide", "start_date", "concept_cd", "nval_num")) %>%
    semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
  import_bios(patients, 620)

# 2017
  readr::read_csv("/manip/pims17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide", "encounter_ide", "start_date", "end_date", "sex_cd", "birth_date", "death_date", "rum_start", "rum_end", "provider_id", "project")) %>%
    dplyr::filter(project %in% UM) ->
  patients

  patients %>%
    import_patients_visits(620)

  readr::read_csv("/manip/diags17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide", "encounter_ide", "start_date", "end_date", "provider_id", "concept_cd", "modifier_cd")) %>%
    semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
    import_diagnostics(620)

  readr::read_csv("/manip/actes17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide", "encounter_ide", "provider_id", "concept_cd", "start_date")) %>%
    semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
  import_actes(620)

  readr::read_csv("/manip/mensurations17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide", "encounter_ide", "poids", "taille", "IMC")) %>%
    semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
  import_mensurations(patients, 620)

  readr::read_csv("/manip/bios17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide", "encounter_ide", "start_date", "concept_cd", "nval_num")) %>%
    semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
  import_bios(patients, 620)

  UM %>%
    map(function(x)
        {
          # 2016
          readr::read_csv("/manip/pims16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
            stats::setNames(c("patient_ide", "encounter_ide", "start_date", "end_date", "sex_cd", "birth_date", "death_date", "rum_start", "rum_end", "provider_id", "project")) %>%
            dplyr::filter(project == x) ->
              patients

            patients %>%
              import_patients_visits(x)

            readr::read_csv("/manip/diags16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
              stats::setNames(c("patient_ide", "encounter_ide", "start_date", "end_date", "provider_id", "concept_cd", "modifier_cd")) %>%
              semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
              import_diagnostics(x)

            readr::read_csv("/manip/actes16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
              stats::setNames(c("patient_ide", "encounter_ide", "provider_id", "concept_cd", "start_date")) %>%
              semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
              import_actes(x)

            readr::read_csv("/manip/mensurations16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
              stats::setNames(c("patient_ide", "encounter_ide", "poids", "taille", "IMC")) %>%
              semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
              import_mensurations(patients, x)

            readr::read_csv("/manip/bio16_1.csv", col_types = readr::cols(.default = readr::col_character())) %>%
              stats::setNames(c("patient_ide", "encounter_ide", "start_date", "concept_cd", "nval_num")) %>%
              semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
              import_bios(patients, x)

            readr::read_csv("/manip/bio16_2.csv", col_types = readr::cols(.default = readr::col_character())) %>%
              stats::setNames(c("patient_ide", "encounter_ide", "start_date", "concept_cd", "nval_num")) %>%
              semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
              import_bios(patients, x)

            # 2017
            readr::read_csv("/manip/pims17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
              stats::setNames(c("patient_ide", "encounter_ide", "start_date", "end_date", "sex_cd", "birth_date", "death_date", "rum_start", "rum_end", "provider_id", "project")) %>%
              dplyr::filter(project == x) ->
                patients

              patients %>%
                import_patients_visits(x)

              readr::read_csv("/manip/diags17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
                stats::setNames(c("patient_ide", "encounter_ide", "start_date", "end_date", "provider_id", "concept_cd", "modifier_cd")) %>%
                semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
                import_diagnostics(x)

              readr::read_csv("/manip/actes17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
                stats::setNames(c("patient_ide", "encounter_ide", "provider_id", "concept_cd", "start_date")) %>%
                semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
                import_actes(x)

              readr::read_csv("/manip/mensurations17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
                stats::setNames(c("patient_ide", "encounter_ide", "poids", "taille", "IMC")) %>%
                semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
                import_mensurations(patients, x)

              readr::read_csv("/manip/bios17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
                stats::setNames(c("patient_ide", "encounter_ide", "start_date", "concept_cd", "nval_num")) %>%
                semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
                import_bios(patients, x)

        })
}


pop_chru <- function()
{
  readr::read_csv("/manip/pims16.csv", col_types = readr::cols(.default = readr::col_character())) -> patients

  patients %>%
  import_patients_visits("CHRU")

  readr::read_csv("/manip/diags16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_diagnostics("CHRU")

  readr::read_csv("/manip/actes16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_actes("CHRU")

  readr::read_csv("/manip/mensurations16.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_mensurations(patients, "CHRU")

  readr::read_csv("/manip/bio16_1.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_bios(patients, "CHRU")

  readr::read_csv("/manip/bio16_2.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_bios(patients, "CHRU")

  readr::read_csv("/manip/pims17.csv", col_types = readr::cols(.default = readr::col_character())) -> patients

  patients %>%
  import_patients_visits("CHRU")

  readr::read_csv("/manip/diags17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_diagnostics("CHRU")

  readr::read_csv("/manip/actes17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_actes("CHRU")

  readr::read_csv("/manip/mensurations17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_mensurations(patients, "CHRU")

  readr::read_csv("/manip/bios17.csv", col_types = readr::cols(.default = readr::col_character())) %>%
  import_bios(patients, "CHRU")
}

