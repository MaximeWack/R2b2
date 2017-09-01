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

  add_project("CHRU", "CHRU - Tous services")

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
  populate_ont(readr::read_csv("../inst/struct.ont"), modi = NULL, "STRUCT", def_facttablecolumn = "provider_id", def_tablename = "provider_dimension", def_columnname = "provider_path")
  populate_ont(readr::read_csv("../inst/bio.ont"), modi = NULL, "BIO", include_code = F)

  # Populate the concept/provider tables needed
  add_ontologies("CHRU")

  # Restart wildfly
  service("jboss", "restart")
}

accounts_obgyn <- function()
{
  "CHRU" -> main
  620 -> top_project
  seq(6040, 6100, 10) -> projects

  "maxx" -> admin

  "obgyn" -> top_user
  "Gynécologie-Obstétrique" -> top_name

  c("amp", "cancero", "ortho", "ante", "endoc", "post", "nn") -> users
  c("AMP Clinique", "Gynécologie et Cancérologie", "Orthogénie", "Anténatal", "Endocrinologie Maternité", "Post-natal", "Nouveaux-nés") -> names

  "med_amp" -> bottom_user
  "Médecin AMP" -> bottom_name

## Admin
# MANAGER on main
  admin %>%
    purrr::map(~add_user_roles("i2b2", "demouser", .x, main,  c("MANAGER", "USER", "DATA_PROT")))

# MANAGER on top_project
  admin %>%
    purrr::map(~add_user_roles("i2b2", "demouser", .x, top_project, c("MANAGER", "USER", "DATA_PROT")))

# MANAGER on projects
  admin %>%
    purrr::map2(projects, ~add_user_roles("i2b2", "demouser", .x, .y, c("MANAGER", "USER", "DATA_PROT")))


## Top user
# Add top user
  top_user %>%
    purrr::map2(top_name, ~add_user("i2b2", "demouser", .x, .y, "", .x))

# DATA_AGG on main
  top_user %>%
    purrr::map(~add_user_roles("i2b2", "demouser", .x, main, c("USER", "DATA_AGG")))

# MANAGER on top_project
  top_user %>%
    purrr::map(~add_user_roles("i2b2", "demouser", .x, top_project, c("MANAGER", "USER", "DATA_PROT")))

# MANAGER on projects
  top_user %>%
    purrr::map2(projects, ~add_user_roles("i2b2", "demouser", .x, .y, c("MANAGER", "USER", "DATA_PROT")))


## Users
# Add users
  users %>%
  purrr::map2(names, ~add_user("i2b2", "demouser", .x, .y, "", .x))

# DATA_OBFSC on main
  users %>%
    purrr::map(~add_user_roles("i2b2", "demouser", .x, main, c("USER", "DATA_OBFSC")))

# DATA_AGG on top_project
  users %>%
    purrr::map(~add_user_roles("i2b2", "demouser", .x, top_project, c("USER", "DATA_AGG")))

# MANAGER on projects
  users %>%
    purrr::map2(projects, ~add_user_roles("i2b2", "demouser", .x, .y, c("MANAGER", "USER", "DATA_PROT")))


## Bottom users
# Add user
  bottom_user %>%
    purrr::map2(bottom_name, ~add_user("i2b2", "demouser", .x, .y, "", .x))

# DATA_OBFSC on main
  bottom_user %>%
    purrr::map(~add_user_roles("i2b2", "demouser", .x, main, c("USER", "DATA_OBFSC")))

# DATA_OBFSC on top_project
  bottom_user %>%
    purrr::map(~add_user_roles("i2b2", "demouser", .x, top_project, c("USER", "DATA_OBFSC")))

# DATA_AGG on project
  bottom_user %>%
    purrr::map2("amp", ~add_user_roles("i2b2", "demouser", .x, .y, c("USER", "DATA_AGG")))
}

#pop_project(620, seq(6040, 6100, 10), "/manip/pims16.csv", "/manip/diags16.csv", "/manip/actes16.csv", "/manip/mensurations16.csv", "/manip/bios16.csv")
#pop_project(620, seq(6040, 6100, 10), "/manip/pims17.csv", "/manip/diags17.csv", "/manip/actes17.csv", "/manip/mensurations17.csv", "/manip/bios17.csv")

pop_projects <- function(top_project, projects, patients_file, diagnostics_file, actes_file, mensurations_file, bios_file)
{
  c(top_project, projects) %>%
    purrr::map(add_ontologies)

  read_patients(patients_file) %>%
    dplyr::filter(project %in% projects) ->
  patients

  patients %>%
    import_patients_visits(top_project)

  read_diagnostics(diagnostics_file) %>%
    dplyr::semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
    import_diagnostics(top_project)

  read_actes(actes_file) %>%
    dplyr::semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
  import_actes(top_project)

  read_mensurations(mensurations_file) %>%
    dplyr::semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
  import_mensurations(patients, top_project)

  read_bios(bios_file, n_max = 5e6) %>%
    dplyr::semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
  import_bios(patients, top_project)

  read_bios(bios_file, skip = 5e6) %>%
    dplyr::semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
  import_bios(patients, top_project)

  projects %>%
    purrr::map(function(x)
        {
          read_patients(patients_file) %>%
            dplyr::filter(project == x) ->
              patients

            patients %>%
              import_patients_visits(x)

            read_diagnostics(diagnostics_file) %>%
              dplyr::semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
              import_diagnostics(x)

            read_actes(actes_file) %>%
              dplyr::semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
              import_actes(x)

            read_mensurations(mensurations_file) %>%
              dplyr::semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
              import_mensurations(patients, x)

            read_bios(bios_file, n_max = 5e6) %>%
              dplyr::semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
              import_bios(patients, x)

            read_bios(bios_file, skip = 5e6) %>%
              dplyr::semi_join(patients, by = c("patient_ide", "encounter_ide")) %>%
              import_bios(patients, x)
        })
}

#pop_chru("CHRU", "/manip/pims16.csv", "/manip/diags16.csv", "/manip/actes16.csv", "/manip/mensurations16.csv", "/manip/bios16.csv")
#pop_chru("CHRU", "/manip/pims17.csv", "/manip/diags17.csv", "/manip/actes17.csv", "/manip/mensurations17.csv", "/manip/bios17.csv")

pop_main <- function(main, patients_file, diagnostics_file, actes_file, mensurations_file, bios_file)
{
  read_patients(patients_file) -> patients

  patients %>%
  import_patients_visits(main)

  read_diagnostics(diagnostics_file) %>%
  add_observations(main)

  read_actes(actes_file) %>%
  add_observations(main)

  read_mensurations(mensurations_file) %>%
  import_mensurations(patients, main)

  read_bios(bios_file, n_max = 5e6) %>%
  import_bios(patients, main)

  read_bios(bios_file, skip = 5e6) %>%
  import_bios(patients, main)
}

read_patients <- function(file)
{
  readr::read_csv(file, col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide",
                      "encounter_ide",
                      "start_date",
                      "end_date",
                      "sex_cd",
                      "birth_date",
                      "death_date",
                      "rum_start",
                      "rum_end",
                      "provider_id",
                      "project")) %>%
    dplyr::mutate(patient_ide   = sanitize_patient(patient_ide),
                  encounter_ide = sanitize_encounter(encounter_ide, start_date),
                  start_date    = start_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
                  end_date      = end_date   %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
                  sex_cd        = ifelse(sex_cd == "1", "M", "F"),
                  birth_date    = birth_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
                  death_date    = death_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
                  rum_start     = rum_start  %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
                  rum_end       = rum_end    %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
                  provider_id   = stringr::str_c("STRUCT:", provider_id))
}

read_diagnostics <- function(file)
{
  readr::read_csv(file, col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide",
                      "encounter_ide",
                      "enc_start_date",
                      "start_date",
                      "end_date",
                      "provider_id",
                      "concept_cd",
                      "modifier_cd")) %>%
    dplyr::mutate(encounter_ide = sanitize_encounter(encounter_ide, enc_start_date),
                  patient_ide   = sanitize_patient(patient_ide),
                  start_date    = start_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
                  end_date      = end_date   %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
                  provider_id   = stringr::str_c("STRUCT:", provider_id),
                  concept_cd    = stringr::str_c("CIM:", concept_cd),
                  modifier_cd   = stringr::str_c("CIM:", modifier_cd)) %>%
    dplyr::select(-enc_start_date)
}

read_actes <- function(file)
{
  readr::read_csv(file, col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide",
                      "encounter_ide",
                      "enc_start_date",
                      "provider_id",
                      "concept_cd",
                      "start_date")) %>%
  dplyr::mutate(encounter_ide = sanitize_encounter(encounter_ide, enc_start_date),
                patient_ide   = sanitize_patient(patient_ide),
                provider_id   = stringr::str_c("STRUCT:", provider_id),
                concept_cd    = stringr::str_c("CCAM:", concept_cd),
                modifier_cd   = "@",
                start_date    = start_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S")) %>%
  dplyr::select(-enc_start_date)
}

read_mensurations <- function(file)
{
  readr::read_csv(file, col_types = readr::cols(.default = readr::col_character())) %>%
    stats::setNames(c("patient_ide",
                      "encounter_ide",
                      "enc_start_date",
                      "poids",
                      "taille",
                      "IMC")) %>%
    dplyr::mutate(patient_ide = sanitize_patient(patient_ide),
                  encounter_ide = sanitize_encounter(encounter_ide, enc_start_date)) %>%
    dplyr::select(-enc_start_date) %>%
    tidyr::gather(concept_cd, nval_num, poids, taille, IMC) %>%
    dplyr::filter(!is.na(nval_num)) %>%
    dplyr::mutate(concept_cd  = stringr::str_c("HOS:", concept_cd),
                  modifier_cd = "@",
                  valtype_cd  = "N",
                  tval_char   = "E",
                  nval_num    = nval_num %>% stringr::str_replace(",", "."))
}

read_bios <- function(file, ...)
{
  readr::read_csv("../inst/bio.map") -> mapping

  readr::read_csv(file, col_types = readr::cols(.default = readr::col_character()), ...) %>%
    stats::setNames(c("patient_ide",
                      "encounter_ide",
                      "enc_start_date",
                      "start_date",
                      "concept_cd",
                      "nval_num")) %>%
    dplyr::left_join(mapping, by = c("concept_cd" = "from")) %>%
    dplyr::mutate(encounter_ide = sanitize_encounter(encounter_ide, enc_start_date),
                  patient_ide   = sanitize_patient(patient_ide),
                  start_date    = start_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
                  modifier_cd   = "@",
                  concept_cd    = ifelse(!is.na(to), to, concept_cd),
                  concept_cd    = stringr::str_c("BIO:", concept_cd)) %>%
    dplyr::select(-to, -enc_start_date)
}

sanitize_encounter <- function(encounter_ide, start_date)
{
  start_date <- start_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S")

  ifelse(encounter_ide %>% stringr::str_detect("\\."),
         stringr::str_c(encounter_ide, lubridate::day(start_date) %>% stringr::str_pad(2, "left", "0")),
         encounter_ide)
}

sanitize_patient <- function(patient_ide)
{
  ifelse(patient_ide %>% as.numeric > 2^32,
         patient_ide %>% stringr::str_sub(2),
         patient_ide)
}

add_ontologies <- function(project)
{
  populate_concept(readr::read_csv("../inst/cim.ont")     , readr::read_csv("../inst/cim.modi"), "CIM" , project)
  populate_concept(readr::read_csv("../inst/ccam.ont")    , modi = NULL                        , "CCAM", project)
  populate_concept(readr::read_csv("../inst/bio.ont")     , modi = NULL                        , "BIO" , project)
  populate_provider(readr::read_csv("../inst/struct.ont") , "STRUCT"                           , project)
  populate_concept(readr::read_csv("../inst/hospit.ont")  , modi = NULL                        , "HOS" , project)
  populate_concept(readr::read_csv("../inst/patients.ont"), modi = NULL                        , "PAT" , project)
}
