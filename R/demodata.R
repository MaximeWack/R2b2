#' Clear the default demodata tables
#'
#' Clear the default demodata tables
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_default_demodata <- function(host = "127.0.0.1", admin, pass)
{
  clear_concept(host, admin, pass)

  clear_modifier(host, admin, pass)

  clear_code_lookup(host, admin, pass)

  clear_encounter_mapping(host, admin, pass)

  clear_encounter_dimension(host, admin, pass)

  clear_patients(host, admin, pass)

  clear_breakdown(host, admin, pass)

  clear_providers(host, admin, pass)

  clear_observations(host, admin, pass)

  clear_patient_mapping(host, admin, pass)
}

#' Clear the default code_lookup
#'
#' Clear the default codes in code_lookup
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_code_lookup <- function(host = "127.0.0.1", admin, pass)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "DELETE FROM code_lookup;")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Clear the default encounter mappings
#'
#' Clear the default mappings in encounter_mapping
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_encounter_mapping <- function(host = "127.0.0.1", admin, pass)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "DELETE FROM encounter_mapping;")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Clear the default observation facts
#'
#' Clear the default facts in observation_fact
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_observations <- function(host = "127.0.0.1", admin, pass)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "DELETE FROM observation_fact;")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Clear the default patients
#'
#' Clear the default patients in patient_dimension
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_patients <- function(host = "127.0.0.1", admin, pass)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "DELETE FROM patient_dimension;")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Clear the default patient mappings
#'
#' Clear the default mappings in patient_mapping
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_patient_mapping <- function(host = "127.0.0.1", admin, pass)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "DELETE FROM patient_mapping;")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Clear the default providers
#'
#' Clear the default providers in provider_dimension
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_providers <- function(host = "127.0.0.1", admin, pass)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "DELETE FROM provider_dimension;")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Clear the default qt_breakdown_path
#'
#' Clear the default paths in qt_breakdown_path
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_breakdown <- function(host = "127.0.0.1", admin, pass)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "DELETE FROM qt_breakdown_path;")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Clear the default encounters
#'
#' Clear the default encounters in visit_dimension
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_encounter_dimension <- function(host = "127.0.0.1", admin, pass)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "DELETE FROM visit_dimension;")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Clear the default modifiers
#'
#' Clear the default concepts in concept_dimension
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_modifier <- function(host = "127.0.0.1", admin, pass)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "DELETE FROM modifier_dimension;")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Clear the default concepts
#'
#' Clear the default concepts in concept_dimension
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_concept <- function(host = "127.0.0.1", admin, pass)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "DELETE FROM concept_dimension;")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Delete modifiers
#'
#' Delete modifiers from modifier_dimension
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param scheme The scheme to delete from the concepts
#' @export
delete_modifier <- function(host = "127.0.0.1", admin, pass, scheme)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, stringr::str_c("DELETE FROM modifier_dimension WHERE (modifier_cd LIKE '", scheme, ":%');"))

  RPostgreSQL::dbDisconnect(demodata)
}

#' Delete concepts
#'
#' Delete concepts from concept_dimension
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param scheme The scheme to delete from the concepts
#' @export
delete_concept <- function(host = "127.0.0.1", admin, pass, scheme)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, stringr::str_c("DELETE FROM concept_dimension WHERE (concept_cd LIKE '", scheme, ":%');"))

  RPostgreSQL::dbDisconnect(demodata)
}

#' Populate the concept_dimension
#'
#' Populate the concept_dimension with new concepts
#'
#' ont is a character vector containing all the leaves of the ontology
#' with their respective path, in the form
#' code_level1 label_level1/code_level2 label_level2/.../code_leaf label_leaf
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param ont The ontology to insert
#' @param modi The modifiers to insert
#' @param name The name of the new ontology
#' @param scheme The scheme to use for this ontology
#' @export
populate_concept <- function(host = "127.0.0.1", admin, pass, ont, modi, name, scheme)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  # Sanitize the ontology
  ont <- ont %>% stringr::str_replace_all("'", "''")
  modi <- modi %>% stringr::str_replace_all("'", "''")

  # Create the data frame holding the contents of the new table
  data.frame(concept_path = ont, stringsAsFactors = F) %>%
    # Insert the name of the ontology at the root
    dplyr::mutate(concept_path = stringr::str_c("\\", name, "\\", concept_path)) %>%
    # Populate the other columns
    dplyr::mutate(name_char = stringr::str_extract(concept_path, "[^\\\\]+$"),
                  concept_cd = stringr::str_c(scheme, ":", name_char %>% stringr::str_extract("^.+? ") %>% stringr::str_trim()),
                  concept_cd = ifelse(is.na(concept_cd), "", concept_cd),
                  concept_path = stringr::str_c(concept_path, "\\"),
                  # Use only codes to build shorter paths
                  concept_path = stringr::str_replace_all(concept_path, "\\\\(.+?) [^\\\\]+", "\\\\\\1"),
                  update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    # Push the dataframe into the new ontology table
    dbPush(con = demodata, table = "concept_dimension", .)

  if (length(modi) > 0)
  {
    data.frame(modi = modi) %>%
      dplyr::mutate(name_char = modi %>% stringr::str_extract(" .*$") %>% stringr::str_trim(),
                    modifier_path = stringr::str_c("\\", name_char, "\\"),
                    modifier_cd = stringr::str_c(scheme, ":", modi %>% stringr::str_extract("^.+? ") %>% stringr::str_trim()),
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    dplyr::select(-modi) %>%
    # Push the dataframe into the new ontology table
    dbPush(con = demodata, table = "concept_dimension", .)
  }

  RPostgreSQL::dbDisconnect(demodata)
}

#' Populate the provider_dimension
#'
#' Populate the provider_dimension with new providers
#'
#' ont is a character vector containing all the leaves of the ontology
#' with their respective path, in the form
#' code_level1 label_level1/code_level2 label_level2/.../code_leaf label_leaf
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param ont The ontology to insert
#' @param name The name of the new ontology
#' @param scheme The scheme to use for this ontology
#' @export
populate_provider <- function(host = "127.0.0.1", admin, pass, ont, name, scheme)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  # Sanitize the ontology
  ont <- ont %>% stringr::str_replace_all("'", "''")

  # Create the data frame holding the contents of the new table
  data.frame(provider_path = ont, stringsAsFactors = F) %>%
    # Insert the name of the ontology at the root
    dplyr::mutate(provider_path = stringr::str_c("\\", name, "\\", concept_path)) %>%
    # Populate the other columns
    dplyr::mutate(name_char = stringr::str_extract(provider_path, "[^\\\\]+$"),
                  provider_id = stringr::str_c(scheme, ":", name_char %>% stringr::str_extract("^.+? ") %>% stringr::str_trim()),
                  provider_id = ifelse(is.na(provider_id), "", provider_id),
                  provider_path = stringr::str_c(provider_path, "\\"),
                  # Use only codes to build shorter paths
                  provider_path = stringr::str_replace_all(provider_path, "\\\\(.+?) [^\\\\]+", "\\\\\\1"),
                  update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    # Push the dataframe into the new ontology table
    dbPush(con = demodata, table = "provider_dimension", .)

  RPostgreSQL::dbDisconnect(demodata)
}

#' Add patients to the CRC cell
#'
#' Add patients to the CRC cell, generate new encrypted IDs,
#'
#' The patients dataframe must contain the following columns:
#' - patient_ide: the original patient ID
#' - birthdate: as a Date object
#' - deathdate
#' - gender (F or M)
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the admin account
#' @param patients A dataframe of patients
#' @param project The project to add the patients to
#' @return A patient mapping dataframe for the patients
#' @export
add_patients_demodata <- function(host, admin, pass, patients, project)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

# Get the existing patient mappings
  dplyr::src_postgres("i2b2demodata", host, user = admin, password = pass) %>%
    dplyr::tbl("patient_mapping") %>%
    dplyr::select(patient_ide,patient_ide_source, patient_num) %>%
    dplyr::collect(n = Inf) -> existing

# Create the new patient mappings
  new_id_start <- ifelse(nrow(existing) == 0, 100000001, existing$patient_num %>% as.numeric %>% max + 1)

  data.frame(patient_ide = as.character(patients$patient_ide), stringsAsFactors = F) %>%
    dplyr::anti_join(existing) %>%
    dplyr::mutate(patient_num = seq(new_id_start, length.out = nrow(.))) -> new_patients

  # Push the new patient mappings
  if (nrow(new_patients) > 0)
  {
    new_patients %>%
      dplyr::mutate(patient_ide_source = project,
                    patient_num = as.character(patient_num),
                    patient_ide_status  = "A",
                    project_id = project,
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    dbPush(con = demodata, table = "patient_mapping", .)
  }

  # Push the new patient mappings for HIVE
  if (nrow(new_patients) > 0)
  {
    new_patients %>%
      dplyr::mutate(patient_ide_source = "HIVE",
                    patient_num = as.character(patient_num),
                    patient_ide = as.character(patient_num),
                    patient_ide_status  = "A",
                    project_id = project,
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    dbPush(con = demodata, table = "patient_mapping", .)
  }

# Push the new patients in patient_dimension
  patients %>%
    dplyr::mutate(patient_ide = patient_ide %>% as.character) %>%
    dplyr::right_join(new_patients) %>%
    dplyr::mutate(birth_date = ifelse(is.na(birthdate), "", format(birthdate, format = "%m/%d/%Y %H:%M:%S")),
           death_date = ifelse(is.na(deathdate), NA, format(deathdate, format = "%m/%d/%Y %H:%M:%S")),
           vital_status_cd = ifelse(is.na(deathdate), "", "S"),
           sex_cd = gender,
           age_in_years_num = ifelse(is.na(deathdate), floor(as.numeric(Sys.Date() - birthdate)/365.25), floor(as.numeric(deathdate - birthdate)/365.25)),
           patient_num = as.character(patient_num),
           update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    dplyr::select(-patient_ide, -birthdate, -deathdate, -gender) %>%
    dbPush(con = demodata, table = "patient_dimension", .)

# Update the existing patients in patient_dimension
  patients %>%
    dplyr::mutate(patient_ide = patient_ide %>% as.character) %>%
    dplyr::inner_join(existing) %>%
    dplyr::mutate(birth_date = ifelse(is.na(birthdate), "", format(birthdate, format = "%m/%d/%Y %H:%M:%S")),
           death_date = ifelse(is.na(deathdate), NA, format(deathdate, format = "%m/%d/%Y %H:%M:%S")),
           vital_status_cd = ifelse(is.na(deathdate), "", "S"),
           sex_cd = gender,
           age_in_years_num = ifelse(is.na(deathdate), floor(as.numeric(Sys.Date() - birthdate)/365.25), floor(as.numeric(deathdate - birthdate)/365.25)),
           patient_num = as.character(patient_num),
           update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    dplyr::select(-patient_ide, -birthdate, -deathdate, -gender, -patient_ide_source) %>%
    dbUpdate(con = demodata, table = "patient_dimension", ., "patient_num")

  RPostgreSQL::dbDisconnect(demodata)

  patients %>%
    inner_join(new_patients, by = "patient_ide") %>%
    select(patient_ide, patient_num) %>%
    bind_rows(patients %>%
              inner_join(existing, by = "patient_ide") %>%
              select(patient_ide, patient_num)) %>%
    mutate(patient_num = patient_num %>% as.character) %>%
    distinct
}

add_encounters <- function(host, admin, pass, encounters, project)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  dplyr::src_postgres("i2b2demodata", host, user = admin, password = pass) %>%
    dplyr::tbl("encounter_mapping") %>%
    dplyr::select(encounter_ide, encounter_num, patient_ide) %>%
    dplyr::collect(n = Inf) -> existing

  new_id_start <- ifelse(nrow(existing) == 0, 100000001, existing$encounter_num %>% as.numeric %>% max + 1)

  encounters %>%
    dplyr::mutate(encounter_ide = encounter_ide %>% as.character) %>%
    dplyr::anti_join(existing, by = "encounter_ide") %>%
    dplyr::mutate(encounter_num = seq(new_id_start, length.out = nrow(.))) %>%
    dplyr::left_join(patient_mapping) %>%
    dplyr::select(-patient_ide, -startdate, -enddate, -inout) -> new_encounters

                  encounter_ide_status  = "A",
                  project_id = project,
                  update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
  dbPush(con = demodata, table = "encounter_mapping", .)

  new_patients %>%
    dplyr::mutate(encounter_ide_source = "HIVE",
                  encounter_num = as.character(encounter_num),
                  encounter_ide = as.character(encounter_num),
                  encounter_ide_status  = "A",
                  project_id = project,
                  update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
  dbPush(con = demodata, table = "encounter_mapping", .)

  RPostgreSQL::dbDisconnect(demodata)
}
