#' Clear the default demodata tables
#'
#' Clear the default demodata tables
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_default_demodata <- function(host = "", admin = "", pass = "")
{
  c("code_lookup", "concept_dimension", "modifier_dimension", "encounter_mapping", "visit_dimension", "patient_dimension", "patient_mapping", "qt_breakdown_path", "provider_dimension", "observation_fact") %>%
    purrr::walk(~clear_table("i2b2demodata", .x, host, admin, pass))
}

#' Delete modifiers
#'
#' Delete modifiers from modifier_dimension
#'
#' @param scheme The scheme to delete from the concepts
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
delete_modifier <- function(scheme, host = "", admin = "", pass = "")
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, stringr::str_c("DELETE FROM modifier_dimension WHERE (modifier_cd LIKE '", scheme, ":%');"))

  RPostgreSQL::dbDisconnect(demodata)
}

list_modifier <- function(scheme, host = "", admin = "", pass = "")
{
  dplyr::src_postgres("i2b2demodata", host = host, user = admin, pass = pass) %>%
    dplyr::tbl("modifier_dimension") %>%
    dplyr::collect(n = Inf) %>%
    dplyr::filter(modifier_cd %>% stringr::str_detect(stringr::str_c(scheme, ":.*")))
}

#' Delete concepts
#'
#' Delete concepts from concept_dimension
#'
#' @param scheme The scheme to delete from the concepts
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
delete_concept <- function(scheme, host = "", admin = "", pass = "")
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, stringr::str_c("DELETE FROM concept_dimension WHERE (concept_cd LIKE '", scheme, ":%');"))

  RPostgreSQL::dbDisconnect(demodata)
}

list_concepts <- function(scheme, host = "", admin = "", pass = "")
{
  dplyr::src_postgres("i2b2demodata", host = host, user = admin, pass = pass) %>%
    dplyr::tbl("concept_dimension") %>%
    dplyr::collect(n = Inf) %>%
    dplyr::filter(concept_cd %>% stringr::str_detect(stringr::str_c(scheme, ":.*")))
}

#' Populate the concept_dimension
#'
#' Populate the concept_dimension with new concepts
#'
#' ont is a character vector containing all the leaves of the ontology
#' with their respective path, in the form
#' code_level1 label_level1/code_level2 label_level2/.../code_leaf label_leaf
#'
#' @param ont The ontology to insert
#' @param modi The modifiers to insert
#' @param name The name of the new ontology
#' @param scheme The scheme to use for this ontology
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
populate_concept <- function(ont, modi, name, scheme, host = "", admin = "", pass = "")
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
    dbPush(demodata, "concept_dimension")

  if (length(modi) > 0)
  {
    data.frame(modi = modi) %>%
      dplyr::mutate(name_char = modi %>% stringr::str_extract(" .*$") %>% stringr::str_trim(),
                    modifier_path = stringr::str_c("\\", name_char, "\\"),
                    modifier_cd = stringr::str_c(scheme, ":", modi %>% stringr::str_extract("^.+? ") %>% stringr::str_trim()),
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    dplyr::select(-modi) %>%
    # Push the dataframe into the new ontology table
    dbPush(demodata, "modifier_dimension")
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
#' @param ont The ontology to insert
#' @param name The name of the new ontology
#' @param scheme The scheme to use for this ontology
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
populate_provider <- function(ont, name, scheme, host = "", admin = "", pass = "")
{
  options(scipen = 999)

  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  # Sanitize the ontology
  ont <- ont %>% stringr::str_replace_all("'", "''")

  # Create the data frame holding the contents of the new table
  data.frame(provider_path = ont, stringsAsFactors = F) %>%
    # Insert the name of the ontology at the root
    dplyr::mutate(provider_path = stringr::str_c("\\", name, "\\", provider_path)) %>%
    # Populate the other columns
    dplyr::mutate(name_char = stringr::str_extract(provider_path, "[^\\\\]+$"),
                  provider_id = stringr::str_c(scheme, ":", name_char %>% stringr::str_extract("^.+? ") %>% stringr::str_trim()),
                  provider_id = ifelse(is.na(provider_id), "", provider_id),
                  provider_path = stringr::str_c(provider_path, "\\"),
                  # Use only codes to build shorter paths
                  provider_path = stringr::str_replace_all(provider_path, "\\\\(.+?) [^\\\\]+", "\\\\\\1"),
                  update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    # Push the dataframe into the new ontology table
    dbPush(demodata, "provider_dimension")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Add patients to the CRC cell
#'
#' Add patients to the CRC cell, generate new encrypted IDs,
#'
#' The patients dataframe must contain the following columns:
#' - patient_ide: the original patient ID
#' - birth_date: as a Date object
#' - death_date: as a Date object
#' - gender (F or M)
#'
#' @param patients A dataframe of patients
#' @param project The project to add the patients to
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the admin account
#' @return A patient mapping dataframe for the patients
#' @export
add_patients_demodata <- function(patients, project, host = "", admin = "", pass = "")
{
  options(scipen = 999)

  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

# Get the existing patient mappings
  dplyr::src_postgres("i2b2demodata", host, user = admin, password = pass) %>%
    dplyr::tbl("patient_mapping") %>%
    dplyr::select(patient_ide,patient_ide_source, patient_num) %>%
    dplyr::collect(n = Inf) -> existing

  if (nrow(existing) == 0)
    existing <- data.frame(patient_ide = character(0), patient_num = character(0))

# Create the new patient mappings
  new_id_start <- ifelse(nrow(existing) == 0, 100000001, existing$patient_num %>% as.numeric %>% max + 1)

  data.frame(patient_ide = as.character(patients$patient_ide), stringsAsFactors = F) %>%
    dplyr::anti_join(existing) %>%
    dplyr::mutate(patient_num = seq(new_id_start, length.out = nrow(.))) -> new_patients

  if (nrow(new_patients) == 0)
    new_patients = data.frame(patient_ide = character(0), patient_num = character(0))

  # Push the new patient mappings
  if (nrow(new_patients) > 0)
  {
    new_patients %>%
      dplyr::mutate(patient_ide_source = project,
                    patient_num = as.character(patient_num),
                    patient_ide_status  = "A",
                    project_id = project,
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    dbPush(demodata, "patient_mapping")
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
    dbPush(demodata, "patient_mapping")
  }

  # Push the new patients in patient_dimension
  if (nrow(new_patients) > 0)
  {
    patients %>%
      dplyr::mutate(patient_ide = patient_ide %>% as.character) %>%
      dplyr::right_join(new_patients) %>%
      dplyr::mutate(age_in_years_num = ifelse(is.na(death_date), floor(as.numeric(Sys.Date() - birth_date)/365.25), floor(as.numeric(death_date - birth_date)/365.25)),
                    birth_date = ifelse(is.na(birth_date), "", format(birth_date, format = "%m/%d/%Y %H:%M:%S")),
                    death_date = ifelse(is.na(death_date), NA, format(death_date, format = "%m/%d/%Y %H:%M:%S")),
                    vital_status_cd = ifelse(is.na(death_date), "", "S"),
                    sex_cd = gender,
                    patient_num = as.character(patient_num),
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
      dplyr::select(-patient_ide, -gender) %>%
      dbPush(demodata, "patient_dimension")
  }

  # Update the existing patients in patient_dimension
  if (nrow(existing) > 0)
  {
    patients %>%
      dplyr::mutate(patient_ide = patient_ide %>% as.character) %>%
      dplyr::inner_join(existing) %>%
      dplyr::mutate(age_in_years_num = ifelse(is.na(death_date), floor(as.numeric(Sys.Date() - birth_date)/365.25), floor(as.numeric(death_date - birth_date)/365.25)),
                    birth_date = ifelse(is.na(birth_date), "", format(birth_date, format = "%m/%d/%Y %H:%M:%S")),
                    death_date = ifelse(is.na(death_date), NA, format(death_date, format = "%m/%d/%Y %H:%M:%S")),
                    vital_status_cd = ifelse(is.na(death_date), "", "S"),
                    sex_cd = gender,
                    patient_num = as.character(patient_num),
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
      dplyr::select(-patient_ide, -gender, -patient_ide_source) %>%
      dbUpdate(demodata, "patient_dimension", "patient_num")
  }

  RPostgreSQL::dbDisconnect(demodata)


  patients %>%
    dplyr::select(patient_ide) %>%
    dplyr::inner_join(new_patients, by = "patient_ide") %>%
    dplyr::bind_rows(patients %>%
              dplyr::inner_join(existing, by = "patient_ide")) %>%
    dplyr::select(patient_ide, patient_num) %>%
    dplyr::distinct() %>%
    purrr::map(as.character) %>%
    data.frame(check.names = F, stringsAsFactors = F)
}

#' Add encounters to the CRC cell
#'
#' Add encounters to the CRC cell, generate new encrypted IDs
#'
#' The encounters dataframe must contain the following columns:
#' - encounter_ide: the original encounter ID
#' - patient_ide: the original patient ID
#' - start_date: the start date of the encounter, as Date object
#' - end_date: the end date of the encounter, as Date object
#' - inout: I or O if inpatient or outpatient
#'
#' @param encounters A dataframe of patients
#' @param project The project to add the patients to
#' @param patient_mapping The patient mapping table
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the admin account
#' @return An encounter mapping dataframe for the encounters
#' @export
add_encounters <- function(encounters, project, patient_mapping, host = "", admin = "", pass = "")
{
  options(scipen = 999)

  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

# Get existing encounters
  dplyr::src_postgres("i2b2demodata", host, user = admin, password = pass) %>%
    dplyr::tbl("encounter_mapping") %>%
    dplyr::select(encounter_ide, encounter_num, patient_ide) %>%
    dplyr::collect(n = Inf) -> existing

# Create the new encounter mappings
  new_id_start <- ifelse(nrow(existing) == 0, 100000001, existing$encounter_num %>% as.numeric %>% max + 1)

  if (nrow(existing) == 0)
    existing = data.frame(encounter_ide = character(0), encounter_num = character(0), patient_num = character(0))

  encounters %>%
    dplyr::mutate(encounter_ide = encounter_ide %>% as.character) %>%
    dplyr::anti_join(existing, by = "encounter_ide") %>%
    dplyr::mutate(encounter_num = seq(new_id_start, length.out = nrow(.))) %>%
    dplyr::left_join(patient_mapping) %>%
    dplyr::select(-patient_ide, -start_date, -end_date, -inout) -> new_encounters

  if (nrow(new_encounters) == 0)
    new_encounters = data.frame(encounter_ide = character(0), ecnounter_num = character(0), patient_num = character(0))

  # Push the new encounter mappings
  if (nrow(new_encounters) > 0)
  {
    new_encounters %>%
      dplyr::mutate(encounter_ide_source = project,
                    encounter_ide_status  = "A",
                    project_id = project,
                    patient_ide_source = "HIVE",
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
      dplyr::rename(patient_ide = patient_num) %>%
      purrr::map(as.character) %>%
      data.frame(check.names = F, stringsAsFactors = F) %>%
    dbPush(demodata, "encounter_mapping")
  }

  # Push the new encounter mappings
  if (nrow(new_encounters) > 0)
  {
    new_encounters %>%
      dplyr::mutate(encounter_ide_source = "HIVE",
                    encounter_ide_status  = "A",
                    project_id = project,
                    patient_ide_source = "HIVE",
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
      dplyr::rename(patient_ide = patient_num) %>%
      purrr::map(as.character) %>%
      data.frame(check.names = F, stringsAsFactors = F) %>%
    dbPush(demodata, "encounter_mapping")
  }

  # Push the new encounters in visit_dimension
  if (nrow(new_encounters) > 0)
  {
    encounters %>%
      dplyr::mutate(encounter_ide = encounter_ide %>% as.character) %>%
      dplyr::right_join(new_encounters) %>%
      dplyr::mutate(length_of_stay = ifelse(is.na(end_date), floor(as.numeric(Sys.Date() - start_date)), floor(as.numeric(end_date - start_date))),
                    start_date = ifelse(is.na(start_date), "", format(start_date, format = "%m/%d/%Y %H:%M:%S")),
                    end_date = ifelse(is.na(end_date), NA, format(end_date, format = "%m/%d/%Y %H:%M:%S")),
                    active_status_cd = ifelse(is.na(end_date), "O", ""),
                    inout_cd = inout,
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
      dplyr::select(-encounter_ide, -patient_ide, -inout) %>%
      purrr::map(as.character) %>%
      data.frame(check.names = F, stringsAsFactors = F) %>%
      dbPush(demodata, "visit_dimension")
  }

  # Update the existing encounters in visit_dimension
  if (nrow(existing) > 0)
  {
    encounters %>%
      dplyr::left_join(patient_mapping) %>%
      dplyr::select(-patient_ide) %>%
      dplyr::mutate(encounter_ide = encounter_ide %>% as.character) %>%
      dplyr::inner_join(existing, by = "encounter_ide") %>%
      dplyr::mutate(length_of_stay = ifelse(is.na(end_date), floor(as.numeric(Sys.Date() - start_date)), floor(as.numeric(end_date - start_date))),
                    start_date = ifelse(is.na(start_date), "", format(start_date, format = "%m/%d/%Y %H:%M:%S")),
                    end_date = ifelse(is.na(end_date), NA, format(end_date, format = "%m/%d/%Y %H:%M:%S")),
                    active_status_cd = ifelse(is.na(end_date), "O", ""),
                    inout_cd = inout,
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
      dplyr::select(-encounter_ide, -patient_ide, -inout) %>%
      purrr::map(as.character) %>%
      data.frame(check.names = F, stringsAsFactors = F) %>%
      dbUpdate(demodata, "visit_dimension", "encounter_num")
  }

  RPostgreSQL::dbDisconnect(demodata)

  encounters %>%
    dplyr::mutate(encounter_ide = encounter_ide %>% as.character) %>%
    dplyr::inner_join(new_encounters, by = "encounter_ide") %>%
    dplyr::select(encounter_ide, encounter_num) %>%
    dplyr::bind_rows(encounters %>%
              dplyr::mutate(encounter_ide = encounter_ide %>% as.character) %>%
              dplyr::inner_join(existing, by = "encounter_ide") %>%
              dplyr::select(encounter_ide, encounter_num)) %>%
    dplyr::mutate(encounter_num = encounter_num %>% as.character) %>%
    dplyr::distinct() %>%
    purrr::map(as.character) %>%
    data.frame(check.names = F, stringsAsFactors = F)
}

#' Add observations to the CRC cell
#'
#' Add observations to the CRC cell
#'
#' The observations dataframe must contain the following columns:
#' - encounter_ide: the original encounter ID
#' - patient_ide: the original patient ID
#' - start_date: the start date of the encounter, as Date object
#' - concept_cd: the concept to insert
#' - provider_id: the provider
#' - modifier_cd: optionnal modifier for the concept
#' Other observation fact columns can optionnaly be included, 
#' such as end_date, valtype_cd, tval_char, nval_num, valueflag_cd, units_cd, etc.
#'
#' @param observations A dataframe of observation facts
#' @param patient_mapping The patient mapping table
#' @param encounter_mapping The encounter mapping table
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the admin account
#' @export
add_observations <- function(observations, patient_mapping, encounter_mapping, host = "", admin = "", pass = "")
{
  options(scipen = 999)

  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  observations %>%
    dplyr::mutate(encounter_ide = encounter_ide %>% as.character) %>%
    dplyr::left_join(patient_mapping) %>%
    dplyr::left_join(encounter_mapping) %>%
    dplyr::select(-patient_ide, -encounter_ide) -> observations

  dplyr::src_postgres("i2b2demodata", host, user = admin, password = pass) %>%
    dplyr::tbl("observation_fact") %>%
    dplyr::filter(encounter_num %in% observations$encounter_num &
           patient_num %in% observations$patient_num &
           concept_cd %in% observations$concept_cd &
           provider_id %in% observations$provider_id &
           start_date %in% observations$start_date &
           modifier_cd %in% observations$modifier_cd) %>%
    dplyr::select(encounter_num, patient_num, concept_cd, provider_id, start_date, modifier_cd) %>%
    dplyr::collect(n = Inf) -> existing

  if (nrow(existing) == 0)
  {
    existing <- data.frame(encounter_num = character(0), patient_num = character(0), concept_cd = character(0), provider_id = character(0), start_date = as.Date(character(0)), modifier_cd = character(0))
  } else
  {
    existing %>%
    dplyr::mutate(start_date = as.Date(start_date),
           patient_num = as.character(patient_num),
           encounter_num = as.character(encounter_num)) -> existing
  }

  observations %>%
    dplyr::anti_join(existing) -> new_observations

  observations %>%
    dplyr::inner_join(existing) -> old_observations

  new_observations %>%
    dplyr::mutate(start_date = ifelse(is.na(start_date), "", format(start_date, format = "%m/%d/%Y %H:%M:%S")),
           update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
  dbPush(demodata, "observation_fact")

  old_observations %>%
    dplyr::mutate(start_date = ifelse(is.na(start_date), "", format(start_date, format = "%m/%d/%Y %H:%M:%S")),
           update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
  dbUpdate(demodata, "observation_fact", c("encounter_num", "patient_num", "concept_cd", "start_date"))

  RPostgreSQL::dbDisconnect(demodata)
}

#' Rebuild the indexes
#'
#' Rebuild the indexes in i2b2demodata
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the admin account
#' @export
rebuild_indexes_demodata <- function(host = "", admin = "", pass = "")
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "REINDEX DATABASE i2b2demodata;")
  RPostgreSQL::dbDisconnect(demodata)
}
