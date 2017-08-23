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
  c("code_lookup",
    "concept_dimension",
    "encounter_mapping",
    "modifier_dimension",
    "observation_fact",
    "patient_dimension",
    "patient_mapping",
    "provider_dimension",
    "qt_analysis_plugin",
    "qt_analysis_plugin_result_type",
    "qt_patient_enc_collection",
    "qt_patient_set_collection",
    "qt_pdo_query_master",
    "qt_xml_result",
    "qt_query_result_instance",
    "qt_query_instance",
    "qt_query_master",
    "visit_dimension") %>%
  purrr::walk(~clear_table(stringr::str_c("i2b2demodata"), .x, host, admin, pass))
}

#' Clear the demodata tables
#'
#' Clear the demodata tables
#'
#' @param project The name of the project
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_demodata <- function(project, host = "", admin = "", pass = "")
{
  c("code_lookup",
    "concept_dimension",
    "encounter_mapping",
    "modifier_dimension",
    "observation_fact",
    "patient_dimension",
    "patient_mapping",
    "provider_dimension",
    "qt_analysis_plugin",
    "qt_analysis_plugin_result_type",
    "qt_patient_enc_collection",
    "qt_patient_set_collection",
    "qt_pdo_query_master",
    "qt_xml_result",
    "qt_query_result_instance",
    "qt_query_instance",
    "qt_query_master",
    "visit_dimension") %>%
  purrr::walk(~clear_table(stringr::str_c("i2b2", project ,"data"), .x, host, admin, pass))
}

#' Delete modifiers
#'
#' Delete modifiers from modifier_dimension
#'
#' @param scheme The scheme to delete from the concepts
#' @param project The name of the project
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
delete_modifier <- function(scheme, project, host = "", admin = "", pass = "")
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = stringr::str_c("i2b2", stringr::str_to_lower(project), "data"), user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, stringr::str_c("DELETE FROM modifier_dimension WHERE (modifier_cd LIKE '", scheme, ":%');"))

  RPostgreSQL::dbDisconnect(demodata)
}

list_modifier <- function(scheme, project, host = "", admin = "", pass = "")
{
  dplyr::src_postgres(stringr::str_c("i2b2", stringr::str_to_lower(project), "data"), host = host, user = admin, pass = pass) %>%
    dplyr::tbl("modifier_dimension") %>%
    dplyr::collect() %>%
    dplyr::filter(modifier_cd %>% stringr::str_detect(stringr::str_c(scheme, ":.*")))
}

#' Delete concepts
#'
#' Delete concepts from concept_dimension
#'
#' @param scheme The scheme to delete from the concepts
#' @param project The name of the project
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
delete_concept <- function(scheme, project, host = "", admin = "", pass = "")
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = stringr::str_c("i2b2", stringr::str_to_lower(project), "data"), user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, stringr::str_c("DELETE FROM concept_dimension WHERE (concept_cd LIKE '", scheme, ":%');"))

  RPostgreSQL::dbDisconnect(demodata)
}

#' List concepts
#'
#' List the concepts corresponding to a scheme
#'
#' @param scheme The scheme to get the concepts from
#' @param project The name of the project
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @return A list of concepts
#' @export
list_concepts <- function(scheme, project, host = "", admin = "", pass = "")
{
  dplyr::src_postgres(stringr::str_c("i2b2", stringr::str_to_lower(project), "data"), host = host, user = admin, pass = pass) %>%
    dplyr::tbl("concept_dimension") %>%
    dplyr::collect() %>%
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
#' @param scheme The scheme to use for this ontology
#' @param project The name of the project
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
populate_concept <- function(ont, modi, scheme, project, host = "", admin = "", pass = "")
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = stringr::str_c("i2b2", stringr::str_to_lower(project), "data"), user = admin, password = pass)

  # Sanitize the ontology
  ont %>%
    dplyr::mutate_all(~stringr::str_replace_all(., "'", "''")) ->
  ont

  if(! modi %>% is.null)
  {
    modi %>%
      dplyr::mutate_all(~stringr::str_replace_all(., "'", "''")) ->
    modi
  }

  # Get the name of the ontology from the scheme
  list_ont(host, admin, pass) %>%
    dplyr::filter(c_table_cd == scheme) %>%
    dplyr::pull(c_name) ->
  name

  # Create the data frame holding the contents of the new table
  data.frame(concept_path = ont$c_fullname, stringsAsFactors = F) %>%
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

  if (! modi %>% is.null)
  {
    modi %>%
      dplyr::mutate(name_char = c_fullname %>% stringr::str_extract(" .*$") %>% stringr::str_trim(),
                    modifier_path = stringr::str_c("\\", name_char, "\\"),
                    modifier_cd = stringr::str_c(scheme, ":", c_fullname %>% stringr::str_extract("^.+? ") %>% stringr::str_trim()),
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
      dplyr::select(-c_fullname) %>%
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
#' @param scheme The scheme to use for this ontology
#' @param project The name of the project
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
populate_provider <- function(ont, scheme, project, host = "", admin = "", pass = "")
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = stringr::str_c("i2b2", stringr::str_to_lower(project), "data"), user = admin, password = pass)

  # Sanitize the ontology
  ont %>%
    dplyr::mutate_all(~stringr::str_replace_all(., "'", "''")) ->
  ont

  # Get the name of the ontology from the scheme
  list_ont(host, admin, pass) %>%
    dplyr::filter(c_table_cd == scheme) %>%
    dplyr::pull(c_name) ->
  name

  # Create the data frame holding the contents of the new table
  data.frame(provider_path = ont$c_fullname, stringsAsFactors = F) %>%
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
#' - sex_cd (F or M)
#'
#' @param patients A dataframe of patients
#' @param project The project to add the patients to
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the admin account
#' @export
add_patients_demodata <- function(patients, project, host = "", admin = "", pass = "")
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = stringr::str_c("i2b2", stringr::str_to_lower(project), "data"), user = admin, password = pass)

# Upsert patients mappings
  patients %>%
    dplyr::mutate(patient_ide_source = project,
                  patient_ide_status = "A",
                  project_id = project,
                  patient_num = patient_ide,
                  update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    dplyr::select(patient_ide, patient_ide_source, patient_num, patient_ide_status, project_id, update_date) %>%
  dbUpsert(demodata, "patient_mapping", c("patient_ide", "patient_ide_source", "project_id"))

  patients %>%
    dplyr::mutate(age_in_years_num = ifelse(is.na(death_date), floor(as.numeric(Sys.Date() - birth_date)/365.25), floor(as.numeric(death_date - birth_date)/365.25)),
                  birth_date = ifelse(is.na(birth_date), NA, format(birth_date, format = "%m/%d/%Y %H:%M:%S")),
                  death_date = ifelse(is.na(death_date), NA, format(death_date, format = "%m/%d/%Y %H:%M:%S")),
                  vital_status_cd = ifelse(is.na(death_date), "N", "S"),
                  update_date = format(Sys.Date(), "%m/%d/%Y"),
                  patient_num = patient_ide) %>%
    dplyr::select(patient_num, vital_status_cd, birth_date, death_date, sex_cd, age_in_years_num, update_date) %>%
  dbUpsert(demodata, "patient_dimension", "patient_num")

  RPostgreSQL::dbDisconnect(demodata)
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
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the admin account
#' @return An encounter mapping dataframe for the encounters
#' @export
add_encounters <- function(encounters, project, host = "", admin = "", pass = "")
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = stringr::str_c("i2b2", stringr::str_to_lower(project), "data"), user = admin, password = pass)

  demodata %>%
    dplyr::tbl("encounter_mapping") %>%
    dplyr::select(encounter_ide, encounter_num) %>%
    dplyr::collect() ->
  mapping

  if (nrow(mapping) == 0)
    mapping <- data.frame(encounter_ide = character(0), encounter_num = numeric(0), stringsAsFactors = F)

  start <- ifelse(nrow(mapping) == 0, 1, max(mapping$encounter_num) + 1)

  encounters %>%
    dplyr::inner_join(mapping) -> mapped

  encounters %>%
    dplyr::anti_join(mapping) %>%
    dplyr::mutate(encounter_num = seq(start, length.out = nrow(.))) -> unmapped

  unmapped %>%
      dplyr::mutate(encounter_ide_source = project,
                    encounter_ide_status  = "A",
                    project_id = project,
                    patient_ide_source = "HIVE",
                    update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
      dplyr::select(encounter_ide, encounter_ide_source, project_id, encounter_num, patient_ide, patient_ide_source, encounter_ide_status, update_date) %>%
  dbUpsert(demodata, "encounter_mapping", c("encounter_ide", "encounter_ide_source", "project_id", "patient_ide", "patient_ide_source"))

  mapped %>%
    dplyr::bind_rows(unmapped) %>%
    dplyr::mutate(length_of_stay = ifelse(is.na(end_date), floor(as.numeric(Sys.Date() - start_date)), floor(as.numeric(end_date - start_date))),
                  start_date = ifelse(is.na(start_date), NA, format(start_date, format = "%m/%d/%Y %H:%M:%S")),
                  end_date = ifelse(is.na(end_date), NA, format(end_date, format = "%m/%d/%Y %H:%M:%S")),
                  active_status_cd = ifelse(is.na(end_date), "O", "S"),
                  inout_cd = inout,
                  patient_num = patient_ide,
                  update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    dplyr::select(encounter_num, patient_num, active_status_cd, start_date, end_date, inout_cd, length_of_stay, update_date) %>%
  dbUpsert(demodata, "visit_dimension", c("encounter_num", "patient_num"))

  RPostgreSQL::dbDisconnect(demodata)
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
#' @param project The name of the project
#' @param patient_mapping The patient mapping table
#' @param encounter_mapping The encounter mapping table
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the admin account
#' @export
add_observations <- function(observations, project, host = "", admin = "", pass = "")
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = stringr::str_c("i2b2", stringr::str_to_lower(project), "data"), user = admin, password = pass)

  # Get the encounter mapping
  demodata %>%
    dplyr::tbl("encounter_mapping") %>%
    dplyr::select(encounter_ide, encounter_num) %>%
    dplyr::collect() ->
  mapping

  observations %>%
    dplyr::inner_join(mapping) -> observations

  # create the text_search_index column
  RPostgreSQL::dbGetQuery(demodata, "SELECT max(text_search_index) from observation_fact;") %>%
  .$max -> nextval

  if (nextval %>% is.na)
  {
    RPostgreSQL::dbGetQuery(demodata, "SELECT nextval('observation_fact_text_search_index_seq'::regclass);") %>%
      .$nextval -> nextval
  }

  observations %>%
    dplyr::mutate(start_date = ifelse(is.na(start_date), NA, format(start_date, format = "%m/%d/%Y %H:%M:%S")),
                  patient_num = patient_ide,
                  update_date = format(Sys.Date(), "%m/%d/%Y"),
                  text_search_index = seq(nextval+1, length.out = nrow(.))) %>%
    dplyr::group_by(patient_ide, encounter_ide, start_date, provider_id, concept_cd, modifier_cd) %>%
    dplyr::mutate(instance_num = seq(1, length.out = n())) %>%
    dplyr::ungroup() %>%
    dplyr::select(-patient_ide, -encounter_ide) %>%
  dbUpsert(demodata, "observation_fact", c("patient_num", "concept_cd", "modifier_cd", "start_date", "encounter_num", "instance_num", "provider_id"))

  RPostgreSQL::dbDisconnect(demodata)
}

#' Rebuild the indexes
#'
#' Rebuild the indexes in i2b2demodata
#'
#' @param project The name of the project
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the admin account
#' @export
rebuild_indexes_demodata <- function(project, host = "", admin = "", pass = "")
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = stringr::str_c("i2b2", stringr::str_to_lower(project), "data"), user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, stringr::str_c("REINDEX DATABASE i2b2", stringr::str_to_lower(project), "data;"))
  RPostgreSQL::dbDisconnect(demodata)
}
