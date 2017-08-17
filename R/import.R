# #' Import data into i2b2
# #'
# #' Import data from a data frame into i2b2
# #'
# #' The data dataframe must contain the following columns:
# #' - encounter_ide: the original encounter ID
# #' - patient_ide: the original patient ID
# #' - start_date: the start date of the encounter, as Date object
# #' - concept_cd: the concept to insert
# #' - provider_id: the provider
# #' - modifier_cd: optionnal modifier for the concept
# #' Other observation fact columns can optionnaly be included,
# #' such as end_date, valtype_cd, tval_char, nval_num, valueflag_cd, units_cd, etc.
# #'
# #' The function creates and updates the corresponding patients and encounters,
# #' inserts the new observations, and finally rebuilds the indexes in the database.
# #'
# #' @param data A dataframe of observation facts
# #' @param project The project to which to add the patients and their data
# #' @param host The host to connect to
# #' @param admin The admin account for the PostgreSQL database
# #' @param pass The password for the admin account
# #' @export
# import_data <- function(data, project, host = "", admin = "", pass = "")
# {
#   # Patients
#   data %>%
#     dplyr::select(patient_ide, birth_date, death_date, gender) %>%
#     dplyr::distinct() %>%
#     add_patients_demodata(project, host, admin, pass) ->
#   patient_mapping

#   # Encounters
#   data %>%
#     dplyr::select(patient_ide, encounter_ide, start_date, end_date) %>%
#     dplyr::distinct() %>%
#     add_encounters(project, patient_mapping, host, admin, pass) ->
#   encounter_mapping

#   # Observations
#   data %>%
#     dplyr::distinct() %>%
#   add_observations(patient_mapping, encounter_mapping, host, admin, pass)

#   # Rebuild indexes
#   rebuild_indexes_demodata(host, admin, pass)
# }

import_patients_visits <- function(patients, project)
{
  patients %>%
    stats::setNames(c("patient_ide", "encounter_ide", "start_date", "end_date", "sex_cd", "birth_date", "death_date", "rum_start", "rum_end", "provider_id", "project")) %>%
    dplyr::mutate(start_date = start_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
           end_date = end_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
           birth_date = birth_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
           death_date = death_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
           sex_cd = ifelse(sex_cd == "1", "M", "F")) %>%
    dplyr::filter(!is.na(patient_ide)) %>%
    dplyr::mutate(patient_ide = ifelse(patient_ide %>% as.numeric > 2^32,
                                patient_ide %>% stringr::str_sub(2),
                                patient_ide)) ->
  patients

  # Patients
  patients %>%
    dplyr::select(patient_ide, birth_date, death_date, sex_cd) %>%
    dplyr::distinct() %>%
    add_patients_demodata(project)

  # Encounters
  patients %>%
    dplyr::select(patient_ide, encounter_ide, start_date, end_date) %>%
    dplyr::distinct() %>%
    dplyr::mutate(inout = "I") %>%
    add_encounters(project)

  # Observations : Age à l'hospitalisation
  patients %>%
    dplyr::select(patient_ide, encounter_ide, start_date, birth_date, provider_id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(concept_cd = "HOS:age_hospit",
           provider_id = stringr::str_c("STRUCT:", provider_id),
           modifier_cd = "@",
           instance_num = 1,
           valtype_cd = "N",
           tval_char = "E",
           nval_num = as.numeric(start_date - birth_date)/365.25) %>%
    dplyr::select(-birth_date) %>%
    add_observations(project)
}

import_diagnostics <- function(diags, project)
{
  # Observations : Diagnostics
  diags %>%
    stats::setNames(c("patient_ide", "encounter_ide", "start_date", "end_date", "provider_id", "concept_cd", "modifier_cd")) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(concept_cd)) %>%
    dplyr::mutate(concept_cd = stringr::str_c("CIM:", concept_cd),
           provider_id = stringr::str_c("STRUCT:", provider_id),
           modifier_cd = stringr::str_c("CIM:", modifier_cd),
           instance_num = 1) %>%
    dplyr::mutate(patient_ide = ifelse(patient_ide %>% as.numeric > 2^32,
                                patient_ide %>% stringr::str_sub(2),
                                patient_ide)) %>%
    add_observations(project)
}

import_actes <- function(actes, project)
{
  # Observations : Actes
  actes %>%
    stats::setNames(c("patient_ide", "encounter_ide", "provider_id", "concept_cd", "start_date")) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(concept_cd)) %>%
    dplyr::mutate(concept_cd = stringr::str_c("CCAM:", concept_cd),
           provider_id = stringr::str_c("STRUCT:", provider_id),
           modifier_cd = "@",
           instance_num = 1) %>%
    dplyr::mutate(patient_ide = ifelse(patient_ide %>% as.numeric > 2^32,
                                patient_ide %>% stringr::str_sub(2),
                                patient_ide)) %>%
    add_observations(project)
}

