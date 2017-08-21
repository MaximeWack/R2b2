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
    dplyr::mutate(encounter_ide = ifelse(encounter_ide %>% stringr::str_detect("\\."),
                                         stringr::str_c(encounter_ide, lubridate::day(start_date) %>% stringr::str_pad(2, "left", "0")),
                                         encounter_ide)) %>%
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
    dplyr::select(patient_ide, encounter_ide, start_date = rum_start, birth_date, provider_id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(concept_cd = "HOS:age_hospit",
           provider_id = stringr::str_c("STRUCT:", provider_id),
           modifier_cd = "@",
           valtype_cd = "N",
           tval_char = "E",
           nval_num = as.numeric(start_date - birth_date)/365.25) %>%
    dplyr::select(-birth_date) %>%
    dplyr::group_by(patient_ide, encounter_ide, start_date, provider_id, concept_cd, modifier_cd) %>%
    dplyr::mutate(instance_num = 1:n()) %>%
    dplyr::ungroup() %>%
  add_observations(project)
}

import_mensurations <- function(mensurations, patients, project)
{
  mensurations %>%
    stats::setNames(c("patient_ide", "encounter_ide", "poids", "taille", "IMC")) %>%
    dplyr::filter(!is.na(patient_ide)) %>%
    dplyr::mutate(encounter_ide = ifelse(encounter_ide %>% stringr::str_detect("\\."),
                                         stringr::str_c(encounter_ide, lubridate::day(start_date) %>% stringr::str_pad(2, "left", "0")),
                                         encounter_ide)) %>%
    dplyr::mutate(patient_ide = ifelse(patient_ide %>% as.numeric > 2^32,
                                patient_ide %>% stringr::str_sub(2),
                                patient_ide)) ->
  mensurations

  patients %>%
    stats::setNames(c("patient_ide", "encounter_ide", "start_date", "end_date", "sex_cd", "birth_date", "death_date", "rum_start", "rum_end", "provider_id", "project")) %>%
    dplyr::mutate(start_date = start_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
           end_date = end_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
           birth_date = birth_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
           death_date = death_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
           sex_cd = ifelse(sex_cd == "1", "M", "F")) %>%
    dplyr::filter(!is.na(patient_ide)) %>%
    dplyr::mutate(encounter_ide = ifelse(encounter_ide %>% stringr::str_detect("\\."),
                                         stringr::str_c(encounter_ide, lubridate::day(start_date) %>% stringr::str_pad(2, "left", "0")),
                                         encounter_ide)) %>%
    dplyr::mutate(patient_ide = ifelse(patient_ide %>% as.numeric > 2^32,
                                patient_ide %>% stringr::str_sub(2),
                                patient_ide)) ->
  patients

  patients %>%
    dplyr::left_join(mensurations) -> mensurations

  mensurations %>%
    dplyr::select(patient_ide, encounter_ide, provider_id, start_date = rum_start, end_date = rum_end, poids) %>%
    dplyr::mutate(concept_cd = "HOS:poids",
                  provider_id = stringr::str_c("STRUCT:", provider_id),
                  modifier_cd = "@",
                  valtype_cd = "N",
                  tval_char = "E",
                  nval_num = as.numeric(poids)) %>%
    dplyr::filter(!is.na(nval_num)) %>%
    dplyr::select(-poids) %>%
    dplyr::group_by(patient_ide, encounter_ide, start_date, provider_id, concept_cd, modifier_cd) %>%
    dplyr::mutate(instance_num = 1:n()) %>%
    dplyr::ungroup() %>%
    add_observations(project)

  mensurations %>%
    dplyr::select(patient_ide, encounter_ide, provider_id, start_date = rum_start, end_date = rum_end, taille) %>%
    dplyr::mutate(concept_cd = "HOS:taille",
                  provider_id = stringr::str_c("STRUCT:", provider_id),
                  modifier_cd = "@",
                  valtype_cd = "N",
                  tval_char = "E",
                  nval_num = as.numeric(taille)) %>%
    dplyr::filter(!is.na(nval_num)) %>%
    dplyr::select(-taille) %>%
    dplyr::group_by(patient_ide, encounter_ide, start_date, provider_id, concept_cd, modifier_cd) %>%
    dplyr::mutate(instance_num = 1:n()) %>%
    dplyr::ungroup() %>%
    add_observations(project)

  mensurations %>%
    dplyr::select(patient_ide, encounter_ide, provider_id, start_date = rum_start, end_date = rum_end, IMC) %>%
    dplyr::mutate(concept_cd = "HOS:IMC",
                  provider_id = stringr::str_c("STRUCT:", provider_id),
                  modifier_cd = "@",
                  valtype_cd = "N",
                  tval_char = "E",
                  nval_num = as.numeric(IMC)) %>%
    dplyr::filter(!is.na(nval_num)) %>%
    dplyr::select(-IMC) %>%
    dplyr::group_by(patient_ide, encounter_ide, start_date, provider_id, concept_cd, modifier_cd) %>%
    dplyr::mutate(instance_num = 1:n()) %>%
    dplyr::ungroup() %>%
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
           modifier_cd = stringr::str_c("CIM:", modifier_cd)) %>%
    dplyr::mutate(encounter_ide = ifelse(encounter_ide %>% stringr::str_detect("\\."),
                                         stringr::str_c(encounter_ide, lubridate::day(start_date)),
                                         encounter_ide)) %>%
    dplyr::mutate(patient_ide = ifelse(patient_ide %>% as.numeric > 2^32,
                                patient_ide %>% stringr::str_sub(2),
                                patient_ide)) %>%
    dplyr::group_by(patient_ide, encounter_ide, start_date, provider_id, concept_cd, modifier_cd) %>%
    dplyr::mutate(instance_num = 1:n()) %>%
    dplyr::ungroup() %>%
  add_observations(project)
}

import_actes <- function(actes, project)
{
  # Observations : Actes
  actes %>%
    stats::setNames(c("patient_ide", "encounter_ide", "provider_id", "concept_cd", "start_date")) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(concept_cd),
                  !is.na(start_date)) %>%
    dplyr::mutate(concept_cd = stringr::str_c("CCAM:", concept_cd),
           provider_id = stringr::str_c("STRUCT:", provider_id),
           modifier_cd = "@") %>%
    dplyr::mutate(encounter_ide = ifelse(encounter_ide %>% stringr::str_detect("\\."),
                                         stringr::str_c(encounter_ide, lubridate::day(start_date)),
                                         encounter_ide)) %>%
    dplyr::mutate(patient_ide = ifelse(patient_ide %>% as.numeric > 2^32,
                                patient_ide %>% stringr::str_sub(2),
                                patient_ide)) %>%
    dplyr::group_by(patient_ide, encounter_ide, start_date, provider_id, concept_cd, modifier_cd) %>%
    dplyr::mutate(instance_num = 1:n()) %>%
    dplyr::ungroup() %>%
  add_observations(project)
}

import_bios <- function(bios, patients, project)
{
  readr::read_csv("../inst/bio.map") -> mapping

  bios %>%
    stats::setNames(c("patient_ide", "encounter_ide", "start_date", "concept_cd", "nval_num")) %>%
    dplyr::filter(!is.na(concept_cd),
                  !is.na(nval_num),
                  !is.na(start_date)) %>%
    dplyr::filter(!concept_cd %in% c("MB_SGT_AER_CB", "MB_SGT_ANA_CB", "MB_LP_TC", "MB_SGT_PED_CB", "MB_CS_NUM_DON_RC", "MB_ANTIBIO_RC")) %>%
    dplyr::left_join(mapping, by = c("concept_cd" = "from")) %>%
    dplyr::mutate(concept_cd = ifelse(!is.na(to), to, concept_cd)) %>%
    dplyr::mutate(concept_cd = stringr::str_c("BIO:", concept_cd)) %>%
    dplyr::mutate(start_date = start_date %>% as.Date(format = "%Y/%m/%d %H:%M:%S")) %>%
    dplyr::select(-to) -> bios

  patients %>%
    stats::setNames(c("patient_ide", "encounter_ide", "start_date", "end_date", "sex_cd", "birth_date", "death_date", "rum_start", "rum_end", "provider_id", "project")) %>%
    dplyr::mutate(rum_start = rum_start %>% as.Date(format = "%Y/%m/%d %H:%M:%S"),
                  rum_end = rum_end %>% as.Date(format = "%Y/%m/%d %H:%M:%S")) %>%
    dplyr::filter(!is.na(patient_ide)) %>%
    dplyr::mutate(encounter_ide = ifelse(encounter_ide %>% stringr::str_detect("\\."),
                                         stringr::str_c(encounter_ide, lubridate::day(start_date) %>% stringr::str_pad(2, "left", "0")),
                                         encounter_ide)) %>%
    dplyr::select(patient_ide, encounter_ide, rum_start, rum_end, provider_id) %>%
    dplyr::mutate(patient_ide = ifelse(patient_ide %>% as.numeric > 2^32,
                                       patient_ide %>% stringr::str_sub(2),
                                       patient_ide)) ->
  patients

  bios %>%
    dplyr::left_join(patients, by = c("patient_ide", "encounter_ide")) %>%
    dplyr::filter(start_date >= rum_start & start_date <= rum_end) %>%
    dplyr::select(-rum_start, -rum_end) %>%
    dplyr::mutate(provider_id = stringr::str_c("STRUCT:", provider_id),
                  modifier_cd = "@",
                  valtype_cd = "N",
                  tval_char = "E") %>%
    dplyr::mutate(nval_num = nval_num %>% stringr::str_replace(",", ".")) %>%
    dplyr::group_by(patient_ide, encounter_ide, start_date, provider_id, concept_cd, modifier_cd) %>%
    dplyr::mutate(instance_num = 1:n()) %>%
    dplyr::ungroup() %>%
  add_observations(project)
}

