#' Import patients and their visits
#'
#' Import patients and their visits
#'
#' Import the patient_dimension and visit_dimension death_data
#' As well as creating the mappings and add visit age observations
#' 
#' Structure for patient dataframe:
#' - patient_ide : character
#' - encounter_ide : character
#' - start_date : Date
#' - end_date : Date
#' - rum_start : Date
#' - rum_end : Date
#' - birth_date : Date
#' - death_date : Date
#' - sex_cd : char, 'M' or 'F'
#' - provider_id : char, 'STRUCT:xxx'
#'
#' @param patients A formatted dataframe with correctly named columns
#' @param project The project to add the data to
#' @export
import_patients_visits <- function(patients, project)
{
  # Patients
  patients %>%
    dplyr::select(patient_ide, birth_date, death_date, sex_cd) %>%
    dplyr::distinct() %>%
  add_patients_demodata(project)

  # Encounters
  patients %>%
    dplyr::select(patient_ide, encounter_ide, start_date, end_date) %>%
    dplyr::distinct() %>%
    dplyr::mutate(inout_cd = "I") %>%
  add_encounters(project)

  # Observations : Age à l'hospitalisation
  patients %>%
    dplyr::select(patient_ide, encounter_ide, start_date = rum_start, birth_date, provider_id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(concept_cd = "HOS:age_hospit",
           modifier_cd = "@",
           valtype_cd = "N",
           tval_char = "E",
           nval_num = as.numeric(start_date - birth_date)/365.25) %>%
    dplyr::select(-birth_date) %>%
  add_observations(project)
}

import_mensurations <- function(mensurations, patients, project)
{
  patients %>%
    dplyr::inner_join(mensurations) %>%
    dplyr::select(patient_ide, encounter_ide, start_date = rum_start, end_date = rum_end, provider_id, concept_cd, nval_num, modifier_cd, valtype_cd, tval_char) %>%
    add_observations(project)
}

import_bios <- function(bios, patients, project)
{
  bios %>%
    dplyr::inner_join(patients, by = c("patient_ide", "encounter_ide")) %>%
    dplyr::rename(start_date = start_date.x) %>%
    dplyr::filter(start_date >= rum_start & start_date <= rum_end) %>%
    dplyr::select(-rum_start, -rum_end, -start_date.y, -sex_cd, -birth_date, -death_date, -project) %>%
    dplyr::mutate(modifier_cd = "@",
                  valtype_cd = "N",
                  tval_char = "E",
                  nval_num = nval_num %>% stringr::str_replace(",", ".")) %>%
  add_observations(project)
}

