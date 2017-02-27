#' Import data into i2b2
#'
#' Import data from a data frame into i2b2
#'
#' The data dataframe must contain the following columns:
#' - encounter_ide: the original encounter ID
#' - patient_ide: the original patient ID
#' - start_date: the start date of the encounter, as Date object
#' - concept_cd: the concept to insert
#' - provider_id: the provider
#' - modifier_cd: optionnal modifier for the concept
#' Other observation fact columns can optionnaly be included, 
#' such as end_date, valtype_cd, tval_char, nval_num, valueflag_cd, units_cd, etc.
#'
#' The function creates and updates the corresponding patients and encounters,
#' inserts the new observations, and finally rebuilds the indexes in the database.
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the admin account
#' @param data A dataframe of observation facts
#' @param project The project to which to add the patients and their data
#' @export
import_data <- function(host, admin, pass, data, project)
{
  # Patients
  data %>%
    dplyr::select(patient_ide, birth_date, death_date, gender) %>%
    dplyr::distinct() %>%
    add_patients_demodata(host, admin, pass, ., project) -> patient_mapping

  # Encounters
  data %>%
    dplyr::select(patient_ide, encounter_ide, start_date, end_date) %>%
    dplyr::distinct() %>%
    add_encounters(host, admin, pass, ., project, patient_mapping) -> encounter_mapping

  # Observations
  data %>%
    dplyr::distinct() %>%
    add_observations(host, admin, pass, ., patient_mapping, encounter_mapping)

  # Rebuild indexes
  rebuild_indexes_demodata(host, admin, pass)
}
