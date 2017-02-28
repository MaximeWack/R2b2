#' Clear the default imdata tables
#'
#' Clear the default imdata tables
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_default_imdata <- function(host, admin, pass)
{
  c("im_mpi_demographics", "im_mpi_mapping", "im_project_patients", "im_project_sites") %>%
    purrr::walk(~clear_table("i2b2imdata", .x, host, admin, pass))
}

#' Add patients to the IM cell
#'
#' Add patients to the IM cell, generate new encrypted IDs,
#' and create a project site if needed
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the admin account
#' @param patients A vector of patients IDs to insert
#' @param project The project to add the patients to
#' @export
add_patients_imdata <- function(host, admin, pass, patients, project)
{
  options(scipen = 999)

  imdata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2imdata", user = admin, password = pass)

# Get the existing patient mappings
  dplyr::src_postgres("i2b2imdata", host, user = admin, password = pass) %>%
    dplyr::tbl("im_mpi_mapping") %>%
    dplyr::select(global_id, lcl_site, lcl_id) %>%
    dplyr::collect(n = Inf) -> existing

# Create new IDEs
  new_id_start <- ifelse(nrow(existing) == 0, 100000001, existing$global_id %>% as.numeric %>% max + 1)

  data.frame(lcl_id = as.character(patients)) %>%
    dplyr::anti_join(existing) %>%
    dplyr::mutate(global_id = seq(new_id_start, length.out = nrow(.))) -> new_patients

# Push the new patient mappings
  new_patients %>%
    dplyr::mutate(lcl_site = project,
                  lcl_status  = "A",
                  update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
  dbPush(con = imdata, table = "im_mpi_mapping", .)

# Push the new patient demographics
  new_patients %>%
    dplyr::mutate(global_status = "A",
                  update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    dplyr::select(- lcl_id) %>%
  dbPush(con = imdata, table = "im_mpi_demographics", .)

# Push the new project sites
  data.frame(project_id = project,
             lcl_site = project,
             project_status = "A",
             update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
  dbPush(con = imdata, table = "im_project_sites", .)

# Push the new project patients
  new_patients %>%
    dplyr::mutate(project_id = project,
                  patient_project_status = "A",
                  update_date = format(Sys.Date(), "%m/%d/%Y")) %>%
    dplyr::select(- lcl_id) %>%
  dbPush(con = imdata, table = "im_project_patients", .)

  RPostgreSQL::dbDisconnect(imdata)
}
