#' Clear the default imdata tables
#'
#' Clear the default imdata tables
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_default_imdata <- function(host = "127.0.0.1", admin, pass)
{
  clear_mpi_demographics(host, admin, pass)
  clear_mpi_mapping(host, admin, pass)
  clear_project_patients(host, admin, pass)
  clear_project_sites(host, admin, pass)
}

#' Clear the mpi_demographics table
#'
#' Clear the default mpi_demographics tables
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_mpi_demographics <- function(host, admin, pass)
{
  imdata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2imdata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(imdata, "DELETE FROM im_mpi_demographics;")

  RPostgreSQL::dbDisconnect(imdata)
}

#' Clear the mpi_mapping tables
#'
#' Clear the mpi_mapping table
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_mpi_mapping <- function(host, admin, pass)
{
  imdata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2imdata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(imdata, "DELETE FROM im_mpi_mapping;")

  RPostgreSQL::dbDisconnect(imdata)
}

#' Clear the project_patients tables
#'
#' Clear the project_patients tables
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_project_patients <- function(host, admin, pass)
{
  imdata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2imdata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(imdata, "DELETE FROM im_project_patients;")

  RPostgreSQL::dbDisconnect(imdata)
}

#' Clear the project_sites tables
#'
#' Clear the project_sites tables
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_project_sites <- function(host, admin, pass)
{
  imdata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2imdata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(imdata, "DELETE FROM im_project_sites;")

  RPostgreSQL::dbDisconnect(imdata)
}
