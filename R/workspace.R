#' Clear workspace tables
#'
#' Clear the workspace tables and inserts default values
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_default_workdata <- function(host = "", admin = "", pass = "")
{
  c("workplace",
    "workplace_access") %>%
  purrr::walk(~clear_table("i2b2workdata", .x, host, admin, pass))

  RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2workdata", user = admin, password = pass) %>%
    RPostgreSQL::dbGetQuery("INSERT INTO workplace_access values ('demo', 'WORKPLACE', 'N', '0', '@', '@', '@', 'N', '0', NULL, 'CA', '@', NULL, NULL, NULL);")
}

