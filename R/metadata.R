clear_metadata <- function(host = "localhost", admin, pass)
{
  metadata   <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2metadata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE birn;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE custom_meta;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE i2b2;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE icd10_icd9;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE icd10_icd9;")

  RPostgreSQL::dbGetQuery(metadata, "DELETE FROM schemes;")
  RPostgreSQL::dbGetQuery(metadata, "DELETE FROM table_access;")

  RPostgreSQL::dbGetQuery(metadata, "INSERT INTO schemes VALUES (NULL, 'None', 'No scheme');")

  RPostgreSQL::dbDisconnect(metadata)
}
