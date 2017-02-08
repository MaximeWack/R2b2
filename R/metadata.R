clear_metadata <- function(host = "localhost", admin, pass)
{
  metadata   <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2metadata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE birn;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE custom_meta;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE i2b2;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE icd10_icd9;")

  RPostgreSQL::dbGetQuery(metadata, "DELETE FROM schemes;")
  RPostgreSQL::dbGetQuery(metadata, "DELETE FROM table_access;")

  RPostgreSQL::dbGetQuery(metadata, "INSERT INTO schemes VALUES (NULL, 'None', 'No scheme');")

  RPostgreSQL::dbDisconnect(metadata)
}

add_ont <- function(host = "localhost", admin, pass, name, scheme, description)
{
  metadata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2metadata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("INSERT INTO schemes VALUES ('", scheme, ":', '", scheme, "', '", description, "');"))

  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("INSERT INTO table_access VALUES ('", scheme, "', '", scheme, "', 'N', 0, '\\", name, "\\', '", name, "', 'N', 'CA', NULL, NULL, NULL, 'concept_cd', 'concept_dimension', 'concept_path', 'T', 'LIKE', '\\", name, "\\', NULL, '", description, "', NULL, NULL, NULL, NULL);"))

  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("CREATE TABLE ", scheme, " (
                          C_HLEVEL INT            NOT NULL,
                          C_FULLNAME VARCHAR(700) NOT NULL,
                          C_NAME VARCHAR(2000)    NOT NULL,
                          C_SYNONYM_CD CHAR(1)    NOT NULL,
                          C_VISUALATTRIBUTES CHAR(3)  NOT NULL,
                          C_TOTALNUM INT      NULL,
                          C_BASECODE VARCHAR(50)  NULL,
                          C_METADATAXML TEXT    NULL,
                          C_FACTTABLECOLUMN VARCHAR(50) NOT NULL,
                          C_TABLENAME VARCHAR(50) NOT NULL,
                          C_COLUMNNAME VARCHAR(50)  NOT NULL,
                          C_COLUMNDATATYPE VARCHAR(50)  NOT NULL,
                          C_OPERATOR VARCHAR(10)  NOT NULL,
                          C_DIMCODE VARCHAR(700)  NOT NULL,
                          C_COMMENT TEXT      NULL,
                          C_TOOLTIP VARCHAR(900)  NULL,
                          M_APPLIED_PATH VARCHAR(700) NOT NULL,
                          UPDATE_DATE timestamp   NOT NULL,
                          DOWNLOAD_DATE timestamp NULL,
                          IMPORT_DATE timestamp NULL,
                          SOURCESYSTEM_CD VARCHAR(50) NULL,
                          VALUETYPE_CD VARCHAR(50)  NULL,
                          M_EXCLUSION_CD  VARCHAR(25) NULL,
                          C_PATH  VARCHAR(700)   NULL,
                          C_SYMBOL  VARCHAR(50) NULL,
                          PLAIN_CODE  VARCHAR(25) NULL);"))

  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("CREATE INDEX META_FULLNAME_IDX_", scheme, " ON ", scheme, "(C_FULLNAME);"))
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("CREATE INDEX META_APPL_PATH_", scheme, "_IDX ON ", scheme, "(M_APPLIED_PATH);"))
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("CREATE INDEX META_EXCLUSION_", scheme, "_IDX ON ", scheme, "(M_EXCLUSION_CD);"))
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("CREATE INDEX META_HLEVEL_", scheme, "_IDX ON ", scheme, "(C_HLEVEL);"))
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("CREATE INDEX META_SYNONYM_", scheme, "_IDX ON ", scheme, "(C_SYNONYM_CD);"))

  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("ALTER TABLE ", scheme, " OWNER TO i2b2metadata;"))

  RPostgreSQL::dbDisconnect(metadata)
}
