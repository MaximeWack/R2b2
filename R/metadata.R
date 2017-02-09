#' Clear the default metadata tables
#'
#' Clear the default metadata tables
#'
#' Drop the birn, custom_meta, i2b2 and icd10_icd9 tables
#' Delete all schemes and table_access
#' Insert the 'No scheme' scheme
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_default_metadata <- function(host = "127.0.0.1", admin, pass)
{
  metadata   <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2metadata", user = admin, password = pass)

# Drop default tables
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE birn;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE custom_meta;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE i2b2;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE icd10_icd9;")

# Empty schemes and table_acess
  RPostgreSQL::dbGetQuery(metadata, "DELETE FROM schemes;")
  RPostgreSQL::dbGetQuery(metadata, "DELETE FROM table_access;")

# Insert the 'empty' scheme
  RPostgreSQL::dbGetQuery(metadata, "INSERT INTO schemes VALUES ('', 'None', 'No scheme');")

  RPostgreSQL::dbDisconnect(metadata)
}

#' Delete an ontology from metadata
#'
#' Delete an existing ontology from metadata
#'
#' Delete the corresponding table
#' Delete the scheme in schemes table
#' Delete the entry in table_acess
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param scheme The scheme to use for this ontology
#' @export
delete_ont<- function(host = "127.0.0.1", admin, pass, scheme)
{
  metadata   <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2metadata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("DROP TABLE ", scheme, ";"))
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("DELETE FROM table_access WHERE (c_table_cd = '", scheme, "');"))
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("DELETE FROM schemes WHERE (c_name = '", scheme, "');"))

  RPostgreSQL::dbDisconnect(metadata)
}

#' Add an ontology to metadata
#'
#' Add an empty ontology space
#'
#' Add a new empty table to metadata, with indexes
#' Add a new scheme to the schemes table
#' Add the corresponding table_access entry for the new table
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param name The name of the new ontology
#' @param scheme The scheme to use for this ontology
#' @param description The description of the scheme
#' @export
add_ont <- function(host = "127.0.0.1", admin, pass, name, scheme, description)
{
  metadata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2metadata", user = admin, password = pass)

# Insert the new scheme
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("INSERT INTO schemes VALUES ('", scheme, ":', '", scheme, "', '", description, "');"))

# Insert the table_acess entry
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("INSERT INTO table_access VALUES ('", scheme, "', '", scheme, "', 'N', 0, '\\", name, "\\', '", name, "', 'N', 'CA', NULL, NULL, NULL, 'concept_cd', 'concept_dimension', 'concept_path', 'T', 'LIKE', '\\", name, "\\', NULL, '", description, "', NULL, NULL, NULL, NULL);"))

# Create the new table
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

# Create the indexes for the new table
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("CREATE INDEX META_FULLNAME_IDX_", scheme, " ON ", scheme, "(C_FULLNAME);"))
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("CREATE INDEX META_APPL_PATH_", scheme, "_IDX ON ", scheme, "(M_APPLIED_PATH);"))
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("CREATE INDEX META_EXCLUSION_", scheme, "_IDX ON ", scheme, "(M_EXCLUSION_CD);"))
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("CREATE INDEX META_HLEVEL_", scheme, "_IDX ON ", scheme, "(C_HLEVEL);"))
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("CREATE INDEX META_SYNONYM_", scheme, "_IDX ON ", scheme, "(C_SYNONYM_CD);"))

# Give ownership of the new table to i2b2metadata
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("ALTER TABLE ", scheme, " OWNER TO i2b2metadata;"))

  RPostgreSQL::dbDisconnect(metadata)
}

#' Populate an empty ontology table
#'
#' Populate an empty ontology table
#'
#' Populate an ontology table
#' ont is a character vector containing all the leaves of the ontology
#' with their respective path, in the form
#' code_level1 label_level1/code_level2 label_level2/.../code_leaf label_leaf
#' The function rebuilds the folders automatically
#' 
#' modi is a character vector containing the modifiers, in the form
#' code_modi label_modi
#' The modifiers apply on all the ontology
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param ont The ontology to insert
#' @param modi The modifiers to insert
#' @param name The name of the new ontology
#' @param scheme The scheme to use for this ontology
#' @export
populate_ont <- function(host = "127.0.0.1", admin, pass, ont, modi, name, scheme)
{
  metadata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2metadata", user = admin, password = pass)

# Sanitize the ontology
  ont <- ont %>% stringr::str_replace_all("'", "''")
  modi <- modi %>% stringr::str_replace_all("'", "''")

# Create the data frame holding the contents of the new table, starting with leaves
  df <- data.frame(c_fullname = ont, c_visualattributes = "LA", stringsAsFactors = F)

# Add the folders by 'deconstructing' the paths
  while (any(stringr::str_detect(ont, "\\\\")))
  {
    ont <- ont %>% stringr::str_replace("\\\\[^\\\\]+$", "") %>% unique
    df <- df %>%
      dplyr::bind_rows(data.frame(c_fullname = ont, c_visualattributes = "FA", stringsAsFactors = F))
  }

  df %>%
# Discard duplicated paths
    distinct() %>%
# Insert the name of the ontology at the root
    mutate(c_fullname = stringr::str_c("\\", name, "\\", c_fullname)) %>%
    dplyr::bind_rows(data.frame(c_fullname = stringr::str_c("\\", name), c_visualattributes = "CA")) %>%
# Populate the other columns
    mutate(c_hlevel = stringr::str_count(c_fullname, "\\\\") - 1,
           c_name = stringr::str_extract(c_fullname, "[^\\\\]+$"),
           c_basecode = stringr::str_c(scheme, ":", c_name %>% stringr::str_extract("^.+? ") %>% stringr::str_trim()),
           c_basecode = ifelse(is.na(c_basecode), "", c_basecode),
           c_synonym_cd = "N",
           c_facttablecolumn = "concept_cd",
           c_tablename = "concept_dimension",
           c_columnname = "concept_path",
           c_columndatatype = "T",
           c_operator = "LIKE",
           c_tooltip = c_name,
           m_applied_path = "@",
           c_fullname = stringr::str_c(c_fullname, "\\"),
# Use only codes to build shorter paths
           c_fullname = stringr::str_replace_all(c_fullname, "\\\\(.+?) [^\\\\]+", "\\\\\\1"),
           c_dimcode = c_fullname,
           update_date = format(Sys.Date(), "%d/%m/%Y")) -> df

# Push the dataframe into the new ontology table
  columns <- stringr::str_c(names(df), collapse = ",")
  total <- nrow(df)
  current <- 0
  df %>%
    apply(1, function(oneline)
          {
            RPostgreSQL::dbGetQuery(metadata, stringr::str_c("INSERT INTO ", scheme, " (", columns, ") VALUES (", oneline %>% str_c("'", ., "'", collapse = ","), ");"))
            current <<- current + 1
            print(stringr::str_c(current, " / ", total))
          })

  data.frame(modi = modi, stringsAsFactors = F) %>%
    mutate(c_hlevel = 1,
           c_name = modi %>% stringr::str_extract(" .*$") %>% stringr::str_trim(),
           c_fullname = stringr::str_c("\\", c_name, "\\"),
           c_synonym_cd = "N",
           c_visualattributes = "RA",
           c_basecode = stringr::str_c(scheme, ":", modi %>% stringr::str_extract("^.+? ") %>% stringr::str_trim()),
           c_facttablecolumn = "modifier_cd",
           c_tablename = "modifier_dimension",
           c_columnname = "modifier_path",
           c_columndatatype = "T",
           c_operator = "LIKE",
           c_tooltip = c_name,
           c_dimcode = c_fullname,
           m_applied_path = stringr::str_c("\\", name, "\\%"),
           update_date = format(Sys.Date(), "%d/%m/%Y")) %>%
  select(-modi) -> df

  columns <- stringr::str_c(names(df), collapse = ",")
  total <- nrow(df)
  current <- 0
  df %>%
    apply(1, function(oneline)
          {
            RPostgreSQL::dbGetQuery(metadata, stringr::str_c("INSERT INTO ", scheme, " (", columns, ") VALUES (", oneline %>% str_c("'", ., "'", collapse = ","), ");"))
            current <<- current + 1
            print(stringr::str_c(current, " / ", total))
          })

  RPostgreSQL::dbDisconnect(metadata)
}
