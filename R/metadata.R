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
clear_default_metadata <- function(host = "", admin = "", pass = "")
{
  metadata   <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2metadata", user = admin, password = pass)

  # Drop default tables
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE birn;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE custom_meta;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE i2b2;")
  RPostgreSQL::dbGetQuery(metadata, "DROP TABLE icd10_icd9;")

  # Empty schemes and table_acess
  c("schemes", "table_access") %>%
    purrr::walk(~clear_table("i2b2metadata", .x, host, admin, pass))

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
#' @param scheme The scheme to use for this ontology
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
delete_ont<- function(scheme, host = "", admin = "", pass = "")
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
#' @export
add_ont <- function(name, scheme, host = "", admin = "", pass = "")
{
  metadata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2metadata", user = admin, password = pass)

  # Insert the new scheme
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("INSERT INTO schemes VALUES ('", scheme, ":', '", scheme, "');"))

  # Insert the table_acess entry
  RPostgreSQL::dbGetQuery(metadata, stringr::str_c("INSERT INTO table_access VALUES ('", scheme, "', '", scheme, "', 'N', 0, '\\", name, "\\', '", name, "', 'N', 'FA', NULL, NULL, NULL, 'concept_cd', 'concept_dimension', 'concept_path', 'T', 'LIKE', '\\", name, "\\', NULL, NULL, NULL, NULL, NULL, NULL);"))

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
#' ont is a dataframe containing at least the c_fullname column, a character vector containing all the leaves of the ontology
#' with their respective path, in the form
#' code_level1 label_level1\\code_level2 label_level2\\...\\code_leaf label_leaf
#' The function rebuilds the folders automatically
#'
#' modi is a dataframe containing at least the c_fullname column, a character vector containing the modifiers, in the form
#' code_modi label_modi
#' The modifiers apply on all the ontology
#'
#' @param ont The ontology to insert
#' @param modi The modifiers to insert
#' @param name The name of the new ontology
#' @param scheme The scheme to use for this ontology
#' @param include_code Whether to include the code in the label or not
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
populate_ont <- function(ont, modi = NULL, name, scheme, include_code = T, def_facttablecolumn = "concept_cd", def_tablename = "concept_dimension", def_columnname = "concept_path", def_columndatatype = "T", def_operator = "LIKE", host = "", admin = "", pass = "")
{
  metadata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2metadata", user = admin, password = pass)

  # Sanitize the ontology
  ont %>%
    dplyr::mutate_all(~stringr::str_replace_all(., "'", "''")) ->
  ont

  if(! modi %>% is.null)
  {
    modi %>%
      dplyr::mutate_all(~stringr::str_replace_all("'", "''")) ->
    modi
  }

  # Tag explicit folders and root leaves
  ont %>%
    dplyr::mutate(type = dplyr::case_when(c_fullname %>% purrr::map_lgl(~stringr::str_detect(setdiff(c_fullname, .x), stringr::fixed(.x)) %>% any) ~ "folder",
                            c_fullname %>% stringr::str_detect("\\\\") ~ "leaf",
                            T ~ "root_leaf"),
           c_visualattributes = ifelse(type == "folder", "FA", "LA")) ->
  ont

  # Add the folders for orphaned leaves by 'deconstructing' the paths
  ont %>%
    dplyr::filter(! (purrr::map(ont$c_fullname[ont$type == "folder"], ~stringr::str_detect(ont$c_fullname, .x)) %>% purrr::reduce(`|`) %||% F),
           type == "leaf") %>%
    dplyr::pull(c_fullname) ->
  leaves

  while (any(stringr::str_detect(leaves, "\\\\")))
  {
    leaves %>%
      stringr::str_replace("\\\\[^\\\\]+$", "") %>%
      unique ->
    leaves
    ont %>%
      dplyr::add_row(c_fullname = leaves, c_visualattributes = "FA") ->
    ont
  }

  ont %>%
    # Delete temp type variable
    dplyr::select(-type) %>%
    # Discard duplicated paths
    dplyr::distinct() %>%
    # Insert the name of the ontology at the root
    dplyr::mutate(c_fullname = stringr::str_c("\\", name, "\\", c_fullname)) %>%
    # dplyr::bind_rows(data.frame(c_fullname = stringr::str_c("\\", name), c_visualattributes = "FA")) %>%
    dplyr::add_row(c_fullname = stringr::str_c("\\", name), c_visualattributes = "FA") %>%
    # Populate the other columns if they are not given, with a default to concept_dimension
    dplyr::bind_cols(tibble::tibble(c_synonym_cd = rep(NA, nrow(.)),
                            c_facttablecolumn = rep(NA, nrow(.)),
                            c_tablename = rep(NA, nrow(.)),
                            c_columnname = rep(NA, nrow(.)),
                            c_columndatatype = rep(NA, nrow(.)),
                            c_operator = rep(NA, nrow(.)),
                            c_tooltip = rep(NA, nrow(.)),
                            c_dimcode = rep(NA, nrow(.)))) %>%
    dplyr::select(-dplyr::ends_with("1")) %>%
    dplyr::mutate(c_hlevel = stringr::str_count(c_fullname, "\\\\") - 1,
                  c_name = stringr::str_extract(c_fullname, "[^\\\\]+$"),
                  c_basecode = stringr::str_c(scheme, ":", c_name %>% stringr::str_extract("^.+? ") %>% stringr::str_trim()),
                  c_basecode = ifelse(is.na(c_basecode), "", c_basecode),
                  c_synonym_cd = ifelse(is.na(c_synonym_cd), "N", c_synonymcd),
                  c_facttablecolumn = ifelse(is.na(c_facttablecolumn), def_facttablecolumn,c_facttablecolumn),
                  c_tablename = ifelse(is.na(c_tablename), def_tablename, c_tablename),
                  c_columnname = ifelse(is.na(c_columnname), def_columnname, c_columnname),
                  c_columndatatype = ifelse(is.na(c_columndatatype), def_columndatatype, c_columndatatype),
                  c_operator = ifelse(is.na(c_operator), def_operator, c_operator),
                  c_tooltip = ifelse(is.na(c_tooltip), c_name, c_tooltip),
                  m_applied_path = "@",
                  c_fullname = stringr::str_c(c_fullname, "\\"),
                  # Use only codes to build shorter paths
                  c_fullname = stringr::str_replace_all(c_fullname, "\\\\(.+?) [^\\\\]+", "\\\\\\1"),
                  c_dimcode = ifelse(is.na(c_dimcode), c_fullname, c_dimcode),
                  update_date = format(Sys.Date(), "%m/%d/%Y")) ->
  ont

  if (!include_code)
    ont$c_name[ont$c_hlevel > 0] <- ont$c_name[ont$c_hlevel > 0] %>% stringr::str_extract(" .*$") %>% stringr::str_trim()

  # Push the dataframe into the new ontology table
  dbPush(ont, metadata, scheme)

  if (length(modi) > 0)
  {
    modi %>%
      dplyr::mutate(c_hlevel = 1,
                    c_name = c_fullname %>% stringr::str_extract(" .*$") %>% stringr::str_trim(),
                    c_synonym_cd = "N",
                    c_visualattributes = "RA",
                    c_basecode = stringr::str_c(scheme, ":", c_fullname %>% stringr::str_extract("^.+? ") %>% stringr::str_trim()),
                    c_fullname = stringr::str_c("\\", c_name, "\\"),
                    c_facttablecolumn = "modifier_cd",
                    c_tablename = "modifier_dimension",
                    c_columnname = "modifier_path",
                    c_columndatatype = "T",
                    c_operator = "LIKE",
                    c_tooltip = c_name,
                    c_dimcode = c_fullname,
                    m_applied_path = stringr::str_c("\\", name, "\\%"),
                    update_date = format(Sys.Date(), "%m/%d/%Y")) ->
    modi

  # Push the dataframe into the new ontology table
  dbPush(modi, metadata, scheme)
  }


  RPostgreSQL::dbDisconnect(metadata)
}

#' List the available ontologies
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
list_ont <- function(host = "", admin = "", pass = "")
{
  dplyr::src_postgres("i2b2metadata", host = host, user = admin, pass = pass) %>%
    dplyr::tbl("table_access") %>%
    dplyr::collect()
}

#' List the available schemes
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
list_schemes <- function(host = "", admin = "", pass = "")
{
  dplyr::src_postgres("i2b2metadata", host = host, user = admin, pass = pass) %>%
    dplyr::tbl("schemes") %>%
    dplyr::collect()
}

#' Fetch an ontology
#'
#' @param ont The name of the ontology, from the c_table_name column in list_ont()
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
get_ont <- function(ont, host = "", admin = "", pass = "")
{
  dplyr::src_postgres("i2b2metadata", host = host, user = admin, pass = pass) %>%
    dplyr::tbl(stringr::str_to_lower(ont)) %>%
    dplyr::collect()
}
