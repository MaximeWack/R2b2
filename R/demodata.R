#' Clear the default modifiers
#'
#' Clear the default concepts in concept_dimension
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_modifier <- function(host = "localhost", admin, pass)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "DELETE FROM modifier_dimension;")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Clear the default concepts
#'
#' Clear the default concepts in concept_dimension
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_concept <- function(host = "localhost", admin, pass)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, "DELETE FROM concept_dimension;")

  RPostgreSQL::dbDisconnect(demodata)
}

#' Delete concepts
#'
#' Delete concepts from concept_dimension
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param scheme The scheme to delete from the concepts
#' @export
delete_concept <- function(host = "localhost", admin, pass, scheme)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

  RPostgreSQL::dbGetQuery(demodata, stringr::str_c("DELETE FROM concept_dimension WHERE (concept_cd LIKE '", scheme, ":%';"))

  RPostgreSQL::dbDisconnect(demodata)
}

#' Populate the concept_dimension
#'
#' Populate the concept_dimension with new concepts
#'
#' ont is a character vector containing all the leaves of the ontology
#' with their respective path, in the form
#' code_level1 label_level1/code_level2 label_level2/.../code_leaf label_leaf
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @param ont The ontology to insert
#' @param modi The modifiers to insert
#' @param name The name of the new ontology
#' @param scheme The scheme to use for this ontology
#' @export
populate_concept <- function(host = "localhost", admin, pass, ont, modi, name, scheme)
{
  demodata <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2demodata", user = admin, password = pass)

# Sanitize the ontology
  ont <- ont %>% stringr::str_replace_all("'", "''")
  modi <- modi %>% stringr::str_replace_all("'", "''")

# Create the data frame holding the contents of the new table
  data.frame(concept_path = ont, stringsAsFactors = F) %>%
# Insert the name of the ontology at the root
    mutate(concept_path = stringr::str_c("\\", name, "\\", concept_path)) %>%
# Populate the other columns
    mutate(name_char = stringr::str_extract(concept_path, "[^\\\\]+$"),
           concept_cd = stringr::str_c(scheme, ":", name_char %>% stringr::str_extract("^.+? ") %>% stringr::str_trim()),
           concept_cd = ifelse(is.na(concept_cd), "", concept_cd),
           concept_path = stringr::str_c(concept_path, "\\"),
# Use only codes to build shorter paths
           concept_path = stringr::str_replace_all(concept_path, "\\\\(.+?) [^\\\\]+", "\\\\\\1"),
           update_date = format(Sys.Date(), "%d/%m/%Y")) -> df

# Push the dataframe into the new ontology table
  columns <- stringr::str_c(names(df), collapse = ",")
  total <- nrow(df)
  current <- 0
  df %>%
    apply(1, function(oneline)
          {
            RPostgreSQL::dbGetQuery(demodata, stringr::str_c("INSERT INTO concept_dimension  (", columns, ") VALUES (", oneline %>% str_c("'", ., "'", collapse = ","), ");"))
            current <<- current + 1
            print(stringr::str_c(current, " / ", total))
          })

  data.frame(modi = modi) %>%
    mutate(name_char = modi %>% stringr::str_extract(" .*$") %>% stringr::str_trim(),
           modifier_path = stringr::str_c("\\", c_name, "\\"),
           modifier_cd = stringr::str_c(scheme, ":", modi %>% stringr::str_extract("^.+? ") %>% stringr::str_trim()),
           update_date = format(Sys.Date(), "%d/%m/%Y")) -> df

  RPostgreSQL::dbDisconnect(demodata)
}
