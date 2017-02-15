#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
NULL

#' Generate a random password
#'
#' Generate a random alphanumeric password of any length
#'
#' Generate a random alphanumeric ([A-Za-z0-9]) password
#'
#' @param length Length of the desired password
#' @return A password of length length
create_password <- function(length = 8)
{
  stringr::str_c(sample(c(LETTERS, letters, 0:9), length), collapse = "")
}

#' Push a dataframe into a database table
#'
#' @param con Database connection
#' @param table Table in the database in which to push the dataframe
#' @param df Dataframe to push into the database
dbPush <- function(con, table, df)
{
    columns <- stringr::str_c(names(df), collapse = ",")
    df %>%
      apply(1, function(oneline)
            {
              oneline[is.na(oneline)] <- "NULL"
              oneline %>%
                stringr::str_c("'", ., "'", collapse = ",") %>% 
                stringr::str_c("(", ., ")") %>%
                stringr::str_replace("'NULL'", "NULL")
            }) %>%
    stringr::str_c(collapse = ",") %>%
    stringr::str_c("INSERT INTO ", table, " (", columns, ") VALUES ", ., ";") %>%
    RPostgreSQL::dbGetQuery(conn = con, .)
}
