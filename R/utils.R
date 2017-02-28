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
#' @param df Dataframe to push into the database
#' @param con Database connection
#' @param table Table in the database in which to push the dataframe
dbPush <- function(df, con, table)
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

#' Udpdate a dataframe into a database table
#'
#' @param df Dataframe to update into the database
#' @param con Database connection
#' @param table Table in the database in which to push the dataframe
#' @param PK Character vector of the primary key(s)
dbUpdate <- function(df, con, table, PK)
{
    df %>%
      apply(1, function(oneline)
            {
              oneline[is.na(oneline)] <- "NULL"
              stringr::str_c(names(oneline), " = '", oneline, "'", collapse = ",") %>%
              stringr::str_replace("'NULL'", "NULL") -> set

              stringr::str_c(PK, " = '", oneline[PK], "'", collapse = " AND ") -> where

              stringr::str_c("UPDATE ", table, " SET ", set, " WHERE ", where, ";") %>%
              RPostgreSQL::dbGetQuery(conn = con, .)
            })
}

#' Clear a database table
#'
#' Clear a database table
#'
#' @param db Name of the database
#' @param table Name of the table
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass the password for the admin account
#' @export
clear_table <- function(db, table, host, admin, pass)
{
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = db, user = admin, password = pass)

  RPostgreSQL::dbGetQuery(con, stringr::str_c("DELETE FROM ", table, ";"))

  RPostgreSQL::dbDisconnect(con)
}
