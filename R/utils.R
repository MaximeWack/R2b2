#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom dplyr %>%
#' @export
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
  options(scipen = 999)

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

#' Update a dataframe into a database table
#'
#' @param df Dataframe to update into the database
#' @param con Database connection
#' @param table Table in the database in which to push the dataframe
#' @param PK Character vector of the primary key(s)
dbUpdate <- function(df, con, table, PK)
{
  options(scipen = 999)

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
clear_table <- function(db, table, host = "", admin = "", pass = "")
{
  con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = db, user = admin, password = pass)

  RPostgreSQL::dbGetQuery(con, stringr::str_c("DELETE FROM ", table, ";"))

  RPostgreSQL::dbDisconnect(con)
}

#' Upserta dataframe into a database table
#'
#' @param df Dataframe to upsert into the database
#' @param con Database connection
#' @param table Table in the database in which to push the dataframe
#' @param PK Character vector of the primary key(s)
dbUpsert <- function(df, con, table, PK)
{
  columns <- setdiff(names(df), PK)

  write.csv(df, file = "/tmp/data.csv", row.names = F, na = "")

  temp <- str_c(table, "_tmp")

# Create a temp table
  stringr::str_c("CREATE TEMP TABLE ", temp, " (LIKE ", table, ");") %>%
  RPostgreSQL::dbGetQuery(conn = con, .)

# Load data in the temp table
  stringr::str_c("COPY ", temp, " (", names(df) %>% str_c(collapse = ","), ") FROM '/tmp/data.csv' WITH CSV HEADER;") %>%
  RPostgreSQL::dbGetQuery(conn = con, .)

# Update existing rows
  stringr::str_c("UPDATE", table,
                 "SET", str_c(columns, "=", temp, ".", columns, collapse = ","),
                 "FROM", temp,
                 "WHERE", str_c(table, ".", PK, "=", temp, ".", PK, collapse = " AND "),
                 ";", sep = " ") %>%
  RPostgreSQL::dbGetQuery(conn = con, .)

# Insert new rows
  stringr::str_c("INSERT INTO", table,
                 "SELECT", str_c(temp, ".*"),
                 "FROM", temp,
                 "LEFT OUTER JOIN", table,
                 "ON (", str_c(table, ".", PK, "=", temp, ".", PK, collapse = " AND "), ")",
                 "WHERE", str_c(table, ".", PK, " IS NULL", collapse = " AND "),
                 ";", sep = " ") %>%
  RPostgreSQL::dbGetQuery(conn = con, .)

# Delete the temp table and file
  stringr::str_c("DROP TABLE", temp, ";", sep = " ") %>%
  RPostgreSQL::dbGetQuery(conn = con, .)

  unlink("/tmp/data.csv")
}
