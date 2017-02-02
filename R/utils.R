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
