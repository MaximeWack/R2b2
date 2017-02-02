create_password <- function(length = 8)
{
  str_c(sample(c(LETTERS, letters, 0:9), length), collapse = "")
}
