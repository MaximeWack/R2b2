#' Add an user
#'
#' Add an i2b2 user to the instance
#'
#' @param domain The name of the domain to interact with
#' @param admin The username to connect with
#' @param pass The password for the user
#' @param id The id (no spaces, unique) of the new user
#' @param name The full name of the new user
#' @param email The email of the new user
#' @param password The password for the new user
#' @param url The URL of the i2b2 cell to communicate with
#' @return The XML return message as an httr::content() object
#' @export
add_user <- function(domain, admin, pass, id, name, email, password, url = "http://127.0.0.1:9090/i2b2/services/PMService/getServices")
{
  base_msg() %>%
    add_header(domain, admin, pass) %>%
    add_body("pm:set_user", user_name = id, full_name = name, email = email, password = password, is_admin = 0) %>%
    send_msg(url)
}

#' Add user roles
#'
#' Add roles to an i2b2 user
#'
#' @param domain The name of the domain to interact with
#' @param admin The username to connect with
#' @param pass The password for the user
#' @param id The id (no spaces, unique) of the new user
#' @param project The project to add the user role to
#' @param roles A character vector of roles to add
#' @param url The URL of the i2b2 cell to communicate with
#' @return The XML return message as an httr::content() object
#' @export
add_user_roles <- function(domain, admin, pass, id, project, roles, url = "http://127.0.0.1:9090/i2b2/services/PMService/getServices")
{
  roles %>%
    purrr::walk(function(role)
                {
                  base_msg() %>%
                    add_header(domain, admin, pass) %>%
                    add_body("pm:set_role", user_name = id, role = role, project_id = project) %>%
                    send_msg(url)
                })
}

#' Add multiple users
#'
#' Add users in bulk from a dataframe
#'
#' Add users in bulk from a dataframe containing the following columns:
#' * id: The unique user id to add for each user
#' * name: The name for each user
#' * email: The email address for each user
#' * password: The password for each user
#' * role: The role to give the user
#'
#' Any role can be given from the ones defined
#' (ADMIN, MANAGER, USER, DATA_PROT, DATA_DEID, DATA_LDS, DATA_AGG, DATA_OBFSC)
#' ADMIN gives all roles for the project, and ADMIN role to project @
#' MANAGER gives all roles down from DATA_DEID for the project
#' USER gives USER and DATA_OBFSC roles for the project
#' Any DATA_* role gives USER and roles down from the DATA_* role given to the project
#'
#' @param domain The name of the domain to interact with
#' @param admin The username to connect with
#' @param pass The password for the user
#' @param users The dataframe containing the users to add
#' @param url The URL of the i2b2 cell to communicate with
#' @return The XML return message as an httr::content() object
#' @export
add_users <- function(domain, admin, pass, users, url = "http://127.0.0.1:9090/i2b2/services/PMService/getServices")
{
  apply(users, 1, function(user)
        {
          add_user(domain, admin, pass, user["id"], user["name"], user["email"], user["password"], url)

          if (user["role"] == "ADMIN")
          {
            roles <- c("MANAGER", "USER", "DATA_PROT", "DATA_DEID", "DATA_LDS", "DATA_AGG", "DATA_OBFSC")
            add_user_roles(domain, admin, pass, user["id"], "@", "ADMIN", url)
          }
          else if (user["role"] == "MANAGER")
            roles <- c("MANAGER", "USER", "DATA_DEID", "DATA_LDS", "DATA_AGG", "DATA_OBFSC")
          else if (user["role"] == "USER")
            roles <- c("USER", "DATA_OBFSC")
          else if (user["role"] == "DATA_PROT")
            roles <- c("USER", "DATA_PROT", "DATA_DEID", "DATA_LDS", "DATA_AGG", "DATA_OBFSC")
          else if (user["role"] == "DATA_DEID")
            roles <- c("USER", "DATA_DEID", "DATA_LDS", "DATA_AGG", "DATA_OBFSC")
          else if (user["role"] == "DATA_LDS")
            roles <- c("USER", "DATA_LDS", "DATA_AGG", "DATA_OBFSC")
          else if (user["role"] == "DATA_AGG")
            roles <- c("USER", "DATA_AGG", "DATA_OBFSC")
          else if (user["role"] == "DATA_OBFSC")
            roles <- c("USER", "DATA_OBFSC")

          add_user_roles(domain, admin, pass, user["id"], user["project"], roles, url)
        })
}

#' Delete users
#'
#' Delete i2b2 users from the instance
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the database admin
#' @param users A character vector of user ids
#' @export
delete_users <- function(host = "127.0.0.1", admin, pass, users)
{
  pm   <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2pm",   user = admin, password = pass)

  user <- users %>% stringr::str_c(collapse = "','") %>% stringr::str_c("'", . ,"'")
  RPostgreSQL::dbGetQuery(pm, stringr::str_c("DELETE FROM pm_user_data WHERE (user_id IN (", user, "));"))
  RPostgreSQL::dbGetQuery(pm, stringr::str_c("DELETE FROM pm_project_user_roles WHERE (user_id IN (", user, "));"))

  RPostgreSQL::dbDisconnect(pm)
}

#' List users
#'
#' Delete i2b2 users from the instance
#'
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the database admin
#' @export
list_users <- function(host = "", admin = "", pass = "")
{
  src_postgres("i2b2pm", host = host, user = admin, pass = pass) %>%
    tbl("pm_user_data") %>%
    select(user_id, full_name, email, project_path) %>%
    collect(n = Inf)
}

#' List user roles
#'
#' Delete i2b2 users from the instance
#'
#' @param user An user id
#' @param host The host to connect to
#' @param admin The admin account for the PostgreSQL database
#' @param pass The password for the database admin
#' @export
list_user_roles <- function(user, host = "", admin = "", pass = "")
{
  src_postgres("i2b2pm", host = host, user = admin, pass = pass) %>%
    tbl("pm_project_user_roles") %>%
    filter(user_id == user) %>%
    select(project_id, user_id, user_role_cd) %>%
    collect(n = Inf)
}
