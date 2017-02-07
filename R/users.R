add_user <- function(domain, admin, pass, id, name, email, password, url = "http://localhost:9090/i2b2/services/PMService/getServices")
{
  base_msg() %>%
    header(domain, admin, pass) %>%
    body("pm:set_user", user_name = id, full_name = name, email = email, password = password, is_admin = 0) %>%
    send_msg(url)
}

add_user_roles <- function(domain, admin, pass, id, project, roles, url = "http://localhost:9090/i2b2/services/PMService/getServices")
{
  roles %>%
    purrr::walk(function(role)
                {
                  base_msg() %>%
                    header(domain, admin, pass) %>%
                    body("pm:set_role", user_name = id, role = role, project_id = project) %>%
                    send_msg(url)
                })
}

add_users <- function(domain, admin, pass, users, url = "http://localhost:9090/i2b2/services/PMService/getServices")
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

delete_users <- function(host = "localhost", admin, pass, users)
{
  pm   <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(), host = host, dbname = "i2b2pm",   user = name, password = pass)

  user <- users %>% stringr::str_c(collapse = "','") %>% stringr::str_c("'", . ,"'")
  RPostgreSQL::dbGetQuery(pm, stringr::str_c("DELETE FROM pm_user_data WHERE (user_id IN (", user, "));"))
  RPostgreSQL::dbGetQuery(pm, stringr::str_c("DELETE FROM pm_project_user_roles WHERE (user_id IN (", user, "));"))
}
