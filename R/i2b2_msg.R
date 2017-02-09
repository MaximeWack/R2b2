#' Create base XML message
#'
#' Create the base XML message
#'
#' Creates the base XML message as an R list
#'
#' @return The base msg list object
#' @export
base_msg <- function()
{
  msg <- list()
  msg$i2b2_request <- list()
  attr(msg$i2b2_request, "xmlns:i2b2") <- "http://www.i2b2.org/xsd/hive/msg/1.1/"
  attr(msg$i2b2_request, "xmlns:pm") <- "http://www.i2b2.org/xsd/cell/pm/1.1/"

  msg
}

#' Add the header to the message
#'
#' Add the header to the XML message
#'
#' Add the header to the XML base message created by base_msg
#' base_msg can be piped into add_header
#'
#' @param msg The XML message to add the header to
#' @param domain The name of the domain to interact with
#' @param username The username to connect with
#' @param password The password for the user
#' @return The XML message list object
#' @export
add_header <- function(msg, domain, username, password)
{
  mh <- list()
  mh$i2b2_version_compatible <- list("1.1")
  mh$hl7_version_compatible <- list("2.4")
  mh$sending_application$application_name <- list("R2b2")
  mh$sending_application$application_version <- list("0.0.9000")
  mh$sending_facility$facility_name <- list("R")
  mh$receiving_application$application_name <- list("R2b2")
  mh$receiving_application$application_version <- list("0.0.9000")
  mh$receiving_facility$facility_name <- list("R")
  mh$datetime_of_message <- list(format(Sys.time(), "%FT%T%z"))
  mh$security$domain <- list(domain)
  mh$security$username <- list(username)
  mh$security$password <- list(password)
  mh$message_control_id$message_num  <- list()
  mh$message_control_id$instance_num  <- list("0")
  mh$processing_id$processing_id  <- list("P")
  mh$processing_id$processing_mode  <- list("I")
  mh$accept_acknowledgement_type <- list("AL")
  mh$application_acknowledgement_type <- list("AL")
  mh$country_code <- list("US")
  mh$project_id <- list("@")

  rh <- list()
  rh$result_waittime_ms <- list("180000")

  msg$i2b2_request$message_header <- mh
  msg$i2b2_request$request_header <- rh

  msg
}

#' Add the body to the message
#'
#' Add the body to the XML message
#'
#' Add the body to the XML message created by base_msg and passed through add_header
#' base_msg can be piped into add_header and then into add_body to build a message
#'
#' @param msg The XML message to add the body to
#' @param service The service to request in the body message
#' @param attrib A list of XML attributes to add to the service tag
#' @param ... Optionnaly named tags to add inside the body, with their value
#' @return The XML message list object
#' @export
add_body <- function(msg, service, ..., attrib = NULL)
{
# Create param nodes
  params <- list(...) %>% purrr::map(list)

  mb <- list()
  mb[[service]] <- params

# Set attributes
  if(!is.null(attrib))
  {
    names(attrib) %>%
      purrr::map2(attrib, function(name, attrib) {attr(mb[[service]], name) <<- attrib})
  }

  msg$i2b2_request$message_body <- mb

  msg
}

#' Send the message
#'
#' Send the XML message to an i2b2 cell
#'
#' Send the XML message built by base_msg %>% add_header %>% add_body
#' to the specified cellurl
#'
#' @param msg The XML message as an R list
#' @param cellurl The URL of the i2b2 cell to communicate with
#' @return The XML return message as an httr::content() object
#' @export
send_msg <- function(msg, cellurl)
{
# Correct the base tag
  request <- msg %>%
    xml2::as_xml_document() %>% 
    as.character %>%
    stringr::str_replace_all("i2b2_request", "i2b2:request")

  httr::POST(cellurl, body = request, httr::content_type("text/xml")) %>%
    httr::content()
}
