base_msg <- function()
{
  msg <- list()
  msg$i2b2_request <- list()
  attr(msg$i2b2_request, "xmlns:i2b2") <- "http://www.i2b2.org/xsd/hive/msg/1.1/"
  attr(msg$i2b2_request, "xmlns:pm") <- "http://www.i2b2.org/xsd/cell/pm/1.1/"

  msg
}

header <- function(msg, domain, username, password)
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

body <- function(msg, service, ..., attrib = NULL)
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
