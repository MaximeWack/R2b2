header <- function(domain, username, password)
{
str_c("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
"<i2b2:request xmlns:i2b2=\"http://www.i2b2.org/xsd/hive/msg/1.1/\" xmlns:pm=\"http://www.i2b2.org/xsd/cell/pm/1.1/\">",
    "<message_header>",
        "<i2b2_version_compatible>1.1</i2b2_version_compatible>",
        "<hl7_version_compatible>2.4</hl7_version_compatible>",
        "<sending_application>",
            "<application_name>R2b2</application_name>",
            "<application_version>0.0.9000</application_version>",
        "</sending_application>",
        "<sending_facility>",
            "<facility_name>R</facility_name>",
        "</sending_facility>",
        "<receiving_application>",
            "<application_name>R2b2</application_name>",
            "<application_version>0.0.9000</application_version>",
        "</receiving_application>",
        "<receiving_facility>",
            "<facility_name>R</facility_name>",
        "</receiving_facility>",
        "<datetime_of_message>", format(Sys.time(), "%FT%T%z"), "</datetime_of_message>",
    "<security>",
      "<domain>", domain, "</domain>",
      "<username>", username, "</username>",
      "<password>", password, "</password>",
    "</security>",
        "<message_control_id>",
            "<message_num></message_num>",
            "<instance_num>0</instance_num>",
        "</message_control_id>",
        "<processing_id>",
            "<processing_id>P</processing_id>",
            "<processing_mode>I</processing_mode>",
        "</processing_id>",
        "<accept_acknowledgement_type>AL</accept_acknowledgement_type>",
        "<application_acknowledgement_type>AL</application_acknowledgement_type>",
        "<country_code>US</country_code>",
        "<project_id>@</project_id>",
    "</message_header>",
    "<request_header>",
        "<result_waittime_ms>180000</result_waittime_ms>",
    "</request_header>")
}

body <- function(service, ...)
{
  params <- list(...)
  if (is.null(names(params)))
    names(params) <- rep("", length(params))
  params <- ifelse(length(params) == 0, "",
         params %>%
           map2_chr(names(params), function(param, name)
                   {
                     ifelse(name == "", param,
                     str_c("<", name, ">", param, "</", name, ">"))
                   }) %>%
         str_c(collapse = ""))
  str_c("<message_body>",
        "<", service, ">",
        params,
        "</", service, ">",
        "</message_body>",
        "</i2b2:request>")
}

i2b2msg <- function(cellurl, msg_header, msg_body)
{
  request <- str_c(msg_header, msg_body)
  system(str_c("curl -H \"Content-Type: text/xml\" -X POST -d '", request , "' ", cellurl), intern = T) %>%
    str_c(collapse = "") %>%
    read_xml %>%
    xml_node("message_body")
}

