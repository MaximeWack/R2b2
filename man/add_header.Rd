% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/i2b2_msg.R
\name{add_header}
\alias{add_header}
\title{Add the header to the message}
\usage{
add_header(msg, username, password, domain = "")
}
\arguments{
\item{msg}{The XML message to add the header to}

\item{username}{The username to connect with}

\item{password}{The password for the user}

\item{domain}{The domain to act on}
}
\value{
The XML message list object
}
\description{
Add the header to the XML message
}
\details{
Add the header to the XML base message created by base_msg
base_msg can be piped into add_header
}
