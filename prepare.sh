#!/bin/sh

echo "Installing the EPEL repo"

yum install epel-release

echo "Updating system and packages list"

yum upgrade

yum update

echo "Installing system dependancies"

yum install man sudo R git libcurl libcurl-devel libxml2 libxml2-devel openssl openssl-devel postgresql postgresql91 postgresql-devel postgresql91-devel libcurl libcurl-devel

echo "Installing R dependancies"

echo "If this step fails, you might need to configure a proxy ? (http_proxy environment var)"

R -e 'install.packages(c("tidyverse", "devtools"), repos = "https://cran.r-project.org")'

echo "Installing R2b2 using devtools"

echo "If this step fails, you might need to configure a proxy"

echo "Alternatively, you can download a local copy and use devtools::install_local to install from file"

R -e 'devtools::install_github("MaximeWack/R2b2")'
