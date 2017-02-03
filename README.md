# R2b2

Manage an i2b2 instance installed from the VM from i2b2.org.  

Tested on i2b2 VM 1.7.08b

----

All the steps up to and including **Create and admin user** are to be carried as root, inside the VM.  

----

# Prerequisites

Download and install the VM image from i2b2.org and run it inside VMWare.

`yum install epel-release && yum update` to use the epel repo and update the system.

`yum install R openssl-devel libxml2-devel postgresql-devel postgresql91-devel libcurl-devel` to install R and necessary dependencies.

Restart the machine.

# Installation

Install dependencies in R:  
`install.packages(c("tidyverse", "RPostgreSQL", "devtools"))`

Install with `devtools::install_github("maximewack/R2b2")`  

# Usage

## Set permissions

The `set_permissions` function sets group owning, setgid and group rights +rwx to the webclient and wildfly folders so that the admin(s) can manage them.

## Create an admin user

By default, `create_admin` creates a **i2b2admin** system user, with a random generated password containing numbers and letters of all cases.  
You can set a preferred username as the first argument (*name*), an arbitrary password (*pass*), or set the desired password length (*pass_length*).

The user is created for the system, with membership to the *wildfly* group.  
An user with the same name, and their corresponding database are also created in PostgreSQL, with the generated or given password.

You are still responsible for managing the root account and its password (default = demouser), and optionally  granting additional privileges to the admin user (such as *sudo* rights).

----

The subsequent steps up to and including **set domain** can be executed using the created admin system user, inside the VM.  

----

## Securing the database

This step is only necessary if you want to access the database remotely. Otherwise, the default settings are secure enough.

All the databases have the default *demouser* password.  
`secure_db` takes a database admin credentials (*name* and *pass*) and a password length (*pass_length*) to generate random passwords for the i2b2{demodata, hive, imdata, metadata, pm, workdata} databases. Those passwords are updated in the {crc, im, ont, pm, work}-ds.xml files in $JBOSS_HOME/standalone/deployments/.

You are still responsible for managing the access rights to the PostgreSQL database (listening addresses in $PSQL_HOME/data/postgresql.conf and access methods in $PSQL_HOME/data/pg_hba.conf) if you want to be able to access it remotely.

## Set the domain

`set_domain` takes a database admin credentials (name and pass) and a *domain_id* (the internal id) and *domain_name* (the name of the host on the webclient login page) and sets the corresponding columns in i2b2hive and i2b2pm, and configures the webclient to connect to this domain.

----

The next steps can be carried with another user, as long as you provide the necessary credentials (database admin account), and even from a different machine (you'll have to configure the **pg_hba.conf** and **postgresql.conf** files accordingly)

----
