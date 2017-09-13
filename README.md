# R2b2

Manage an i2b2 instance installed from the VM from i2b2.org.  

Tested on i2b2 VM 1.7.08b

----

The package is best used locally on the VM.  
All the steps involving file modifications **are to be run locally**, as root.  
This includes the creation/deletion of projects.

----

# Prerequisites

Download and install the VM image from i2b2.org and run it inside VMWare.

# Installation

Run `curl https://raw.githubusercontent.com/MaximeWack/R2b2/master/prepare.sh` to download the installation script, and run it as root.

You might need to configure your internet connection if you are behind a proxy.

This script will add the EPEL repo, update the installed packages and package list on the system, and install system dependancies to run R2b2. It will also install R packages dependancies and install R2b2 from github.

Restart the machine.

# Usage

## Set permissions

The `set_permissions` function sets group owning, setgid and group rights +rwx to the webclient and wildfly folders so that the admin(s) can manage them.

## Create an admin user

By default, `create_admin` creates a **i2b2admin** system user, with a random generated password containing numbers and letters of all cases.  
You can set a preferred username as the first argument (*name*), an arbitrary password (*pass*), or set the desired password length (*pass_length*).

The user is created for the system, with membership to the *wildfly* group.  
An user with the same name, and their corresponding database are also created in PostgreSQL, with the generated or given password.

You are still responsible for managing the root account and its password (default = demouser), and optionally  granting additional privileges to the admin user (such as *sudo* rights).
