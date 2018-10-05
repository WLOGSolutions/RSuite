#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Global package definitions and imports
#----------------------------------------------------------------------------

#' @import logging
#' @import devtools
#' @import roxygen2
#' @import git2r
#' @import jsonlite
#' @import processx
#' @importFrom stats aggregate
#' @importFrom httr http_error
NULL

#' Supports Developing, Building and Deploying R Solutions.
#'
#' Supports safe and reproducible solutions development in R.\cr
#' \cr
#' It will help you with environment separation per project, dependency
#' management, local packages creation and preparing deployment packs
#' for your solutions.
#'
#' @section Package options:
#' RSuite uses the following \code{\link{options}} to configure behavior:
#' \itemize{
#'    \item \code{rsuite.user_templ_path}: path to folder containing user customized templates. If not set
#'       (which is default) no user custom templates can be used.
#'    \item \code{rsuite.cache_path}: path to RSuite's cache folder to store downloaded packages for
#'       later usage and content index of used repositories. If not set (which is default) no caching
#'       will be performed.
#' }
#'
#' @section Project management:
#' These functions will help you start a new RSuite project or package inside
#' it, detect and install dependencies into the local environment, build your
#' project packages and prepare deployment zip when you are done with the development.
#'
#' \describe{
#'   \item{\code{\link{prj_start}}}{Creates project structure at the specified path.}
#'   \item{\code{\link{prj_start_package}}}{Creates package structure inside a project.}
#'   \item{\code{\link{prj_install_deps}}}{Installs project dependencies and needed supportive packages.}
#'   \item{\code{\link{prj_clean_deps}}}{Uninstalls unused packages from project local environment.}
#'   \item{\code{\link{prj_build}}}{Builds project internal packages and installs them.}
#'   \item{\code{\link{prj_zip}}}{Prepares deployment zip tagged with version.}
#'   \item{\code{\link{prj_pack}}}{Prepares project source pack tagged with version.}
#'   \item{\code{\link{prj_lock_env}}}{Locks the project environment.}
#'   \item{\code{\link{prj_unlock_env}}}{Unlocks the project environment.}
#' }
#'
#' @section Repository management:
#' These functions make you able to manage package repositories. This RSuite
#' built-in repository manager allows you to manage S3 based and local (in folder)
#' repositories.
#'
#' \describe{
#'   \item{\code{\link{repo_mng_start}}}{Starts management over the repository.}
#'   \item{\code{\link{repo_mng_init}}}{Initializes a repository (creates its structure).}
#'   \item{\code{\link{repo_mng_stop}}}{Stops management over the repository.}
#'   \item{\code{\link{repo_mng_list}}}{Retrieves the list of packages available in the repository.}
#'   \item{\code{\link{repo_mng_remove}}}{Removes packages from the repository.}
#'   \item{\code{\link{repo_upload_prj_packages}}}{Builds and uploads project package(s) into the repository.}
#'   \item{\code{\link{repo_upload_package_files}}}{Uploads package file(s) into a managed repository.}
#'   \item{\code{\link{repo_upload_ext_packages}}}{Uploads external packages into a managed repository.}
#'   \item{\code{\link{repo_upload_pkgzip}}}{Uploads PKGZIP into a managed repository.}
#'   \item{\code{\link{repo_upload_github_package}}}{Loads package from a GitHub repository.}
#'   \item{\code{\link{repo_upload_bioc_package}}}{Loads package from a BioConductor repository.}
#' }
#'
#' @section PKGZIP building:
#' PKGZIPs are for management of repositories in an internet-less environment.
#' There is often no internet access on corporate servers. In that case, you
#' can prepare a PKGZIP with required packages somewhere with an internet connection
#' and use it to update an internal CRAN-like repository which has no access to the internet.
#'
#' \describe{
#'   \item{\code{\link{pkgzip_build_prj_packages}}}{Builds PKGZIP out of project packages.}
#'   \item{\code{\link{pkgzip_build_package_files}}}{Builds PKGZIP out of passed package files.}
#'   \item{\code{\link{pkgzip_build_ext_packages}}}{Builds PKGZIP out of passed external packages.}
#'   \item{\code{\link{pkgzip_build_github_package}}}{Builds PKGZIP out of a package on GitHub.}
#'   \item{\code{\link{pkgzip_build_bioc_package}}}{Builds PKGZIP out of a package on BioConductor.}
#' }
#'
#' @section RSuite miscellaneous:
#'
#' \describe{
#'   \item{\code{\link{rsuite_check_version}}}{Checks if a newer version of RSuite is available.}
#'   \item{\code{\link{rsuite_update}}}{Updates RSuite to the newest available version}
#'   \item{\code{\link{rsuite_register_repo_adapter}}}{Registers repository adapter to use for projects.}
#'   \item{\code{\link{rsuite_get_repo_adapter_names}}}{Gets all names of known repository adapters.}
#'   \item{\code{\link{rsuite_register_rc_adapter}}}{Registers RC (revision control) adapter to use for projects.}
#'   \item{\code{\link{rsuite_unregister_rc_adapter}}}{Unregisters RC (revision control) adapter.}
#'   \item{\code{\link{rsuite_get_rc_adapter_names}}}{Gets all names of known RC (revision control) adapters.}
#'   \item{\code{\link{rsuite_getLogger}}}{Retrieves RSuite logger.}
#'   \item{\code{\link{rsuite_get_os_info}}}{Retrieves information on current OS.}
#' }
#'
#' @section Template management:
#' These functions will help you to manage RSuite templates. They allow
#' you to create a project and package templates, register them in the
#' local or global template directory and list all registered templates.
#'
#' \describe{
#'   \item{\code{\link{tmpl_start}}}{Creates a new template.}
#'   \item{\code{\link{tmpl_list_registered}}}{Lists all registered templates}
#'   \item{\code{\link{tmpl_register}}}{Registers a template.}
#' }
#'
#' @section System requirements:
#' Some packages have special system requirements declared. E.g. XML package on
#' Linuxes requires the libxml2 system library to be installed. Such requirements
#' are specified in free form in the SystemRequirements field in the package
#' DESCRIPTION. The team from R Consortium (\url{https://www.r-consortium.org/})
#' performed a great job with collecting sysreqs database. As RSuite is supposed
#' to work also in a connection-less environment the database they created is
#' included in RSuite.
#'
#' These functions extract system requirements for the whole project environment
#' and make it possible to prepare installation scripts or update your system
#' if you have privileged access.
#'
#' \describe{
#'   \item{\code{\link{sysreqs_collect}}}{Prints out all system requirements from dependencies and project packages.}
#'   \item{\code{\link{sysreqs_check}}}{Checks for system requirements availability.}
#'   \item{\code{\link{sysreqs_install}}}{Updates the system to satisfy detected requirements.}
#'   \item{\code{\link{sysreqs_script}}}{Creates a script to update a system to satisfy project requirements.}
#' }
#'
#' @section Extending RSuite - RC adapter:
#' This API allows you to implement your own RC (revision control) adapter for
#' RSuite.
#'
#' RSuite has SVN and Git adapters built-in for you.
#'
#' After you developed your very own RC adapter you can register it in RSuite
#' with the \code{\link{rsuite_register_rc_adapter}} function.
#'
#' \describe{
#'   \item{\code{\link{rc_adapter_create_base}}}{
#'      Creates a base presentation for RC adapter to use by concrete implementations.
#'   }
#'   \item{\code{\link{rc_adapter_is_under_control}}}{Detects if directory is under adapter's managed version control.}
#'   \item{\code{\link{rc_adapter_prj_struct_add}}}{Puts project structure under RC adapter's managed version control.}
#'   \item{\code{\link{rc_adapter_pkg_struct_add}}}{Puts package structure under RC adapter's managed version control.}
#'   \item{\code{\link{rc_adapter_get_version}}}{
#'      Retrieves current RC version number for working copy at the passed directory.
#'   }
#'   \item{\code{\link{rc_adapter_remove_admins}}}{
#'      Removes all RC related administrative entries from folder tree at the directory.
#'   }
#' }
#'
#' @section Extending RSuite - Repository adapter and manager:
#' This API allows you to implement your own repository adapter for RSuite.
#' If the repository can be managed (you can add/remove/update packages in it)
#' you can provide a repo manager object creation ability to manage it with
#' RSuite.
#'
#' RSuite has CRAN, MRAN, S3(Amazon S3 bucket base repository), Url(repository
#' under Url) and Dir(local CRAN-like folder) repo adapters and Dir and S3 repo
#' managers built-in for you.
#'
#' After you develop your very own repository adapter you can register it in
#' RSuite with the \code{\link{rsuite_register_repo_adapter}} function.
#'
#' \describe{
#'   \item{\code{\link{repo_adapter_create_base}}}{
#'      Creates the base presentation for the repo adapter to use by concrete implementations.
#'   }
#'   \item{\code{\link{repo_adapter_get_info}}}{Returns information about the repository the adapter is working on.}
#'   \item{\code{\link{rc_adapter_prj_struct_add}}}{Puts the project structure under RC adapter's managed version control.}
#'   \item{\code{\link{repo_adapter_get_path}}}{
#'      Returns adapter path related to project to use for dependencies resolution.
#'   }
#'   \item{\code{\link{repo_adapter_create_manager}}}{
#'      Creates repo manager to manage its repository.
#'   }
#'   \item{\code{\link{repo_manager_get_info}}}{Returns information on repo manager.}
#'   \item{\code{\link{repo_manager_init}}}{Initializes managed repository structure.}
#'   \item{\code{\link{repo_manager_upload}}}{Adds packages to the managed repository.}
#'   \item{\code{\link{repo_manager_remove}}}{Removes specified packages from the repository.}
#'   \item{\code{\link{repo_manager_destroy}}}{Releases resources allocated to manage the repository.}
#' }
#'
#' @section Project access/loading/unloading:
#' You normally will not need to use these functions unless you want to perform some scripting
#' with use of RSuite.
#'
#' \describe{
#'   \item{\code{\link{prj_init}}}{Loads project settings without loading it into the environment.}
#'   \item{\code{\link{prj_load}}}{Loads project into the environment so all master scripts can run.}
#'   \item{\code{\link{prj_unload}}}{Unloads last loaded project.}
#' }
#'
#' @section Project configuration:
#' These functions are a convenient way to change the project global configuration.
#'
#' You normally will not need to use these functions unless you want to perform some scripting
#' with use of RSuite.
#'
#' \describe{
#'   \item{\code{\link{prj_config_set_repo_adapters}}}{
#'      Updates the project configuration to use only specified repository adapters.
#'   }
#'   \item{\code{\link{prj_config_set_rversion}}}{Updates the project configuration to use specified R Version.}
#' }
#'
#' @docType package
#' @name RSuite
NULL
