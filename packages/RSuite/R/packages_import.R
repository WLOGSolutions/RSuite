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
#' @import subprocess
#' @importFrom stats aggregate
NULL

#' Supports Developing, Building and Deploying R Solution.
#'
#' The main package purpose is to support safe and supportable
#' solutions development in R.\cr
#' \cr
#' It will help you with environment separation per project, dependency
#' management, local packages creation and preparing deployment packs
#' for you solutions.
#'
#' @section Project management:
#' These functions will help you start new RSuite project or package inside
#' it, detect and install dependencies into local environemnt, build you
#' project packages and prepare deployment zip then you are done with development.
#'
#' \describe{
#'   \item{\code{\link{prj_start}}}{Creates project structure at specified path.}
#'   \item{\code{\link{prj_start_package}}}{Creates package structure inside project.}
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
#' built-in repository managers you can manage S3 based and local (in folder)
#' repositories.
#'
#' \describe{
#'   \item{\code{\link{repo_mng_start}}}{Starts managment over repository.}
#'   \item{\code{\link{repo_mng_init}}}{Initializes repository (creates its structure).}
#'   \item{\code{\link{repo_mng_stop}}}{Stops managment over repository.}
#'   \item{\code{\link{repo_mng_list}}}{Retrieve list of packages available in repository.}
#'   \item{\code{\link{repo_mng_remove}}}{Removes packages from repository.}
#'   \item{\code{\link{repo_upload_prj_packages}}}{Builds and uploads project package(s) into repository.}
#'   \item{\code{\link{repo_upload_package_files}}}{Uploads package file(s) into managed repository.}
#'   \item{\code{\link{repo_upload_ext_packages}}}{Uploads external packages into managed repository.}
#'   \item{\code{\link{repo_upload_pkgzip}}}{Uploads PKGZIP into managed repository.}
#'   \item{\code{\link{repo_upload_github_package}}}{Loads package from github repository.}
#' }
#'
#' @section PKGZIP building:
#' PKGZIPs are for management of repositories in internet-less environment.
#' On corporate servers often you do not have access to internet. In that
#' case you can prepare PKGZIP with required packages somethere with internet
#' connection and use it to update internal CRAN-like repository which has no
#' access to internet.
#'
#' \describe{
#'   \item{\code{\link{pkgzip_build_prj_packages}}}{Builds PKGZIP out of project packages.}
#'   \item{\code{\link{pkgzip_build_package_files}}}{Builds PKGZIP out of passed package files.}
#'   \item{\code{\link{pkgzip_build_ext_packages}}}{Builds PKGZIP out of passed external packages.}
#'   \item{\code{\link{pkgzip_build_github_package}}}{Builds PKGZIP out of package on GitHub.}
#' }
#'
#' @section RSuite miscellaneous:
#'
#' \describe{
#'   \item{\code{\link{rsuite_check_version}}}{Checks if newer version of RSuite is available.}
#'   \item{\code{\link{rsuite_update}}}{Updates RSuite to newest available version}
#'   \item{\code{\link{rsuite_register_repo_adapter}}}{Registers repository adapter to use for projects.}
#'   \item{\code{\link{rsuite_get_repo_adapter_names}}}{Gets all names of known repository adapters.}
#'   \item{\code{\link{rsuite_register_rc_adapter}}}{Registers RC (revision control) adapter to use for projects.}
#'   \item{\code{\link{rsuite_unregister_rc_adapter}}}{Unregisters RC (revision control) adapter.}
#'   \item{\code{\link{rsuite_get_rc_adapter_names}}}{Gets all names of known RC (revision control) adapters.}
#'   \item{\code{\link{rsuite_getLogger}}}{Retrieves RSuite logger.}
#' }
#'
#' @section System requirements:
#' Some packages have special system requirements declared. E.g. XML package on
#' Linuxes require libxml2 system library to be installed. Such requirements
#' are specified in free form in SystemRequirements field in package
#' DESCRIPTION. Team from R Consortium (\url{https://www.r-consortium.org/})
#' performed a great job with collecting sysreqs database. As RSuite is supposed
#' to work also in connection-less environments database they created is
#' included into RSuite.
#'
#' These functions extract system requements for the whole project environment
#' and makes it possible to prepare installation script or update your system
#' if you have priviledged access.
#'
#' \describe{
#'   \item{\code{\link{sysreqs_collect}}}{Prints out all system requirements from dependencies and project packages.}
#'   \item{\code{\link{sysreqs_check}}}{Checks for system requirements availability.}
#'   \item{\code{\link{sysreqs_install}}}{Updates system to satisfy detected requirements.}
#'   \item{\code{\link{sysreqs_script}}}{Creates script to update system to satisfy project requirements.}
#' }
#'
#' @section Extending RSuite - RC adapter:
#' This API alows you to implement your own RC (revision control) adapter for
#' RSuite.
#'
#' RSuite has SVN and Git adapters in-build for you.
#'
#' After you developed your very own RC adapter you can register it in RSuite
#' with \code{\link{rsuite_register_rc_adapter}} function.
#'
#' \describe{
#'   \item{\code{\link{rc_adapter_create_base}}}{
#'      Creates base presentation for RC adapter to use by concrete implementations.
#'   }
#'   \item{\code{\link{rc_adapter_is_under_control}}}{Detects if dir is under adapter's managed versison control.}
#'   \item{\code{\link{rc_adapter_prj_struct_add}}}{Puts project structure under RC adapter's managed version control.}
#'   \item{\code{\link{rc_adapter_pkg_struct_add}}}{Puts package structure under RC adapter's managed version control.}
#'   \item{\code{\link{rc_adapter_get_version}}}{
#'      Retrieves current RC version number for working copy at directory passed.
#'   }
#'   \item{\code{\link{rc_adapter_remove_admins}}}{
#'      Remove all RC related administrative entries from folder tree at dir.
#'   }
#' }
#'
#' @section Extending RSuite - Repository adapter and manager:
#' This API alows you to implement your own repository adapter for RSuite.
#' If the repository can be managed (you can add/remove/update packages in it)
#' you can provide repo manager object creation ability to manage it with
#' RSuite.
#'
#' RSuite has CRAN, MRAN, S3(Amazon S3 bucket base repository), Url(repository
#' under Url) and Dir(local CRAN-like folder) repo adapters and Dir and S3 repo
#' managers in-build for you.
#'
#' After you developed your very own repository adapter you can register it in
#' RSuite with \code{\link{rsuite_register_repo_adapter}} function.
#'
#' \describe{
#'   \item{\code{\link{repo_adapter_create_base}}}{
#'      Creates base presentation for repo adapter to use by concrete implementations.
#'   }
#'   \item{\code{\link{repo_adapter_get_info}}}{Returns informations about repository the adapter is working on.}
#'   \item{\code{\link{rc_adapter_prj_struct_add}}}{Puts project structure under RC adapter's managed version control.}
#'   \item{\code{\link{repo_adapter_get_path}}}{
#'      Returns adapter path related to project to use for dependencies resolution.
#'   }
#'   \item{\code{\link{repo_adapter_create_manager}}}{
#'      Creates repo manager to manage its repository.
#'   }
#'   \item{\code{\link{repo_manager_get_info}}}{Returns informations on repo manager.}
#'   \item{\code{\link{repo_manager_init}}}{Initializes managed repository structure.}
#'   \item{\code{\link{repo_manager_upload}}}{Adds packages to managed repository.}
#'   \item{\code{\link{repo_manager_remove}}}{Removes specified packages from the repository.}
#'   \item{\code{\link{repo_manager_destroy}}}{Releases resources allocated to manage the repository.}
#' }
#'
#' @section Project access/loading/unloding:
#' You normally will not need to use these functions unless you want perform some scripting
#' with use of RSuite.
#'
#' \describe{
#'   \item{\code{\link{prj_init}}}{Loads project settings without loading it into environment.}
#'   \item{\code{\link{prj_load}}}{Loads project into environment so all master scripts can run.}
#'   \item{\code{\link{prj_unload}}}{Unloads last loaded project.}
#' }
#'
#' @section Project configuration:
#' These functions are conviniet way to change project global configuration.
#'
#' You normally will not need to use these functions unless you want perform some scripting
#' with use of RSuite.
#'
#' \describe{
#'   \item{\code{\link{prj_config_set_repo_adapters}}}{
#'      Updates project configuration to use only specified repository adapters.
#'   }
#'   \item{\code{\link{prj_config_set_rversion}}}{Updates project configuration to use specified R Version.}
#' }
#'
#' @docType package
#' @name RSuite
NULL
