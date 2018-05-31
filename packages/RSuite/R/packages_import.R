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
#' It will help you with environment separation per project, dependency
#' management, local packages creation and preparing deployment packs
#' for you solutions.
#'
#' @section Project management:
#' \describe{
#'   \item{\code{\link{prj_start}}}{Creates project structure at specified path.}
#'   \item{\code{\link{prj_start_package}}}{Creates package structure inside project.}
#'   \item{\code{\link{prj_install_deps}}}{Installs project dependencies and needed supportive packages.}
#'   \item{\code{\link{prj_clean_deps}}}{Uninstalls unused packages from project local environment.}
#'   \item{\code{\link{prj_build}}}{Builds project internal packages and installs them.}
#'   \item{\code{\link{prj_zip}}}{Prepares deployment zip tagged with version.}
#'   \item{\code{\link{prj_pack}}}{Prepares project source pack tagged with version.}
#'   \item{\code{\link{prj_lock_env}}}{Locks the project environment.}
#' }
#'
#' @section Project access/loading/unloding:
#' \describe{
#'   \item{\code{\link{prj_init}}}{Loads project settings without loading it into environment.}
#'   \item{\code{\link{prj_load}}}{Loads project into environment so all master scripts can run.}
#'   \item{\code{\link{prj_unload}}}{Unloads last loaded project.}
#' }
#'
#' @section PKGZIP building:
#' \describe{
#'   \item{\code{\link{pkgzip_build_prj_packages}}}{Builds PKGZIP out of project packages.}
#'   \item{\code{\link{pkgzip_build_package_files}}}{Builds PKGZIP out of passed package files.}
#'   \item{\code{\link{pkgzip_build_ext_packages}}}{Builds PKGZIP out of passed external packages.}
#'   \item{\code{\link{pkgzip_build_github_package}}}{Builds PKGZIP out of package on GitHub.}
#' }
#'
#' @docType package
#' @name RSuite
NULL
