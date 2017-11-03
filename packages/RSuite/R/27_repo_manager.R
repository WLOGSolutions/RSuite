#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Repo manager class API.
#----------------------------------------------------------------------------


#'
#' Returns informations on repo manager.
#'
#' @param repo_manager repo manager object
#'
#' @return named list with following entries:
#' \describe{
#'   \item{types}{Types of packages manager can manage. (type: character)}
#'   \item{rver}{R version repo manager is managing. NA if repo
#'     manager is managing source packages. (type: character)}
#'   \item{url}{Url to repository. (type: character)}
#' }
#'
#' @export
#'
repo_manager_get_info <- function(repo_manager) {
  stopifnot(is_repo_manager(repo_manager))

  UseMethod("repo_manager_get_info")
}

#'
#' Default implementation of repo_manager_get_info.
#'
#' @keywords internal
#'
repo_manager_get_info.default <- function(repo_manager) {
  assert(FALSE,
         "repo_manager_get_info not implemented by %s",
         paste(class(repo_manager), collapse = " "))
}

#'
#' Initializes repository structure to use for uploading and dependency
#' detection.
#'
#' @param repo_manager repo manager object
#' @param types package types for which repository should be initialized.
#'   If missing all project supported package types will be initialized (type: character)
#'
#' @return TRUE if initialized repository for at least one type, FALSE if
#'   structure was fully initialized already. (type:logical, invisible)
#'
#' @export
#'
repo_manager_init <- function(repo_manager, types) {
  assert(is_repo_manager(repo_manager), "rsuite_repo_manager object expected for repo_manager")

  if (!missing(types)) {
    assert(is.character(types) && length(types) > 0, "non empty character(N) expected for types")
  }

  UseMethod("repo_manager_init")
}

#'
#' Default implementation of repo_manager_init
#'
#' @keywords internal
#'
repo_manager_init.default <- function(repo_manager, types) {
  assert(FALSE,
         "repo_manager_init not implemented by %s",
         paste(class(repo_manager), collapse = " "))
}


#'
#' Puts packages of specified type located in local directory repository
#' into managed repository.
#'
#' @param repo_manager repo manager object.
#' @param src_dir local directory repository path. Directory must exist. (type: character)
#' @param types type of packages to sync. If missing all project supported
#'   package types will by synced. (type: character(N))
#'
#' @export
#'
repo_manager_upload <- function(repo_manager, src_dir, types) {
  assert(is_repo_manager(repo_manager), "rsuite_repo_manager object expected for repo_manager")
  assert(is_nonempty_char1(src_dir), "non empty character(1) expected for src_dir")
  assert(dir.exists(src_dir), "existing directory expected for src_dir")

  if (!missing(types)) {
    assert(is.character(types) && length(types) > 0,
           "non empty character(N) expected for types")
  }

  UseMethod("repo_manager_upload")
}

#'
#' Default implementation of repo_manager_upload
#'
#' @keywords internal
#'
repo_manager_upload.default <- function(repo_manager, src_dir, types) {
  assert(FALSE,
         "repo_manager_upload not implemented by %s",
         paste(class(repo_manager), collapse = " "))
}

#'
#' Removes specified packages of specified types from the repository.
#'
#' @param repo_manager repo manager object.
#' @param toremove data.frame with at lease Package(type:character) and
#'   Version(type: character) columns. (type: data.frame)
#' @param type package type to remove
#'
#' @return data.frame containig packages removed with Package and Version columns.
#'
#' @export
#'
repo_manager_remove <- function(repo_manager, toremove, type) {
  assert(is_repo_manager(repo_manager), "rsuite_repo_manager object expected for repo_manager")
  assert(is.data.frame(toremove) && all(c('Package', 'Version') %in% colnames(toremove)),
         "data.frame with at least colums Package and Version expected for toremove")
  assert(is_nonempty_char1(type), "non empty character(1) expected for type")

  res <- UseMethod("repo_manager_remove")

  assert(is.data.frame(res) && all(c('Package', 'Version') %in% colnames(toremove)),
         "Unexpected result of repo_manager_remove for %s implementation",
         paste(class(repo_manager), collapse = " "))
  return(res)
}

#'
#' Default implementation of repo_manager_remove.
#'
#' @keywords internal
#'
repo_manager_remove.default <- function(repo_manager, toremove, type) {
  assert(FALSE,
         "repo_manager_remove not implemented by %s",
         paste(class(repo_manager), collapse = " "))
}


#'
#' For repositories which needs some kind of connection to manage it cleans up
#'   previously initialized connection and releases all appropriate resources.
#'
#' @param repo_manager repo adapter object.
#'
#' @export
#'
repo_manager_destroy <- function(repo_manager) {
  assert(is_repo_manager(repo_manager), "rsuite_repo_manager object expected for repo_manager")
  UseMethod("repo_manager_destroy")
}

#'
#' Default implementation of repo_manager_destroy
#'
#' @keywords internal
#'
repo_manager_destroy.default <- function(repo_manager) {
  assert(FALSE,
         "repo_manager_destroy not implemented by %s",
         paste(class(repo_manager), collapse = " "))
}

#'
#' Checks if object is repo manager.
#'
#' @param obj object to check.
#' @return TRUE if object is of class rsuite_repo_manager
#'
#' @export
#'
is_repo_manager <- function(obj) {
  return("rsuite_repo_manager" %in% class(obj))
}

