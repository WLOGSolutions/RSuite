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
#' @family in extending RSuite with Repo adapter
#'
#' @examples
#' # create you own Repo adapter
#' repo_adapter_create_own <- function() {
#'   result <- repo_adapter_create_base("Own")
#'   class(result) <- c("repo_adapter_own", class(result))
#'   return(result)
#' }
#'
#' #' create own repo manager
#' #' @export
#' repo_adapter_create_manager.repo_adapter_own <- function(repo_adapter, ...) {
#'   repo_manager <- list() # create you own repo manager
#'   class(repo_manager) <- c("repo_manager_own", "rsuite_repo_manager")
#'   return(repo_manager)
#' }
#'
#' #' @export
#' repo_manager_get_info.repo_manager_own <- function(repo_manager) {
#'   return(list(
#'      types = c("source", "win-binary"), # package types supported by the manager
#'      rver = "3.5", # R version supported by the manager
#'      url = "file:///..." # base URL of repository
#'   ))
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
#' @noRd
#'
repo_manager_get_info.default <- function(repo_manager) {
  assert(FALSE,
         "repo_manager_get_info not implemented by %s",
         paste(class(repo_manager), collapse = " "))
}

#'
#' Initializes managed repository structure.
#'
#' @param repo_manager repo manager object
#' @param types package types for which repository should be initialized.
#'   If missing all project supported package types will be initialized (type: character)
#'
#' @return TRUE if initialized repository for at least one type, FALSE if
#'   structure was fully initialized already. (type:logical, invisible)
#'
#' @family in extending RSuite with Repo adapter
#'
#' @examples
#' # create you own Repo adapter
#' repo_adapter_create_own <- function() {
#'   result <- repo_adapter_create_base("Own")
#'   class(result) <- c("repo_adapter_own", class(result))
#'   return(result)
#' }
#'
#' #' create own repo manager
#' #' @export
#' repo_adapter_create_manager.repo_adapter_own <- function(repo_adapter, ...) {
#'   repo_manager <- list() # create you own repo manager
#'   class(repo_manager) <- c("repo_manager_own", "rsuite_repo_manager")
#'   return(repo_manager)
#' }
#'
#' #' @export
#' repo_manager_init.repo_manager_own <- function(repo_manager, types) {
#'   was_inited_already <- TRUE
#'   # ... if repository structure was not initialized initialize it  ...
#'   return(invisible(was_inited_already))
#' }
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
#' @noRd
#'
repo_manager_init.default <- function(repo_manager, types) {
  assert(FALSE,
         "repo_manager_init not implemented by %s",
         paste(class(repo_manager), collapse = " "))
}


#'
#' Adds packages to managed repository.
#'
#' @param repo_manager repo manager object.
#' @param src_dir local directory repository path. Directory must exist. (type: character)
#' @param types type of packages to sync. If missing all project supported
#'   package types will by synced. (type: character(N))
#'
#' @family in extending RSuite with Repo adapter
#'
#' @examples
#' # create you own repo adapter
#' repo_adapter_create_own <- function() {
#'   result <- repo_adapter_create_base("Own")
#'   class(result) <- c("repo_adapter_own", class(result))
#'   return(result)
#' }
#'
#' #' create own repo manager
#' #' @export
#' repo_adapter_create_manager.repo_adapter_own <- function(repo_adapter, ...) {
#'   repo_manager <- list() # create you own repo manager
#'   class(repo_manager) <- c("repo_manager_own", "rsuite_repo_manager")
#'   return(repo_manager)
#' }
#'
#' #' @export
#' repo_manager_upload.repo_manager_own <- function(repo_manager, src_dir, types) {
#'   # ... upload packages in src_dir into the repository ...
#'   # ... update PACKAGES ...
#' }
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
#' @noRd
#'
repo_manager_upload.default <- function(repo_manager, src_dir, types) {
  assert(FALSE,
         "repo_manager_upload not implemented by %s",
         paste(class(repo_manager), collapse = " "))
}

#'
#' Removes specified packages from the repository.
#'
#' @param repo_manager repo manager object.
#' @param toremove data.frame with at lease Package(type:character) and
#'   Version(type: character) columns. (type: data.frame)
#' @param type package type to remove
#'
#' @return data.frame containing packages removed with Package and Version columns.
#'
#' @family in extending RSuite with Repo adapter
#'
#' @examples
#' # create you own repo adapter
#' repo_adapter_create_own <- function() {
#'   result <- repo_adapter_create_base("Own")
#'   class(result) <- c("repo_adapter_own", class(result))
#'   return(result)
#' }
#'
#' #' create own repo manager
#' #' @export
#' repo_adapter_create_manager.repo_adapter_own <- function(repo_adapter, ...) {
#'   repo_manager <- list() # create you own repo manager
#'   class(repo_manager) <- c("repo_manager_own", "rsuite_repo_manager")
#'   return(repo_manager)
#' }
#'
#' #' @export
#' repo_manager_remove.repo_manager_own <- function(repo_manager, toremove, type) {
#'   # ... remove packages from the repository ...
#'   # ... update PACKAGES ...
#'   return(data.frame(Package = c(),   # return packages removed
#'                     Version = c(),
#'                     stringsAsFactors = FALSE))
#' }
#'
#' @export
#'
repo_manager_remove <- function(repo_manager, toremove, type) {
  assert(is_repo_manager(repo_manager), "rsuite_repo_manager object expected for repo_manager")
  assert(is.data.frame(toremove) && all(c("Package", "Version") %in% colnames(toremove)),
         "data.frame with at least colums Package and Version expected for toremove")
  assert(is_nonempty_char1(type), "non empty character(1) expected for type")

  res <- UseMethod("repo_manager_remove")

  assert(is.data.frame(res) && all(c("Package", "Version") %in% colnames(toremove)),
         "Unexpected result of repo_manager_remove for %s implementation",
         paste(class(repo_manager), collapse = " "))
  return(res)
}

#'
#' Default implementation of repo_manager_remove.
#'
#' @keywords internal
#' @noRd
#'
repo_manager_remove.default <- function(repo_manager, toremove, type) {
  assert(FALSE,
         "repo_manager_remove not implemented by %s",
         paste(class(repo_manager), collapse = " "))
}


#'
#' Releases resources allocated to manage the repository.
#'
#' For repositories which needs some kind of connection to manage it cleans up
#' previously initialized connection and releases all appropriate resources.
#'
#' @param repo_manager repo adapter object.
#'
#' @family in extending RSuite with Repo adapter
#'
#' @examples
#' # create you own repo adapter
#' repo_adapter_create_own <- function() {
#'   result <- repo_adapter_create_base("Own")
#'   class(result) <- c("repo_adapter_own", class(result))
#'   return(result)
#' }
#'
#' #' create own repo manager
#' #' @export
#' repo_adapter_create_manager.repo_adapter_own <- function(repo_adapter, ...) {
#'   repo_manager <- list() # create you own repo manager
#'   class(repo_manager) <- c("repo_manager_own", "rsuite_repo_manager")
#'   return(repo_manager)
#' }
#'
#' #' @export
#' repo_manager_destroy.repo_manager_own <- function(repo_manager) {
#'   # ... release resources ...
#' }
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
#' @noRd
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
#' @keywords internal
#' @noRd
#'
is_repo_manager <- function(obj) {
  return("rsuite_repo_manager" %in% class(obj))
}
