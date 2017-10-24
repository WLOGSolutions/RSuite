#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Repo adapter class API.
#----------------------------------------------------------------------------

#'
#' Creates base presentation for repo adapter to use by concrete implementations.
#'
#' @param name name under which repository adapter will registered in RSuite. It
#'   cannot contain whitespaces or comma. (type: character)
#'
#' @return object of type rsuite_repo_adapter
#'
#' @export
#'
repo_adapter_create_base <- function(name) {
  assert(!missing(name) && is.character(name) && length(name) == 1 && nchar(name) > 0,
         "Non empty character(1) expected for name")
  assert(!grepl("[\\s,]", name, perl = TRUE),
         "Adapter name cannot contain whitespaces or comma.")

  result <- list(name = name)
  class(result) <- 'rsuite_repo_adapter'
  return(result)
}

#'
#' Returns informations about repository the adapter is working on.
#'
#' @param repo_adapter repo adapter object
#' @param params rsuite_project_params object
#'
#' @return named list with following entries:
#' \describe{
#'   \item{readonly}{TRUE if the repository is for reading only (type:logical)}
#'   \item{reliable}{TRUE if content of the repository does not change over time
#'     unless repository changes enforce change of project itself (like project
#'     local repository) (type: logical).}
#' }
#'
#' @export
#'
repo_adapter_get_info <- function(repo_adapter, params) {
  stopifnot(class(params) == "rsuite_project_params")
  stopifnot(is_repo_adapter(repo_adapter))

  UseMethod("repo_adapter_get_info")
}

#'
#' Default implementation of repo_adapter_get_info
#'
#' @export
#'
repo_adapter_get_info.default <- function(repo_adapter, params) {
  assert(FALSE,
         "repo_adapter_get_info not implemented by %s",
         paste(class(repo_adapter), collapse = " "))
}

#'
#' Returns adapter path related to project to use for dependencies resolution.
#'
#' @param repo_adapter repo adapter object
#' @param params rsuite_project_params object
#' @param ix repo adapter index in project repositories or NA to retrieve all
#'  paths for the adapter. (type: integer, default: NA)
#'
#' @return path to repository for the project.
#'
#' @export
#'
repo_adapter_get_path <- function(repo_adapter, params, ix = NA) {
  stopifnot(class(params) == "rsuite_project_params")
  stopifnot(is_repo_adapter(repo_adapter))
  stopifnot(!missing(ix) && length(ix) == 1 && (is.na(ix) || is.integer(ix)))

  UseMethod("repo_adapter_get_path")
}

#'
#' Default implementation of repo_adapter_get_path.
#'
#' @export
#'
repo_adapter_get_path.default <- function(repo_adapter, params) {
  assert(FALSE,
         "repo_adapter_get_path not implemented by %s",
         paste(class(repo_adapter), collapse = " "))
}


#'
#' Creates repo manager to manager ther repository.
#'
#' For repositories which needs some kind of connection to manage it initializes
#'   appropriate resources..
#'
#' Raises error if failes to create manager.
#'
#' @param repo_adapter repo adapter on which manager is base. (type: rsuite_repo_adapter)
#' @param ... manager specific parameters.
#'
#' @return object of type rsuite_repo_adapter
#'
#' @export
#'
repo_adapter_create_manager <- function(repo_adapter, ...) {
  assert(is_repo_adapter(repo_adapter), "rsuite_repo_adapter object expected for repo_adapter")

  repo_manager <- UseMethod("repo_adapter_create_manager")
  stopifnot(is_repo_manager(repo_manager))

  return(repo_manager)
}

#'
#' Default implementation of repo_manager_create
#'
#' @export
#'
repo_adapter_create_manager.default <- function(repo_adapter, ...) {
  assert(FALSE,
         "repo_manager_create not implemented for %s",
         paste(class(repo_adapter), collapse = " "))
}


#'
#' Checks if object is repo adapter.
#'
#' @param obj object to check.
#' @return TRUE if object is of class rsuite_repo_adapter
#'
#' @export
#'
is_repo_adapter <- function(obj) {
  return("rsuite_repo_adapter" %in%  class(obj))
}
