#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Internal environment management utilities.
#----------------------------------------------------------------------------

#'
#' @keywords internal
#' 
#' Internal env for storing default project
#'
internal_env <- new.env()
assign('loaded_prj', NULL, envir = internal_env)
assign('prj_reg', list(), envir = internal_env)
assign('repo_adapter_reg', list(), envir = internal_env)
assign('rc_adapter_reg', list(), envir = internal_env)

#'
#' @keywords internal
#' 
#' If no default project, set argument as default project.
#' If argument is default project, update it in internal env.
#' Register(or update) project in project registry.
#'
register_prj <- function(prj) {
  stopifnot(is_prj(prj))

  loaded_prj <- get('loaded_prj', envir = internal_env)
  if (!is.null(loaded_prj) && loaded_prj$path == prj$path) {
    assign('loaded_prj', prj, envir = internal_env)
  }

  # update project registry
  prj_reg <- get('prj_reg', envir = internal_env)
  prj_reg[[prj$path]] <- prj
  assign('prj_reg', prj_reg, envir = internal_env)

  return(prj)
}

#'
#' @keywords internal
#' 
#' Finds project in project registry by it's base path.
#'
find_prj <- function(path) {
  stopifnot(is.character(path) && length(path) == 1)
  stopifnot(!is.null(path)
            && dir.exists(path) && file.exists(file.path(path, 'PARAMETERS')))

  prj <- get('prj_reg', envir = internal_env)[[path]]
  return(prj)
}

#'
#' @keywords internal
#' 
#' Register project as loaded.
#' If prj is NULL unsets loaded prj.
#'
set_loaded_prj <- function(prj) {
  if (!is.null(prj)) {
    stopifnot(is_prj(prj) && !is.null(find_prj(prj$path))) # registered project
  }

  prev_prj <- get('loaded_prj', envir = internal_env)
  assign('loaded_prj', prj, envir = internal_env)

  invisible(prev_prj)
}

#'
#' @keywords internal
#' 
#' Return loaded project or NULL if none loaded.
#'
get_loaded_prj <- function() {
  prj <- get('loaded_prj', envir = internal_env)
  return(prj)
}


#'
#' @keywords internal
#' 
#' Register repo_adapter in internal environment under name.
#'
reg_repo_adapter <- function(name, repo_adapter) {
  stopifnot(is_repo_adapter(repo_adapter))

  reg <- get('repo_adapter_reg', envir = internal_env)
  reg[[name]] <- repo_adapter
  assign('repo_adapter_reg', reg, envir = internal_env)
}

#'
#' @keywords internal
#' 
#' Get all registered repo adapter names.
#'
reg_repo_adapter_names <- function() {
  names(get('repo_adapter_reg', envir = internal_env))
}

#'
#' @keywords internal
#' 
#' Get all registered repo adapter names.
#'
reg_repo_adapter_names <- function() {
  names(get('repo_adapter_reg', envir = internal_env))
}


#'
#' @keywords internal
#' 
#' Return repo adapter under the name.
#'
find_repo_adapter <- function(name) {
  reg <- get('repo_adapter_reg', envir = internal_env)
  return(reg[[name]])
}


#'
#' @keywords internal
#' 
#' Register rc_adapter in internal environment under name.
#'
reg_rc_adapter <- function(name, rc_adapter) {
  stopifnot(is_rc_adapter(rc_adapter))
  
  reg <- get('rc_adapter_reg', envir = internal_env)
  reg[[name]] <- rc_adapter
  assign('rc_adapter_reg', reg, envir = internal_env)
}

#'
#' @keywords internal
#' 
#' Get all registered rc adapter names.
#'
reg_rc_adapter_names <- function() {
  names(get('rc_adapter_reg', envir = internal_env))
}

#'
#' @keywords internal
#' 
#' Return repo adapter under the name.
#'
find_rc_adapter <- function(name) {
  reg <- get('rc_adapter_reg', envir = internal_env)
  return(reg[[name]])
}
