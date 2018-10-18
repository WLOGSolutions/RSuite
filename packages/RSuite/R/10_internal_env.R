#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Internal environment management utilities.
#----------------------------------------------------------------------------

#'
#' Internal env for storing default project
#'
#' @keywords internal
#' @noRd
#'
internal_env <- new.env()
assign("loaded_prj", NULL, envir = internal_env)
assign("prj_reg", list(), envir = internal_env)
assign("repo_adapter_reg", list(), envir = internal_env)
assign("ci_adapter_reg", list(), envir = internal_env)
assign("rc_adapter_reg", list(), envir = internal_env)

#'
#' If no default project, set argument as default project.
#' If argument is default project, update it in internal env.
#' Register(or update) project in project registry.
#'
#' @keywords internal
#' @noRd
#'
register_prj <- function(prj) {
  stopifnot(is_prj(prj))

  loaded_prj <- get("loaded_prj", envir = internal_env)
  if (!is.null(loaded_prj) && loaded_prj$path == prj$path) {
    assign("loaded_prj", prj, envir = internal_env)
  }

  # update project registry
  prj_reg <- get("prj_reg", envir = internal_env)
  prj_reg[[prj$path]] <- prj
  assign("prj_reg", prj_reg, envir = internal_env)

  return(prj)
}

#'
#' Finds project in project registry by it's base path.
#'
#' @keywords internal
#' @noRd
#'
find_prj <- function(path) {
  stopifnot(is.character(path) && length(path) == 1)
  stopifnot(!is.null(path)
            && dir.exists(path) && file.exists(file.path(path, "PARAMETERS")))

  prj <- get("prj_reg", envir = internal_env)[[path]]
  return(prj)
}

#'
#' Register project as loaded.
#' If prj is NULL unsets loaded prj.
#'
#' @keywords internal
#' @noRd
#'
set_loaded_prj <- function(prj) {
  if (!is.null(prj)) {
    stopifnot(is_prj(prj) && !is.null(find_prj(prj$path))) # registered project
  }

  prev_prj <- get("loaded_prj", envir = internal_env)
  assign("loaded_prj", prj, envir = internal_env)

  invisible(prev_prj)
}

#'
#' Return loaded project or NULL if none loaded.
#'
#' @keywords internal
#' @noRd
#'
get_loaded_prj <- function() {
  prj <- get("loaded_prj", envir = internal_env)
  return(prj)
}


#'
#' Register repo_adapter in internal environment under name.
#'
#' @keywords internal
#' @noRd
#'
reg_repo_adapter <- function(name, repo_adapter) {
  stopifnot(is_repo_adapter(repo_adapter))

  reg <- get("repo_adapter_reg", envir = internal_env)
  reg[[name]] <- repo_adapter
  assign("repo_adapter_reg", reg, envir = internal_env)
}


#'
#' Unregister repo_adapter from internal environment under name.
#'
#' @param name name of the repo_adapter to unregister (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
unreg_repo_adapter <- function(name) {
  stopifnot(name %in% reg_repo_adapter_names())

  reg <- get("repo_adapter_reg", envir = internal_env)
  reg[[name]] <- NULL
  assign("repo_adapter_reg", reg, envir = internal_env)
}

#'
#' Get all registered repo adapter names.
#'
#' @keywords internal
#' @noRd
#'
reg_repo_adapter_names <- function() {
  names(get("repo_adapter_reg", envir = internal_env))
}

#'
#' Get all registered repo adapter names.
#'
#' @keywords internal
#' @noRd
#'
reg_repo_adapter_names <- function() {
  names(get("repo_adapter_reg", envir = internal_env))
}


#'
#' Return repo adapter under the name.
#'
#' @keywords internal
#' @noRd
#'
find_repo_adapter <- function(name) {
  reg <- get("repo_adapter_reg", envir = internal_env)
  return(reg[[name]])
}


#'
#' Register rc_adapter in internal environment under name.
#'
#' @param name name of adapter to (un)register.
#' @param rc_adapter adapter to register. if NULL unregister it.
#'
#' @keywords internal
#' @noRd
#'
reg_rc_adapter <- function(name, rc_adapter) {
  stopifnot(is.null(rc_adapter) || is_rc_adapter(rc_adapter))

  reg <- get("rc_adapter_reg", envir = internal_env)
  reg[[name]] <- rc_adapter
  assign("rc_adapter_reg", reg, envir = internal_env)
}

#'
#' Get all registered rc adapter names.
#'
#' @keywords internal
#' @noRd
#'
reg_rc_adapter_names <- function() {
  names(get("rc_adapter_reg", envir = internal_env))
}

#'
#' Return rc adapter under the name.
#'
#' @keywords internal
#' @noRd
#'
find_rc_adapter <- function(name) {
  reg <- get("rc_adapter_reg", envir = internal_env)
  return(reg[[name]])
}

#'
#' Register ci_adapter in internal environment under name.
#'
#' @param name name of adapter to (un)register.
#' @param ci_adapter adapter to register. if NULL unregister it.
#'
#' @keywords internal
#' @noRd
#'
reg_ci_adapter <- function(name, ci_adapter) {
  stopifnot(is.null(ci_adapter) || is_ci_adapter(ci_adapter))

  reg <- get("ci_adapter_reg", envir = internal_env)
  reg[[name]] <- ci_adapter
  assign("ci_adapter_reg", reg, envir = internal_env)
}

#'
#' Get all registered ci adapter names.
#'
#' @keywords internal
#' @noRd
#'
reg_ci_adapter_names <- function() {
  names(get("ci_adapter_reg", envir = internal_env))
}

#'
#' Return ci adapter under the name.
#'
#' @keywords internal
#' @noRd
#'
find_ci_adapter <- function(name) {
  reg <- get("ci_adapter_reg", envir = internal_env)
  return(reg[[name]])
}
