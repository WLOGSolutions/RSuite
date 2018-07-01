#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Repo adapter working on directory.
#----------------------------------------------------------------------------

.is_abs_path <- function(path) {
  return(all(grepl("^([A-Za-z]:)?[\\/\\\\]", path)))
}

.can_eventualy_have_rw_access <- function(full_path) {
  unlist(lapply(full_path,
                function(base_dir) {
                  while (!dir.exists(base_dir)) {
                    base_dir <- dirname(base_dir)
                  }
                  return (file.access(base_dir, 2) != -1)
                }))
}

#'
#' Creates repo adapter providing repository under path passed.
#'
#' The adapter can use argument which is interpreted as repository path. If not
#'    passed default value for repository path is used.
#'
#' @param name under which repo adapter will be registered in RSuite.
#'
#' @return object of type rsuite_repo_adapter_dir
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_create_dir <- function(name) {
  result <- repo_adapter_create_base(name)

  result$get_full_path <- function(params, ix = NA) {
    path <- params$get_repo_adapter_arg(name, default = "repository", ix = ix)
    if (.is_abs_path(path)) {
      full_path <- path
    } else {
      full_path <- file.path(params$prj_path, path)
    }
    return(rsuite_fullUnifiedPath(full_path))
  }

  class(result) <- c("rsuite_repo_adapter_dir", class(result))
  return(result)
}

#'
#' Implementation of repo_adapter_get_info for rsuite_repo_adapter_dir (repo
#'   adapter working on directory).
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_get_info.rsuite_repo_adapter_dir <- function(repo_adapter, params) {
  full_path <- repo_adapter$get_full_path(params)
  readonly <- !all(.can_eventualy_have_rw_access(full_path))
  prj_local <- all(substring(full_path, 1, nchar(params$prj_path)) == params$prj_path)

  return(list(
    readonly = readonly,
    reliable = prj_local
  ))
}

#'
#' Implementation of repo_adapter_get_path for rsuite_repo_adapter_dir (repo
#'   adapter working on directory).
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_get_path.rsuite_repo_adapter_dir <- function(repo_adapter, params, ix = NA) {
  full_path <- repo_adapter$get_full_path(params, ix)
  return(paste0("file:///", full_path))
}

#'
#' Implementation of repo_adapter_start_management for rsuite_repo_adapter_dir (repo
#'   adapter working on directory).
#'
#' @param repo_adapter repo adapter object.
#' @param ... should contain prj of class rsuite_project and repo ix
#'    or path to repository and rver. It also can contain types, a vector of types to be managed
#'   (default: .Platform$pkgType)
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_create_manager.rsuite_repo_adapter_dir <- function(repo_adapter, ...) {
  dots <- list(...)
  if ("prj" %in% names(dots)) {
    prj <- dots$prj
    assert(!is.null(prj) && is_prj(prj), "rsuite_project expected for prj")
    dots$params <- prj$load_params()
  }

  if ("params" %in% names(dots)) {
    params <- dots$params
    assert(!is.null(params) && "rsuite_project_params" %in% class(params),
           "rsuite_project_params expected for params")
    assert("ix" %in% names(dots),
           paste0("Either prj/params and ix or path and rver must be provided to",
                  " repo_adapter_create_manager.rsuite_repo_adapter_dir"))
    ix <- dots$ix
    full_path <- repo_adapter$get_full_path(params, ix)

    rver <- params$r_ver

    is_rw <- !repo_adapter_get_info.rsuite_repo_adapter_dir(repo_adapter, params)$readonly
    types <- c(params$pkgs_type, params$aux_pkgs_type)
  } else {
    assert(all(c("rver", "path") %in% names(dots)),
           paste0("Either prj/params and ix or path and rver must be provided to",
                  " repo_adapter_create_manager.rsuite_repo_adapter_dir"))

    full_path <- dots$path
    assert(is_nonempty_char1(full_path), "Non empty character(1) expected for 'path'")
    assert(.is_abs_path(full_path), "Absolute path expected for 'path': %s", full_path)

    rver <- dots$rver

    is_rw <- all(.can_eventualy_have_rw_access(full_path))
    if ("types" %in% names(dots)) {
      types <- dots$types
      assert(is.character(types) & length(types) > 0, "Non empty character(N) expected for types")

      exp_types <- unique(c("win.binary", "mac.binary", "binary", "source", .Platform$pkgType))
      assert(all(types %in% exp_types),
             "Invalid types management requested. Supported types are %s",
             paste(exp_types, collapse = ", "))
    } else {
      types <- .Platform$pkgType
    }
  }

  assert(is_rw,
         "Repository cannot be managed due to insufficient access permissions")
  repo_manager <- repo_manager_dir_create(path = full_path, types = types, rver = rver)
  return(repo_manager)
}
