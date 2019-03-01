#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Repo adapter working on S3.
#----------------------------------------------------------------------------

#'
#' Creates repo adapter providing repository on S3 access.
#' It based on Url adapter, just adds some functionalities and checks.
#'
#' @param name under which repo adapter will be registered in RSuite.
#'
#' @return object of type rsuite_repo_adapter_s3
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_create_s3 <- function(name) {
  result <- repo_adapter_create_url(name)
  class(result) <- c("rsuite_repo_adapter_s3", class(result))
  return(result)
}

#'
#' Implementation of repo_adapter_get_path for rsuite_repo_adapter_s3 (repo
#'   adapter working on Amazon S3).
#'
#' @param params should contain url to repository. Amazon S3 url must be in form
#'   <schema>://<bucket>.s3.amazonaws.com/<path>, there <schema> is http or
#'   https, <bucket> is name of bucket, and <path> is optional path to root of
#'   the repository inside bucket. (type: character).
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_get_path.rsuite_repo_adapter_s3 <- function(repo_adapter, params, ix = NA) {
  url <- repo_adapter_get_path.rsuite_repo_adapter_url(repo_adapter, params, ix)
  assert(all(grepl("^https?://[\\w\\d\\._-]+\\.s3\\.amazonaws\\.com(/.*)?$", url, perl = TRUE)),
         paste0("Invalid url specified for %s repository in project PARAMETERS file.",
                " Amazon S3 url should have <schema>://<bucket>.s3.amazonaws.com[/<path>] form;",
                " Url does not have required form: %s"),
         repo_adapter$name, url)
  return(url)
}

#'
#' Implementation of repo_adapter_create_manager for rsuite_repo_adapter_s3 (repo
#'   adapter working on Amazon S3).
#'
#' Checks if AWS CLI is useable and has configuration to access S3 bucket with RW
#'   privileges.
#'
#' It uses AWS CLI client to put temporary file onto S3 bucket and removes it
#'   afterwards.
#'
#'
#' @param repo_adapter repo adapter object.
#' @param ... can contain s3_profile value (type: character, default: "default").
#'   It must contain prj, params or url to the repository and rver.
#'   It also can contain types, a vector of types to be managed
#'   (default: .Platform$pkgType)
#'
#' @return manageable repo adapter object.
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_create_manager.rsuite_repo_adapter_s3 <- function(repo_adapter, ...) {
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
           paste0("Either prj/params and ix or url and rver must be provided to",
                  " repo_adapter_start_management.rsuite_repo_adapter_s3"))

    ix <- dots$ix
    url <- repo_adapter_get_path.rsuite_repo_adapter_s3(repo_adapter, params, ix = ix)
    types <- c(params$pkgs_type, params$aux_pkgs_type)
    rver <- params$r_ver
  } else {
    assert(all(c("url", "rver") %in% names(dots)),
           paste0("Either prj/params and ix or url and rver must be provided to",
                  " repo_adapter_start_management.rsuite_repo_adapter_s3"))
    url <- dots$url
    rver <- dots$rver

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

  repo_manager <- repo_manager_s3_create(url, types, rver, dots$s3_profile)
  return(repo_manager)
}
