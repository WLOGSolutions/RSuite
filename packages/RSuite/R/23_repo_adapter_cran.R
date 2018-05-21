#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Repo adapter working on CRAN.
#----------------------------------------------------------------------------

#'
#' Creates repo adapter providing repository to CRAN access.
#'
#' @param name under which repo adapter will be registered in RSuite.
#'
#' @return object of type rsuite_repo_adapter_cran
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_create_cran <- function(name) {
  result <- repo_adapter_create_base(name)

  local_env <- new.env()
  assign("path", NULL, envir = local_env)
  result$get_repo_path <- function() {
    path <- get("path", envir = local_env)
    if (!is.null(path)) {
      return(path)
    }
    path <- getOption("repos") # This can point to CRAN mirror
    if ("@CRAN@" %in% path) {
      # it sometimes contains just placeholder for URL '@CRAN@'
      #   replace it with first mirror URL in the case
      path <- utils::getCRANmirrors()$URL[[1]]
    }
    stopifnot(!is.null(path))
    assign("path", path, envir = local_env)
    return(path)
  }
  class(result) <- c("rsuite_repo_adapter_cran", class(result))
  return(result)
}

#'
#' Implementation of repo_adapter_get_info for rsuite_repo_adapter_cran (repo
#'   adapter working on CRAN).
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_get_info.rsuite_repo_adapter_cran <- function(repo_adapter, params) {
  return(list(
    readonly = TRUE,
    reliable = FALSE
  ))
}

#'
#' Implementation of repo_adapter_get_path for rsuite_repo_adapter_cran (repo
#'   adapter working on CRAN).
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_get_path.rsuite_repo_adapter_cran <- function(repo_adapter, params, ix = NA) {
  return(repo_adapter$get_repo_path())
}


#'
#' Implementation of repo_adapter_create_manager for rsuite_repo_adapter_cran.
#'
#' Just informs that CRAN is not manageable
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_create_manager.rsuite_repo_adapter_cran <- function(repo_adapter, ...) {
  assert(FALSE, "CRAN is readonly and cannot be managed")
}
