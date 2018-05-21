#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Repo adapter working on Microsoft MRAN.
#----------------------------------------------------------------------------

#'
#' Creates repo adapter providing repository to Microsoft MRAN access.
#'
#' @param name under which repo adapter will be registered in RSuite.
#'
#' @return object of type rsuite_repo_adapter_mran
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_create_mran <- function(name) {
  result <- repo_adapter_create_base(name)

  result$get_repo_path <- function(snapshot_date) {
    repo_path <- paste0("https://mran.microsoft.com/snapshot/", snapshot_date)
    return(repo_path)
  }
  class(result) <- c("rsuite_repo_adapter_mran", class(result))
  return(result)
}

#'
#' Implementation of repo_adapter_get_info for rsuite_repo_adapter_mran (repo
#'   adapter working on Microsoft MRAN).
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_get_info.rsuite_repo_adapter_mran <- function(repo_adapter, params) {
  return(list(
    readonly = TRUE,
    reliable = TRUE
  ))
}

#'
#' Implementation of repo_adapter_get_path for rsuite_repo_adapter_mran (repo
#'   adapter working on Microsoft MRAN).
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_get_path.rsuite_repo_adapter_mran <- function(repo_adapter, params, ix = NA) {
  snapshot_date <- params$get_repo_adapter_arg(repo_adapter$name, default = NA, ix = ix)
  assert(all(!is.na(snapshot_date)),
         paste0("MRAN repository should have SnapshotDate as argument.",
                " Please, provide it in project PARAMETERS file in form %s[YYYY-MM-DD]"),
         repo_adapter$name)
  return(repo_adapter$get_repo_path(snapshot_date))
}


#'
#' Implementation of repo_adapter_create_manager for rsuite_repo_adapter_mran.
#'
#' Just informs that MRAN is not manageable
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_create_manager.rsuite_repo_adapter_mran <- function(repo_adapter, ...) {
  assert(FALSE, "MRAN is readonly and cannot be managed")
}
