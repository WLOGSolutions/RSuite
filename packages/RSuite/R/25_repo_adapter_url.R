#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Repo adapter working on url.
#----------------------------------------------------------------------------

#'
#' Creates repo adapter providing repository under url access.
#'
#' @param name under which repo adapter will be registered in RSuite.
#'
#' @return object of type rsuite_repo_adapter_url
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_create_url <- function(name) {
  result <- repo_adapter_create_base(name)
  class(result) <- c("rsuite_repo_adapter_url", class(result))
  return(result)
}


#'
#' Implementation of repo_adapter_get_info for rsuite_repo_adapter_url.
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_get_info.rsuite_repo_adapter_url <- function(repo_adapter, params) {
  info <- list(
    readonly = TRUE,
    reliable = FALSE
  )
  return(info)
}

#'
#' Implementation of repo_adapter_get_path for rsuite_repo_adapter_url.
#'
#' @param params should contain url to repository. (type: character).
#'
#' @keywords internal
#' @noRd
#'
repo_adapter_get_path.rsuite_repo_adapter_url <- function(repo_adapter, params, ix = NA) {
  url <- params$get_repo_adapter_arg(repo_adapter$name, default = "", ix = ix)
  assert(all(nchar(url) > 0),
         paste0("Url required as parameter to %s repository.",
                " Please specify it in project PARAMETERS file under Repositories entry",
                " in form %s[<url>]."),
         repo_adapter$name, repo_adapter$name)
  return(url)
}
