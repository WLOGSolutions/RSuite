#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for managing dependency search results.
#
#----------------------------------------------------------------------------

#'
#' Builds search results object.
#'
#' It contains versions found and versions describing dependencies
#' which failed to find in any of repositories or versions found which
#' cannot meet requirements.
#'
#' @param found versions object with found packages
#' @param missing versions object with mising pacakges (optional)
#'
#' @return pkgSearchResults object
#'
#' @keywords internal
#'
build.pkgSearchResults <- function(found, missing) {
  if (missing(found)) {
    found <- versions.build(avails = data.frame())
  }
  stopifnot(is.versions(found))
  if (!found$is_empty()) {
    stopifnot(found$has_avails() && all(c("Package", "Version", "Repository", "File") %in% colnames(found$get_avails())))
  }

  if (base::missing(missing)) {
    missing <- versions.build()
  }
  stopifnot(is.versions(missing))

  res <- list(
    found = found,
    missing = missing,

    union = function(psr) {
      stopifnot(is.pkgSearchResults(psr))

      return(build.pkgSearchResults(
        found = versions.union(found, psr$get_found()),
        missing = versions.union(missing, psr$get_missing())
      ))
    },
    join = function(psr) {
      stopifnot(is.pkgSearchResults(psr))

      return(build.pkgSearchResults(
        found = versions.union(found, psr$get_found()),
        missing = versions.union(missing$rm(psr$get_found_names()),
                                 psr$get_missing()$rm(found$get_names()))
      ))
    },
    exclude = function(pkg_names) {
      return(build.pkgSearchResults(
        found = found$rm(pkg_names),
        missing = missing$rm(pkg_names)
      ))
    },

    has_found = function() { !found$is_empty() },
    get_found = function() { found },
    get_found_names = function() { found$get_names() },
    get_avails = function() { found$get_avails() },

    has_missing = function() { !missing$is_empty() },
    get_missing = function() { missing },
    get_missing_names = function() { missing$get_names() }
  )
  class(res) <- "pkgSearchResults"

  return(res)
}

#'
#' Check if object is pkgSearchResults object.
#'
#' @param psr object to check
#' @return TRUE if version object.
#'
#' @keywords internal
#'
is.pkgSearchResults <- function(psr) {
  return(class(psr) == "pkgSearchResults")
}
