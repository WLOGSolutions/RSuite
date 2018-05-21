#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for managing requirements check results.
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
#' @return check_result object
#'
#' @keywords internal
#' @noRd
#'
check_res.build <- function(found, missing) {
  if (missing(found)) {
    found <- vers.build(avails = data.frame())
  }
  stopifnot(is.versions(found))
  if (!vers.is_empty(found)) {
    stopifnot(found$has_avails()
              && all(c("Package", "Version", "Repository", "File") %in% colnames(found$get_avails())))
  }

  if (base::missing(missing)) {
    missing <- vers.build()
  }
  stopifnot(is.versions(missing))

  res <- list(
    found = found,
    missing = missing,

    get_found_names = function() vers.get_names(found),
    get_missing_names = function() vers.get_names(missing)
  )
  class(res) <- "check_result"

  return(res)
}


#'
#' Custom print implementation for check_result
#'
#' @param cr check_result object to print.
#'
#' @keywords internal
#' @noRd
#'
print.check_result <- function(cr) {
  cat("found:\n")
  print(cr$found)
  cat("missing:\n")
  print(cr$missing$pkgs)
}


#'
#' Merges two check results. REsult hase common found and missings of two of them.
#'
#' @param cr check_result object.
#' @param oth check_result object.
#'
#' @return check_result object which is result of merging inputs.
#'
#' @keywords internal
#' @noRd
#'
check_res.union <- function(cr, oth) {
  stopifnot(is.check_result(cr))
  stopifnot(is.check_result(oth))

  vers <- vers.union(vers.drop_avails(cr$found),
                     vers.drop_avails(oth$found),
                     cr$missing,
                     oth$missing)
  avails <- vers.union(cr$found, oth$found)

  return(vers.check_against(vers, avails))
}

#'
#' Joins two check_results controling if they have some new missings presended
#'   due to common requiremets infeasibility. Result missings are fixed to
#'   reflect thir common missing state.
#'
#' @param cr check_result object
#' @param oth check_result object
#'
#' @return check_result object representing join of inputs.
#'
#' @keywords internal
#' @noRd
#'
check_res.join <- function(cr, oth) {
  stopifnot(is.check_result(cr))
  stopifnot(is.check_result(oth))

  found <- vers.union(cr$found, oth$found)
  missing <- vers.union(cr$missing, oth$missing)

  infeasibles <- c(vers.get_unfeasibles(found), vers.get_unfeasibles(missing))
  infeas_vers <- vers.rm(found, setdiff(vers.get_names(found), infeasibles))
  missing <- vers.union(missing, vers.drop_avails(infeas_vers))

  found <- vers.rm(found, infeasibles)
  missing <- vers.rm(missing, vers.get_names(found))

  return(check_res.build(found, missing))
}


#'
#' Totaly excludes packages passed from check_result.
#'
#' @param cr check_result object to exclude packages from.
#' @param pkg_names character verctor of package names to exclude. (type: character(N))
#'
#' @return check_result object with packages excluded.
#'
#' @keywords internal
#' @noRd
#'
check_res.exclude <- function(cr, pkg_names) {
  stopifnot(is.check_result(cr))
  stopifnot(is.character(pkg_names))

  return(check_res.build(
    found = vers.rm(cr$found, pkg_names),
    missing = vers.rm(cr$missing, pkg_names)
  ))
}

#'
#' Checks if check_res object has any found packages.
#'
#' @param cr check_res object to check for found packages.
#'
#' @return TRUE if found packages in check_result is not empty.
#'
#' @keywords internal
#' @noRd
#'
check_res.has_found <- function(cr) {
  stopifnot(is.check_result(cr))

  !vers.is_empty(cr$found)
}


#'
#' Retrieves versions object containing all found packages in check_result.
#'
#' @param cr check_res object to retrieve found from.
#'
#' @return versions object with packages found. It has availables.
#'
#' @keywords internal
#' @noRd
#'
check_res.get_found <- function(cr) {
  stopifnot(is.check_result(cr))

  cr$found
}


#'
#' Checks if check_res object has any missing packages.
#'
#' @param cr check_res object to check for missing packages.
#'
#' @return TRUE if missing packages in check_result is not empty.
#'
#' @keywords internal
#' @noRd
#'
check_res.has_missing <- function(cr) {
  stopifnot(is.check_result(cr))

  !vers.is_empty(cr$missing)
}

#'
#' Retrieves versions object containing all missing packages in check_result.
#'
#' @param cr check_res object to retrieve missing from.
#'
#' @return versions object with packages missing. It has no availables.
#'
#' @keywords internal
#' @noRd
#'
check_res.get_missing <- function(cr) {
  stopifnot(is.check_result(cr))

  cr$missing
}


#'
#' Check if object is check_result.
#'
#' @param cr object to check
#' @return TRUE if version object.
#'
#' @keywords internal
#' @noRd
#'
is.check_result <- function(cr) {
  return(class(cr) == "check_result")
}
