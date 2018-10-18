#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2018, WLOG Solutions
#
# CI adapter class API.
#----------------------------------------------------------------------------

#'
#' Creates the base presentation for the CI adapter to use by concrete implementations.
#'
#' @param name name under which CI adapter will be registered in RSuite. It
#'   cannot contain whitespaces or comma. (type: character)
#'
#' @return object of type rsuite_ci_adapter
#'
#' @family in extending RSuite with CI adapter
#'
#' @examples
#' # create you own CI adapter
#' ci_adapter_create_own <- function() {
#'   result <- ci_adapter_create_base("Own")
#'   class(result) <- c("ci_adapter_own", class(result))
#'   return(result)
#' }
#'
#' @export
#'
ci_adapter_create_base <- function(name) {
  assert(is_nonempty_char1(name), "Non empty character(1) expected for name")
  assert(!grepl("[\\s,]", name, perl = TRUE),
         "Adapter name cannot contain whitespaces or comma.")

  result <- list(name = name)
  class(result) <- "rsuite_ci_adapter"
  return(result)
}

#'
#' Detects if build process triggered by CI is currently running.
#'
#' @param ci_adapter ci adapter object
#'
#' @return TRUE if build triggered by CI is currently running.
#'
#' @family in extending RSuite with CI adapter
#'
#' @examples
#' # create you own CI adapter
#' ci_adapter_create_own <- function() {
#'   result <- ci_adapter_create_base("Own")
#'   class(result) <- c("ci_adapter_own", class(result))
#'   return(result)
#' }
#'
#' #' @export
#' ci_adapter_is_building.ci_adapter_own <- function(ci_adapter) {
#'   # ... check ...
#'   return(TRUE)
#' }
#'
#' @export
#'
ci_adapter_is_building <- function(ci_adapter) {
  assert(is_ci_adapter(ci_adapter), "rsuite_ci_adapter object expected for ci_adapter")

  UseMethod("ci_adapter_is_building")
}

#'
#' Default implemenation of ci_adapter_is_building
#'
#' @keywords internal
#' @noRd
#'
ci_adapter_is_building.default <- function(ci_adapter) {
  assert(FALSE,
         "ci_adapter_is_building not implemented by %s",
         paste(class(ci_adapter), collapse = " "))
}

#'
#' Retrieves current CI build number.
#'
#' @param ci_adapter ci adapter object
#'
#' @return build number reported by CI. (type: character).
#'
#' @family in extending RSuite with CI adapter
#'
#' @examples
#' # create you own CI adapter
#' ci_adapter_create_own <- function() {
#'   result <- ci_adapter_create_base("Own")
#'   class(result) <- c("ci_adapter_own", class(result))
#'   return(result)
#' }
#'
#' #' @export
#' ci_adapter_get_version.ci_adapter_own <- function(ci_adapter) {
#'   # ... detect if build triggered by CI is currently running ...
#'   return("0.0")
#' }
#'
#' @export
#'
ci_adapter_get_version <- function(ci_adapter) {
  assert(is_ci_adapter(ci_adapter), "rsuite_ci_adapter object expected for ci_adapter")

  UseMethod("ci_adapter_get_version")
}

#'
#' Default implemenation of ci_adapter_get_version
#'
#' @keywords internal
#' @noRd
#'
ci_adapter_get_version.default <- function(ci_adapter) {
  assert(FALSE,
         "ci_adapter_get_version not implemented by %s",
         paste(class(ci_adapter), collapse = " "))
}

#'
#' Checks if object is CI adapter.
#'
#' @param obj object to check.
#' @return TRUE if object is of class rsuite_ci_adapter
#'
#' @keywords internal
#' @noRd
#'
is_ci_adapter <- function(obj) {
  return("rsuite_ci_adapter" %in%  class(obj))
}
