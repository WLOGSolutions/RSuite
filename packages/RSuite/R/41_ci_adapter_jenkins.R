#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2018, WLOG Solutions
#
# CI adapter working with Jenkins.
#----------------------------------------------------------------------------

#'
#' Creates CI adapter to handle projects under Jenkins.
#'
#' @param name under which CI adapter will be registered in RSuite.
#' @return object of type rsuite_ci_adapter
#'
#' @keywords internal
#' @noRd
#'
ci_adapter_create_jenkins <- function(name) {
  result <- ci_adapter_create_base(name)

  class(result) <- c("rsuite_ci_adapter_jenkins", class(result))

  return(result)
}

#'
#' Implementation of ci_adapter_is_building for Jenkins ci adapter.
#'
#' Checks if build triggered by Jenkins CI is currently running: JOB_NAME environment variable exists.
#'
#' @keywords internal
#' @noRd
#'
ci_adapter_is_building.rsuite_ci_adapter_jenkins <- function(ci_adapter) {
  job_name <- Sys.getenv("JOB_NAME")
  return(job_name != "")
}

#'
#' Implementation of ci_adapter_get_version for Jenkins ci adapter.
#'
#' @keywords internal
#' @noRd
#'
ci_adapter_get_version.rsuite_ci_adapter_jenkins <- function(ci_adapter) {
  build_number <- Sys.getenv("BUILD_NUMBER")
  return(build_number)
}
