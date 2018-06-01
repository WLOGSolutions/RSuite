#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Package logger setup
#----------------------------------------------------------------------------

.logger_name <- "rsuite"
.pkg_logger <- logging::getLogger(.logger_name)
.pkg_logger$setLevel("FINEST")

pkg_loginfo <- function(msg, ...) tryCatch(logging::loginfo(msg, ..., logger = .pkg_logger),
                                           error = function(e) warning(e))
pkg_logdebug <- function(msg, ...) tryCatch(logging::logdebug(msg, ..., logger = .pkg_logger),
                                            error = function(e) warning(e))
pkg_logerror <- function(msg, ...) tryCatch(logging::logerror(msg, ..., logger = .pkg_logger),
                                            error = function(e) warning(e))
pkg_logwarn <- function(msg, ...) tryCatch(logging::logwarn(msg, ..., logger = .pkg_logger),
                                           error = function(e) warning(e))
pkg_logfinest <- function(msg, ...) tryCatch(logging::logfinest(msg, ..., logger = .pkg_logger),
                                             error = function(e) warning(e))

#'
#' Retrieves RSuite logger.
#'
#' @return logger object
#'
#' @family miscellaneous
#'
#' @examples
#' logging::loginfo("This is an INFO from RSuite", logger = rsuite_getLogger())
#'
#' @export
#'
rsuite_getLogger <- function() {
  .pkg_logger
}
