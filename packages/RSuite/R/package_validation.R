#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Validation tools.
#----------------------------------------------------------------------------

#'
#' Check if parameter is non empty character(1).
#'
#' @keywords internal
#' @noRd
#'
is_nonempty_char1 <- function(val) {
  !missing(val) && is.character(val) && length(val) == 1 && nchar(val) > 0
}

#'
#' The same as stopifnot but with message.
#'
#' @param cond Condition to be evaluated
#' @param msg Message for stop if condition is FALSE. If not passed \code{cond} code itself is used for the message.
#'
#' @keywords internal
#' @noRd
#'
assert <- function(cond, fail_msg = NULL, ...) {
  if (!cond) {
    if (is.null(fail_msg) || missing(fail_msg)) {
      fail_msg <- sprintf("Condition failed: %s", deparse(substitute(cond), width.cutoff = 30L))
    } else {
      fail_msg <- sprintf(fail_msg, ...)
    }
    pkg_logerror(fail_msg)
    stop(fail_msg, call. = FALSE)
  }
  invisible()
}
