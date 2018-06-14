#----------------------------------------------------------------------------
# __PackageName__
#
# Validation tools
#----------------------------------------------------------------------------

#'
#' The same as stopifnot but with message.
#'
#' @param cond Condition to be evaluated
#' @param msg Message for stop if condition is FALSE. If not passed \code{cond} code itself is used for the message.
#'
assert <- function(cond, fail_msg = NULL, ...) {
  if (!cond) {
    if (is.null(fail_msg) || missing(fail_msg)) {
      fail_msg <- sprintf("Condition failed: %s", deparse(substitute(cond), width.cutoff = 30L))
    } else {
      fail_msg <- sprintf(fail_msg, ...)
    }
    stop(fail_msg, call. = FALSE)
  }
  invisible()
}

#' Performes validation of data.table passed
#'
#' @param dt data.table to be validated
#' @param dt_arg name of data.table argument for proper message content
#' @param col_class named list of columns and there expected classes
#'
#' @examples {
#'  dt <- data.table(c1= c(1,2,3), c2=c(Sys.Date()), c3=c("Ala & kotek"))
#'  dt_validate(dt, "dt", list(c2 = "Date", c1 = "numeric"))
#'  }
#'
dt_validate <- function(dt, dt_arg, expected_col_class) {
  assert(is.data.table(dt),
         fail_msg = sprintf("data.table expected for '%s'", dt_arg))

  expected_names <- names(expected_col_class)

  notfound_columns <- setdiff(expected_names, colnames(dt))
  assert(!length(notfound_columns),
         fail_msg = sprintf("Expected columns not present in the '%s' data.table: %s",
                            dt_arg,
                            paste(notfound_columns, collapse = ", ")))

  for (cn in expected_names) {
    col_class <- class(dt[, get(cn)])
    exp_class <- expected_col_class[[cn]]
    assert(col_class == exp_class,
           fail_msg = sprintf("Column of type '%s' expected for '%s' in data.table '%s'; '%s' found",
                              exp_class, cn, dt_arg, col_class))
  }
}

#'
#' Performes validation and grooming of data.table passed
#'
#' @param dt data.table to be validated and groomed
#' @param dt_arg name of data.table argument for proper message content
#' @param col_class named list of columns and there expected classes
#'
#' @return data.table with only expected columns in order specified by \code{col_class}.
#' @examples {
#'  dt <- data.table(c1= c(1,2,3), c2=c(Sys.Date()), c3=c("Ala & kotek"))
#'  dt_validate_and_groom(dt, "dt", list(c2 = "Date", c1 = "numeric"))
#'  # should output
#'  #            c2 c1
#'  # 1: 2016-04-05  1
#'  # 2: 2016-04-05  2
#'  # 3: 2016-04-05  3
#' }
#'
dt_validate_and_groom <- function(dt, dt_arg, expected_col_class) {
  dt_validate(dt, dt_arg, expected_col_class)

  expected_names <- names(expected_col_class)
  res_dt <- dt[, expected_names, with = FALSE] # select only requested colums
  setcolorder(res_dt, expected_names) # set proper column ordering

  return(res_dt)
}
