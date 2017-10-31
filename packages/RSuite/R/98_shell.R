#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for running shell command.
#----------------------------------------------------------------------------

#'
#' Runs command and collects it's output lines
#'
#' Logs command to run and all output to finest level.
#'
#' @param desc description of command to prefix logged messages. (type: character)
#' @param cmd command to run. Can contain formating expessions. (type: character)
#' @param ... parameters to build cmd using sprintf.
#'
#' @return character(N) containing command output lines.
#'
#' @keywords internal
#'
get_cmd_lines <- function(desc, cmd, ...) {
  full_cmd <- paste0(sprintf(cmd, ...), " 2>&1")

  pkg_logfinest("%s cmd: %s", desc, full_cmd)

  lines <- character(0)
  con <- pipe(full_cmd, open = "rt")
  tryCatch({
    while(TRUE) {
      ln <- readLines(con, n = 1, skipNul = T)
      if (!length(ln) || !nchar(ln)) {
        break
      }
      pkg_logfinest("%s output: %s", desc, ln)
      lines <- c(lines, ln)
    }
  }, finally = close(con))

  return(lines)
}


#'
#' Executes R script code in RScript subprocess collecting it's output
#' and logging it on DEBUG level.
#'
#' @param script_code R code commands to run. Lines are merged with ';'.
#'    Can contain formating expessions. (type: character)
#' @param ... parameters to build script_code using sprintf.
#' @param rver R vestion to run rscript with. If not passed (or NA) current
#'   R version will be used. (type: character, default: NA)
#' @param ex_libpath extra path to add to .libPaths. (type: character, default: NULL)
#'
#' @return NULL if succeded, if failed returns FALSE or error string.
#'
#' @keywords internal
#'
run_rscript <- function(script_code, ..., rver = NA, ex_libpath = NULL) {
  full_code <- sprintf(paste0(script_code, collapse = ";"), ...)

  cmd0 <- get_rscript_path(rver = ifelse(is.na(rver), current_rver(), rver))

  libs <- c(ex_libpath, .libPaths())
  libs <- libs[-length(libs)] # last is always .Library

  script <- sprintf(paste0(".Library <- NULL;",
                           ".libPaths(%s);",
                           "tryCatch({",
                           "  suppressWarnings({ %s });",
                           "  cat(sprintf('~ done\\n'))",
                           "}, error = function(e) {",
                           "  cat(sprintf('~ error:%%s\\n', e))",
                           "})"),
                    rscript_arg("new", rsuite_fullUnifiedPath(libs)), full_code)

  rscript_cmd <- paste(cmd0, "--no-init-file", "--no-site-file", "-e", shQuote(script), "2>&1")
  pkg_logdebug("> cmd: %s", rscript_cmd)

  con <- pipe(rscript_cmd, open = "rt")

  result <- tryCatch({
    ok <- FALSE
    while(TRUE) {
      ln <- readLines(con, n = 1, skipNul = T)
      if (!length(ln)) {
        break
      }

      if (grepl("^~ error:", ln)) {
        ok <- sub("^~ error:", "", ln)
      } else if (ln == "~ done") {
        ok <- NULL
      } else {
        pkg_logdebug("> %s", ln)
      }
    }
    ok
  },
  error = function(e) { FALSE },
  finally = close(con))

  return(result)
}

#'
#' Creates string which can be passed as parameter to script code.
#'
#' @keywords internal
#'
rscript_arg <- function(name, val) {
  if (is.null(val)) {
    return(sprintf("%s=NULL", name))
  }

  val <- gsub("\\\\", "/", val)
  val <- gsub("\"", "\\\"", val, fixed = T)
  val <- gsub("'", "\\\"", val, fixed = T)
  if (length(val) == 1) {
    sprintf("%s='%s'", name, val)
  } else {
    sprintf("%s=c('%s')", name, paste(val, collapse = "', '"))
  }
}
