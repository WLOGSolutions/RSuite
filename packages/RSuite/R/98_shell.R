#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for running shell command.
#----------------------------------------------------------------------------

.log_output <- function(desc, out_arr, log_fun){
  lines <- c()
  for(line in out_arr){
    if(nchar(line) > 0){
      log_fun("%s output: %s", desc, line)
      lines <- c(lines, line)
    }
  }
  return(lines)
}


#'
#' Runs command and collects it's return code using subprocess library.
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
get_cmd_output <- function(desc, cmd, ..., log_debug = FALSE) {
  full_cmd <- sprintf(cmd, ...)
  cmd_split <- strsplit(full_cmd, split = ' ')[[1]]

  log_fun <- if(log_debug) { pkg_logdebug } else { pkg_logfinest }
  log_fun("%s cmd: %s", desc, full_cmd)


  con <- spawn_process(
    command = Sys.which(cmd_split[1]),
    arguments = cmd_split[2:length(cmd_split)]
  )
  tryCatch({
    while(process_state(con) == 'running'){
      ln <- process_read(con, PIPE_STDOUT, timeout = 1000)
      .log_output(desc, ln, log_fun)
    }
  }, finally = {
    ln <- process_read(con, PIPE_STDERR)
    .log_output(desc, ln, pkg_logwarn)
    ret_code <- process_return_code(con)
    # TODO if null it prints empty space as status code
    if (is.null(ret_code) || ret_code > 0) {
      log_fun("Process terminated with code: %s.", ret_code)
    }
  })
  return(ret_code)
}

#' TODO DELETE THIS, use get_cmd_output
#'
#' Runs command and collects it's output lines
#'
#' Logs command to run and all output to finest level.
#'
#' @param desc description of command to prefix logged messages. (type: character)
#' @param cmd command to run. Can contain formating expessions. (type: character)
#' @param ... parameters to build cmd using sprintf.
#' @param log_debug if TRUE will log onto DEBUG level else FINEST log level
#'   will be used. (type: logical(1), default: FALSE)
#'
#' @return character(N) containing command output lines.
#'
#' @keywords internal
#'
get_cmd_lines <- function(desc, cmd, ..., log_debug = FALSE) {
  full_cmd <- paste0(sprintf(cmd, ...), " 2>&1")

  log_fun <- if(log_debug) { pkg_logdebug } else { pkg_logfinest }
  log_fun("%s cmd: %s", desc, full_cmd)

  lines <- character(0)
  con <- pipe(full_cmd, open = "rt")
  tryCatch({
    while(TRUE) {
      ln <- readLines(con, n = 1, skipNul = T)
      if (!length(ln) || !nchar(ln)) {
        break
      }
      log_fun("%s output: %s", desc, ln)
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

  cmd0 <- get_rscript_path(rver = ifelse(is.na(rver), current_rver(), rver)) # from 97_rversion.R

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
