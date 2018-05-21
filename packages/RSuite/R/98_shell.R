#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for running shell command.
#----------------------------------------------------------------------------

#'
#' Runs command and collects it's return code using subprocess library.
#'
#' Logs command to run and all output to finest level.
#'
#' @param desc description of command to prefix logged messages. (type: character(1))
#' @param cmd command to run. Can contain formating expessions. (type: character)
#' @param ... parameters to build cmd using sprintf.
#' @param log_debug if TRUE will log onto DEBUG level else FINEST log level
#'   will be used. (type: logical(1), default: FALSE)
#'
#' @return integer(1) containing retcode of the process.
#'
#' @keywords internal
#' @noRd
#'
get_cmd_retcode <- function(desc, cmd, ..., log_debug = FALSE) {
  .log_single_out <- function(desc, out_arr, log_fun) {
    for (single_out in out_arr) {
      for (line in single_out) {
        if (nchar(line) > 0) {
          log_fun("%s output: %s", desc, line)
        }
      }
    }
  }
  .split_cmd <- function(cmd) {
    args <- c()
    quote_arg <- NULL
    for (t in unlist(strsplit(cmd, "\\s+"))) {
      if (!is.null(quote_arg)) {
        if (grepl("[\"']$", t)) {
          args <- c(args, paste(quote_arg, substring(t, 1, nchar(t) - 1)))
          quote_arg <- NULL
        } else {
          quote_arg <- paste(quote_arg, t)
        }
      } else {
        if (grepl("^[\"']", t)) {
          quote_arg <- substring(t, 2)
        } else {
          args <- c(args, t)
        }
      }
    }
    if (!is.null(quote_arg)) {
      args <- c(args, quote_arg)
    }
    return(args)
  }

  log_fun <- if (log_debug) {
    pkg_logdebug
  } else {
    pkg_logfinest
  }
  full_cmd <- sprintf(cmd, ...)
  log_fun("%s cmd: %s", desc, cmd)

  args <- .split_cmd(full_cmd)
  cmd_path <- Sys.which(args[1])
  assert(file.exists(cmd_path), "Command %s is not available", cmd)
  con <- spawn_process(command = cmd_path, arguments = args[2:length(args)])
  tryCatch({
    while (process_state(con) == "running") {
      out_arr <- process_read(con, PIPE_BOTH, timeout = 3000)
      .log_single_out(desc, out_arr$stdout, log_fun)
      .log_single_out(desc, out_arr$stderr, log_fun)
    }
  },
  finally = {
    ret_code <- process_return_code(con)
  })
  return(ret_code)
}

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
#' @noRd
#'
get_cmd_outlines <- function(desc, cmd, ..., log_debug = FALSE) {
  full_cmd <- paste0(sprintf(cmd, ...), " 2>&1")

  log_fun <- if (log_debug) pkg_logdebug else pkg_logfinest
  log_fun("%s cmd: %s", desc, full_cmd)

  lines <- character(0)
  con <- pipe(full_cmd, open = "rt")
  tryCatch({
    while (TRUE) {
      ln <- readLines(con, n = 1, skipNul = TRUE)
      if (!length(ln) || !nchar(ln)) {
        break
      }
      log_fun("%s output: %s", desc, ln)
      lines <- c(lines, ln)
    }
  },
  finally = {
    close(con)
  })

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
#' @param log_debug if TRUE will log onto DEBUG level else FINEST log level
#'   will be used. (type: logical(1), default: TRUE)
#'
#' @return NULL if succeded, if failed returns FALSE or error string.
#'
#' @keywords internal
#' @noRd
#'
run_rscript <- function(script_code, ..., rver = NA, ex_libpath = NULL, log_debug = TRUE) {
  full_code <- sprintf(paste0(script_code, collapse = ";"), ...)

  cmd0 <- get_rscript_path(rver = ifelse(is.na(rver), current_rver(), rver)) # from 97_rversion.R

  old_libs_user <- Sys.getenv("R_LIBS_USER")
  Sys.unsetenv("R_LIBS_USER") # required to prevent Rscript detecting own user libraries
  on.exit({
    Sys.setenv(R_LIBS_USER = old_libs_user)
  },
  add = TRUE)

  script <- sprintf(paste0(".Library <- NULL;",
                           ".libPaths(c(%s, Sys.getenv('R_LIBS_USER'), .Library.site));",
                           "cat('Lib paths:\\n');",
                           "void <- lapply(.libPaths(), function(lp) { cat(paste0('\\t', lp, '\\n')) });",
                           "tryCatch({",
                           "  suppressWarnings({ %s });",
                           "  cat(sprintf('~ done\\n'))",
                           "}, error = function(e) {",
                           "  cat(sprintf('~ error:%%s\\n', e))",
                           "})"),
                    rscript_arg("new", rsuite_fullUnifiedPath(ex_libpath)), full_code)

  rscript_cmd <- paste(cmd0, "--no-init-file", "--no-site-file", "-e", shQuote(script), "2>&1")
  log_fun <- if (log_debug) pkg_logdebug else pkg_logfinest
  log_fun("> cmd: %s", rscript_cmd)

  con <- pipe(rscript_cmd, open = "rt")

  result <- tryCatch({
    ok <- FALSE
    while (TRUE) {
      ln <- readLines(con, n = 1, skipNul = T)
      if (!length(ln)) {
        break
      }

      if (grepl("^~ error:", ln)) {
        ok <- sub("^~ error:", "", ln)
      } else if (ln == "~ done") {
        ok <- NULL
      } else {
        log_fun("> %s", ln)
      }
    }
    ok
  },
  error = function(e) FALSE,
  finally = {
    close(con)
  })

  return(result)
}

#'
#' Creates string which can be passed as parameter to script code.
#'
#' @keywords internal
#' @noRd
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
