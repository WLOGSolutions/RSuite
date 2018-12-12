#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for running shell command.
#----------------------------------------------------------------------------


#'
#' Splits command onto arguments.
#'
#' @param cmd command to split. (type: character(1))
#' @return character(N) containing arguments.
#'
#' @keywords internal
#' @noRd
#'
split_cmd <- function(cmd) {
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
      if (!grepl("^[\"']", t)) {
        args <- c(args, t)
      } else if (grepl("[\"']$", t)) {
        args <- c(args, substring(t, 2, last = nchar(t) - 1))
      } else {
        quote_arg <- substring(t, 2)
      }
    }
  }
  if (!is.null(quote_arg)) {
    args <- c(args, quote_arg)
  }
  return(args)
}

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
  log_fun <- if (log_debug) {
    pkg_logdebug
  } else {
    pkg_logfinest
  }

  .log_single_out <- function(lines) {
    lines <- unlist(strsplit(lines, "\n"))
    lines <- gsub("^.*\r", "", lines)

    lapply(lines, function(ln) {
      if (nchar(ln, type = "bytes") > 0) {
        log_fun("%s output: %s", desc, ln)
      }
    })
  }

  full_cmd <- sprintf(cmd, ...)
  log_fun("%s cmd: %s", desc, cmd)

  args <- split_cmd(full_cmd)
  cmd_path <- Sys.which(args[1])
  assert(file.exists(cmd_path), "Command %s is not available", cmd)
  p <- processx::process$new(command = cmd_path, args = args[2:length(args)],
                             stdout = "|", stderr = "|", cleanup = TRUE)
  tryCatch({
    repeat {
      p$poll_io(timeout = 1000)
      .log_single_out(p$read_output_lines())
      .log_single_out(p$read_error_lines())
      if (!p$is_alive()) break
    }
    p$poll_io(timeout = 1000)
    .log_single_out(p$read_output_lines())
    .log_single_out(p$read_error_lines())
  },
  finally = {
    ret_code <- p$get_exit_status()
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
  cmd0 <- rsuite_fullUnifiedPath(cmd0)

  # Before unsetting the R_LIBS_USER variable we have to check whether we are
  # going to run a subprocess using a different R version than the one
  # that we're currently using. As the subprocess inherints environment variables
  # from the parent process it may result in errors because one R version might
  # be using enviromental variables defined for the other R version.
  if (!is.na(rver) && current_rver() != rver) {
    old_libs_user <- Sys.getenv("R_LIBS_USER")
    Sys.unsetenv("R_LIBS_USER") # required to prevent Rscript detecting own user libraries
    on.exit({
      Sys.setenv(R_LIBS_USER = old_libs_user)
    },
    add = TRUE)
  }

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
                    rscript_arg("new", rsuite_fullUnifiedPath(ex_libpath)),
                    full_code)
  if (get_os_platform() %in% c("MacOS", "SunOS")) {
    # On MacOS special characters are interpreted by process, so they have to be twice escaped
    # something alike is happening on SunOS
    script <- gsub("\\\\([tn])", "\\\\\\\\\\1", script)
  }

  log_fun <- if (log_debug) pkg_logdebug else pkg_logfinest

  cmd <- paste(c(cmd0, "--no-init-file", "--no-site-file", "-e", shQuote(script), "2>&1"),
               collapse = " ")
  log_fun("> cmd: %s", cmd)

  start_time <- Sys.time()
  con <- pipe(cmd, open = "rt")
  Sys.sleep(0.5)

  result <- tryCatch({
    status <- FALSE
    has_output <- FALSE
    repeat {
      ln <- readLines(con, n = 1, skipNul = TRUE)
      if (length(ln) == 0) {
        if (has_output || as.numeric(Sys.time() - start_time) > 5) {
          # if has output already or is waiting for 5 secs alredy - give up
          break
        }
        Sys.sleep(1.0) # wait subprocess to start
        next
      }
      has_output <- TRUE
      if (nchar(trimws(ln)) == 0) {
        # nothing interesting to log
        next
      }

      log_fun("> %s", ln)
      if (grepl("^~ error:", ln)) {
        status <- sub("^~ error:", "", ln)
      } else if (ln == "~ done") {
        status <- NULL
      }
    }
    status
  },
  error = function(e) {
    log_fun("Error while running script: %s", e)
    FALSE
  },
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
  val <- gsub("\"", "\\\"", val, fixed = TRUE)
  val <- gsub("'", "\\\"", val, fixed = TRUE)
  if (length(val) == 1) {
    sprintf("%s='%s'", name, val)
  } else {
    sprintf("%s=c('%s')", name, paste(val, collapse = "', '"))
  }
}


#'
#' Copies folder from onto folder to if to does not exists.
#'
#' @keywords internal
#' @noRd
#'
copy_folder <- function(from, to) {
  if (basename(from) == basename(to)) {
    success <- file.copy(from = from, to = dirname(to),
                         recursive = TRUE, copy.mode = TRUE, overwrite = FALSE)
    return(invisible(success))
  }

  success <- TRUE

  if (!dir.exists(to)) {
    success <- (dir.create(to, recursive = TRUE, showWarnings = FALSE)
                && success)
  }

  for (ent in list.files(from, all.files = TRUE, recursive = FALSE, include.dirs = TRUE, no.. = TRUE)) {
    file.copy(from = file.path(from, ent), to = to,
              recursive = TRUE, copy.mode = TRUE, overwrite = FALSE)
  }

  expected <- list.files(from, all.files = TRUE, recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
  copied <- list.files(to, all.files = TRUE, recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
  success <- (length(setdiff(expected, copied)) == 0
              && success)

  invisible(success)
}

#'
#' Retrieves path of RSuite cache folder with specified name.
#'
#' Cache folder base path is taken from rsuite.cache_path option. If not specified
#' repositories contents and downloaded packages will not be cached.
#'
#' If folder base is specified and it does not exist or subfolder does not exist,
#' tries to create it.
#'
#' Logs messages (on DEBUG level) if rsuite.cache_path is not set or fails to create
#'  cache folder.
#'
#' @param subname name of subfolder inside cache base folder to retrieve path of.
#'   If NULL cache base folder will be retrieved. (type: character, default: NULL)
#'
#' @return path to cache folder retrieved or NULL if caching is off or
#'   failed to create cache folder.
#'
#' @keywords internal
#' @noRd
#'
get_cache_dir <- function(subname = NULL) {
  cache_dir <- getOption("rsuite.cache_path", "")
  if (nchar(cache_dir) == 0) {
    pkg_logdebug("rsuite.cache_path option is not set; Caching is off.")
    return()
  }

  if (!is.null(subname)) {
    cache_dir <- file.path(cache_dir, subname)
  }

  if (get_os_type() == "windows") {
    cache_dir <- utils::shortPathName(cache_dir)
  }

  if (!dir.exists(cache_dir)) {
    created <- dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (!created) {
      pkg_logdebug("Failed to create folder for caching (%s); Caching is off.", cache_dir)
      return()
    }
  }

  return(cache_dir)
}

#'
#' Retrieves current OS type.
#'
#' @return one of windows, macos, unix or unknown (if failes to detect)
#'
#' @keywords internal
#' @noRd
#'
get_os_type <- function() {
  if (.Platform$OS.type == "windows") {
    return("windows")
  }

  if (grepl("darwin", R.version$os)) {
    return("macos")
  }

  if (.Platform$OS.type == "unix") {
    return("unix")
  }

  return("unknown")
}

#'
#' Retrieves various information on current platform.
#'
#' @return named list od following contents
#' \describe{
#'   \item{type}{One of windows, macos, unix. (type: character)}
#'   \item{platform}{One of Windows, MacOS, SunOS, RedHat, Debian. (type: character(1))}
#'   \item{release}{One of Solaris, Ubuntu, Debian, Fedora, CentOS or RedHat or NA. (type: character(1))}
#'   \item{distrib}{Distribution release e.g. for Debian: squeeze, wheezy, jessie. (type: character(1))}
#'   \item{version}{Version number of the distribution. (type: character(1))}
#' }
#'
#' @keywords internal
#' @noRd
#'
get_os_info <- function() {
  type <- get_os_type()
  platform <- get_os_platform()
  assert(!is.na(platform), "Could not detect current platform name.")

  distrib <- .get_distrib(platform)
  release <- .get_release(distrib)
  version <- .get_os_version(distrib)

  return(list(type = type,
              platform = platform,
              distrib = distrib,
              release = release,
              version = version))
}

#'
#' Retrieves platform identifier: Windows, MacOS, SunOS, RedHat or Debian
#'
#' @return platform identifier retrieved or NA if failed to retrieve.
#'   (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
get_os_platform <- function() {
  os_type <- get_os_type()
  if (os_type == "windows") {
    return("Windows")
  }
  if (os_type == "macos") {
    return("MacOS")
  }
  if (os_type == "unix") {
    if (grepl("^solaris", R.version$os)) {
      return("SunOS")
    }
    if (file.exists("/etc/redhat-release") || file.exists("/etc/fedora-release")) {
      return("RedHat")
    }
    if (file.exists("/etc/debian_version")) {
      return("Debian")
    }
    return(NA_character_)
  }
  return(NA_character_)
}

#'
#' Checks if root is running the current R session on Unix systems
#'
#' @return TRUE if root is running the current R session.
#'   (type: logical(1))
#'
#' @keywords internal
#' @noRd
#'
is_root <- function() {
  if (.Platform$OS.type != "unix") {
    return(FALSE)
  }

  home_dir <- Sys.getenv("HOME")
  return(home_dir == "/root")
}

#'
#' Retrieves distribution for the platform: Solaris, Ubuntu, Debian, Fedora, CentOS, RedHat or MacOS.
#'
#' @param platform current platform identifier (type: character(1))
#' @return platform distribution name or NA if failed to detect. (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
.get_distrib <- function(platform) {
  if (platform == "Debian") {
    if (file.exists("/etc/lsb-release") && any(grepl("DISTRIB[ _]ID=Ubuntu", readLines("/etc/lsb-release")))) {
      return("Ubuntu")
    }
    if (file.exists("/etc/os-release") && any(grepl("ID=debian", readLines("/etc/os-release")))) {
      return("Debian")
    }
    return(NA_character_)
  }

  if (platform == "RedHat") {
    if (file.exists("/etc/fedora-release")) {
      return("Fedora")
    }
    if (!file.exists("/etc/redhat-release")) {
      # unexpected: RPM must have redhat-release file
      return(NA_character_)
    }
    if (file.exists("/etc/os-release") && any(grepl("ID=\"?centos\"?", readLines("/etc/os-release")))) {
      return("CentOS")
    }
    if (file.exists("/etc/redhat-release") && any(grepl("^Red Hat Linux", readLines("/etc/redhat-release")))) {
      return("RedHat")
    }
    return(NA_character_)
  }

  if (platform == "SunOS") {
    return("Solaris")
  }
  if (platform == "MacOS") {
    return("MacOS")
  }
  return(NA_character_)
}

#'
#' Retrieves release name(or number) for passed distribution.
#'
#' @param distrib current distribution to detect release for. (type: character(1))
#' @return release identifier retrieved or NA if failed to detected. (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
.get_release <- function(distrib) {
  if (is.na(distrib)) {
    return(NA_character_)
  }

  if (distrib == "Ubuntu") {
    if (!file.exists("/etc/lsb-release")) {
      return(NA_character_)
    }
    codename <- grep("^DISTRIB[ _]CODENAME=", readLines("/etc/lsb-release"), value = TRUE)
    if (length(codename) != 1) {
      return(NA_character_)
    }
    return(gsub("^.+=([a-z]+).*$", "\\1", codename))
  }

  if (distrib == "Debian") {
    if (!file.exists("/etc/os-release")) {
      return(NA_character_)
    }
    ver <- grep("^VERSION=", readLines("/etc/os-release"), value = TRUE)
    if (length(ver) != 1) {
      return(NA_character_)
    }
    return(gsub("^.+\\(([^)]+)\\).*$", "\\1", ver))
  }

  if (distrib == "CentOS") {
    if (!file.exists("/etc/redhat-release")) {
      return(NA_character_)
    }
    rel_ln <- readLines("/etc/redhat-release")[1]
    if (!grepl("^.+release\\s+([0-9]+[.][0-9]+).+$", rel_ln)) {
      return(NA_character_)
    }
    return(gsub("^.+release\\s+([0-9]+[.][0-9]+).+$", "\\1", rel_ln))
  }

  if (distrib == "RedHat") {
    if (!file.exists("/etc/redhat-release")) {
      return(NA_character_)
    }
    rel_ln <- readLines("/etc/redhat-release")[1]
    if (!grepl("^.+release\\s+([0-9]+([.][0-9]+)?)\\s+\\(([^)]+)\\).*$", rel_ln)) {
      return(NA_character_)
    }
    return(gsub("^.+release\\s+([0-9]+([.][0-9]+)?)\\s+\\(([^)]+)\\).*$", "\\3", rel_ln))
  }

  if (distrib == "Fedora") {
    if (!file.exists("/etc/os-release")) {
      return(NA_character_)
    }

    ver <- grep("^VERSION[ _]ID=[0-9]+([.][0-9]+)$*", readLines("/etc/os-release"), value = TRUE)
    if (length(ver) != 1) {
      return(NA_character_)
    }
    return(gsub("^.+=([0-9]+([.][0-9]+)*)$", "\\1", ver))
  }

  if (distrib == "Solaris") {
    if (!grepl("^solaris[0-9]+[.][0-9]+$", R.version$os)) {
      return(NA_character_)
    }
    return(gsub("^solaris([0-9]+[.][0-9]+)$", "\\1", R.version$os))
  }

  if (distrib == "MacOS") {
    if (grepl("^mac[.]binary[.][a-z-]+$", .Platform$pkgType)) {
      return(gsub("^mac[.]binary[.]([a-z-]+)$", "\\1", .Platform$pkgType))
    }
    if (grepl("^darwin[0-9]+[.][0-9]+.+$", R.version$os)) {
      return(gsub("^darwin([0-9]+[.][0-9]+).+$", "\\1", R.version$os))
    }
    return(NA_character_)
  }

  return(NA_character_)
}

#'
#' Retrieves os version for passed distribution.
#'
#' @param distrib current distribution to detect release for. (type: character(1))
#' @return release identifier retrieved or NA if failed to detected. (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
.get_os_version <- function(distrib) {
  if (is.na(distrib)) {
    return(NA_character_)
  }

  if (distrib == "Ubuntu") {
    if (!file.exists("/etc/lsb-release")) {
      return(NA_character_)
    }
    release <- grep("^DISTRIB[ _]RELEASE=[0-9]+([.][0-9]+)*$", readLines("/etc/lsb-release"), value = TRUE)
    if (length(release) != 1) {
      return(NA_character_)
    }
    return(gsub("^.+=([0-9]+([.][0-9]+)*)$", "\\1", release))
  }

  if (distrib == "Debian") {
    if (!file.exists("/etc/os-release")) {
      return(NA_character_)
    }
    ver <- grep("^VERSION[ _]ID=\"[0-9]+([.][0-9]+)*\"$", readLines("/etc/os-release"), value = TRUE)
    if (length(ver) != 1) {
      return(NA_character_)
    }
    return(gsub("^.+=\"([0-9]+([.][0-9]+)*)\"$", "\\1", ver))
  }

  if (distrib %in% c("RedHat", "CentOS")) {
    if (!file.exists("/etc/redhat-release")) {
      return(NA_character_)
    }
    rel_ln <- readLines("/etc/redhat-release")[1]
    if (!grepl("^.+release\\s+([0-9]+([.][0-9]+)?).+$", rel_ln)) {
      return(NA_character_)
    }
    return(gsub("^.+release\\s+([0-9]+([.][0-9]+)?).+$", "\\1", rel_ln))
  }

  if (distrib == "Fedora") {
    if (!file.exists("/etc/os-release")) {
      return(NA_character_)
    }

    ver <- grep("^VERSION[ _]ID=[0-9]+([.][0-9]+)*$", readLines("/etc/os-release"), value = TRUE)
    if (length(ver) != 1) {
      return(NA_character_)
    }
    return(gsub("^.+=([0-9]+([.][0-9]+)*)$", "\\1", ver))
  }

  if (distrib == "Solaris") {
    if (!grepl("^solaris[0-9]+[.][0-9]+$", R.version$os)) {
      return(NA_character_)
    }
    return(gsub("^solaris([0-9]+[.][0-9]+)$", "\\1", R.version$os))
  }

  if (distrib == "MacOS") {
    if (!grepl("^darwin[0-9]+[.][0-9]+.+$", R.version$os)) {
      return(NA_character_)
    }
    return(gsub("^darwin([0-9]+[.][0-9]+).+$", "\\1", R.version$os))
  }

  return(NA_character_)
}
