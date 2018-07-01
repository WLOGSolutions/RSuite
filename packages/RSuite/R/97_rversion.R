#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for handling different R versions.
#----------------------------------------------------------------------------

#'
#' Retrieves current R version numeber in form X.X.
#'
#' @return version number detected (type: character)
#'
#' @keywords internal
#' @noRd
#'
current_rver <- function() {
  paste(c(R.version$major, unlist(strsplit(R.version$minor, ".", fixed = TRUE))[1]), collapse = ".")
}

#'
#' Takes R version and changes extracts major and first minor part of it.
#'
#' @param rver R version number. Can be full in form X.X.X (type: character).
#'
#' @return version number in form X.X (type: character)
#'
#' @keywords internal
#' @noRd
#'
majmin_rver <- function(rver) {
  gsub("(\\d+\\.\\d+)\\.\\d+", "\\1", rver)
}

#'
#' Detects RScript path for R verion passed. If exact version not found detects
#' version compatible on major.minor version number.
#'
#' Detects version out of PATH variable and registy entries on windows.
#'
#' @param rver R version number to detect Rscript path for (type: character)
#'
#' @return full path to Rscript command with appropriate version.
#'
#' @keywords internal
#' @noRd
#'
get_rscript_path <- function(rver) {
  stopifnot(is_nonempty_char1(rver))

  rscript_cmd <- ifelse(get_os_type() == "windows", "Rscript.exe", "Rscript")
  if (majmin_rver(rver) == current_rver()) {
    return(file.path(R.home("bin"), rscript_cmd))
  }

  candidate_path <- NA
  candidate_ver <- NA

  bin_paths <- get_rscript_search_paths(rscript_cmd)
  for (b in bin_paths) {
    rscript_path <- file.path(b, rscript_cmd)
    stopifnot(file.exists(rscript_path))


    b_ver <- tryCatch({
      output <- get_cmd_outlines("Rscript detection", "%s --version", rscript_path) # from 98_shell.R
      gsub("^.+ (\\d+\\.\\d+\\.\\d+) .+$", "\\1", output)
    },
    error = function(e) NA)

    if (is.na(b_ver)) {
      next
    }

    is_exact_ver <- (majmin_rver(rver) == rver && majmin_rver(b_ver) == majmin_rver(rver)) || (b_ver == rver)
    if (is_exact_ver) {
      return(rscript_path)
    }
    if (majmin_rver(b_ver) == majmin_rver(rver)) {
      candidate_path <- rscript_path
      candidate_ver <- b_ver
    }
  }

  assert(!is.na(candidate_path),
         sprintf("Failed to detect R v%s. Please fix run environment.", rver))
  pkg_logwarn("No exact R v%s found. Will use compatible R v%s.", rver, candidate_ver)
  return(candidate_path)
}

#'
#' Retrieves all path to check Rscripts from. All paths returned are ensured to
#' contain passed command.
#'
#' @param rscript_cmd command to check for existence in candidate folders. (type: character)
#'
#' @return vector of paths containing command. (type: character)
#'
#' @keywords internal
#' @noRd
#'
get_rscript_search_paths <- function(rscript_cmd) {
  paths <- unlist(strsplit(Sys.getenv("PATH"), split = .Platform$path.sep, fixed = TRUE))

  if (get_os_type() == "windows") {
    machine_regs <- tryCatch({
      reg_ents <- unlist(utils::readRegistry("SOFTWARE\\R-core\\R", hive = "HLM", maxdepth = 2))
      unique(file.path(reg_ents[grepl("InstallPath$", names(reg_ents))], "bin"))
    },
    error = function(e) NULL)

    user_regs <- tryCatch({
      reg_ents <- unlist(utils::readRegistry("SOFTWARE\\R-core\\R", hive = "HCU", maxdepth = 2))
      unique(file.path(reg_ents[grepl("InstallPath$", names(reg_ents))], "bin"))
    },
    error = function(e) NULL)

    paths <- c(machine_regs, user_regs, paths)
  }

  found <- paths[file.exists(file.path(paths, rscript_cmd))]
  found <- unique(rsuite_fullUnifiedPath(found))
  return(found)
}
