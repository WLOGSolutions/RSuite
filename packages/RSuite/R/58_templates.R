#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for managing project/package templates
#----------------------------------------------------------------------------

#'
#' Returns the filepath to the requested project template
#'
#' @param prj_tmpl name of the project template
#'
#' @return filepath to the requested project template
#'
#' @keywords internal
#' @noRd
#'
get_prj_tmpl_dir <- function(prj_tmpl) {
  prj_tmpl_dir <- prj_tmpl
  if (!dir.exists(prj_tmpl_dir)) {
    cache_base_dir <- get_cache_base_dir() # from 98_shell.R
    prj_tmpl_dir <- file.path(cache_base_dir, "templates", "projects", prj_tmpl)
  }

  if (prj_tmpl == "builtin" && !dir.exists(prj_tmpl_dir)) {
    prj_tmpl_dir <- system.file(file.path("extdata", "prj_template"), package = "RSuite")
  }

  assert(dir.exists(prj_tmpl_dir), "%s template does not exists", prj_tmpl)
  return(prj_tmpl_dir)
}


#'
#' Returns the filepath to the requested package template
#'
#' @param pkg_tmpl name of the package template
#'
#' @return filepath to the requested package template
#'
#' @keywords internal
#' @noRd
#'
get_pkg_tmpl_dir <- function(pkg_tmpl) {
  pkg_tmpl_dir <- pkg_tmpl
  if (!dir.exists(pkg_tmpl_dir)) {
    cache_base_dir <- get_cache_base_dir() # from 98_shell.R
    pkg_tmpl_dir <- file.path(cache_base_dir, "templates", "packages", pkg_tmpl)
  }

  if (pkg_tmpl == "builtin" && !dir.exists(pkg_tmpl_dir)) {
    pkg_tmpl_dir <- system.file(file.path("extdata", "pkg_template"), package = "RSuite")
  }

  assert(dir.exists(pkg_tmpl_dir), "%s template does not exist", pkg_tmpl)
  return(pkg_tmpl_dir)
}

#'
#' Checks whether the project template contains required files and directories
#'
#' @param prj_tmpl name of project template
#'
#' @return TRUE if the template satisfies requirements.
#'
#' @keywords internal
#' @noRd
#'

check_prj_tmpl <- function(prj_tmpl) {
  tmpl_dir <- get_prj_tmpl_dir(prj_tmpl)
  assert(dir.exists(tmpl_dir), "%s template does not exists.", tmpl_dir)

  required_files <- c("PARAMETERS")
  files <- list.files(tmpl_dir, include.dirs = TRUE, recursive = FALSE)

  return(all(required_files %in% files))
}


#'
#' Checks whether the package template contains required files and directories
#'
#' @param pkg_tmpl name of project template
#'
#' @return TRUE if the template satisfies requirements.
#'
#' @keywords internal
#' @noRd
#'

check_pkg_tmpl <- function(pkg_tmpl) {
  tmpl_dir <- get_pkg_tmpl_dir(pkg_tmpl)
  assert(dir.exists(tmpl_dir), "%s template does not exists.", tmpl_dir)

  required_files <- c("DESCRIPTION", "NAMESPACE", "NEWS")
  files <- list.files(tmpl_dir, include.dirs = TRUE, recursive = FALSE)

  return(all(required_files %in% files))
}


#'
#' Creates a project based on the given template.
#'
#' @param prj_dir project base directory
#'
#' @param prj_tmpl name of project template
#'
#' @return TRUE if the template satisfies requirements.
#'
#' @keywords internal
#' @noRd
#'
create_prj_structure_from_tmpl <- function(prj_dir, prj_tmpl) {
  assert(check_prj_tmpl(prj_tmpl), "%s does not satisfy project template requirements.", prj_tmpl)

  # copy template
  tmpl_dir <- get_prj_tmpl_dir(prj_tmpl)
  files <- list.files(tmpl_dir)

  success <- file.copy(file.path(tmpl_dir, files), prj_dir, copy.mode = TRUE, recursive = TRUE)
  assert(length(success) > 0, "Failed to copy template files.")

  # now replace markers in files
  files <- list.files(prj_dir, full.names = TRUE, include.dirs = FALSE, recursive = TRUE)
  files <- files[!file.info(files)$isdir]

  markers <- list("__ProjectName__", "__Date__", "__User__")
  keywords <- list(
    pkg_name = basename(prj_dir),
    today = as.character(Sys.Date()),
    user = iconv(Sys.info()[["user"]], from = "utf-8", to = "latin1")
  )

  for (f in files) {
    lines <- readLines(con = f, warn = FALSE)
    lines <- replace_markers(markers, keywords, lines)
    writeLines(lines, con = f)
  }

  success <- file.rename(files, replace_markers(markers, keywords, files))
  assert(length(success) > 0, "Failed to rename files in template.")
}


#'
#' Replaces markers in the given input using keywords
#'
#' @param markers list of data to markers to replace.
#'
#' @param keywords list of data to replace markers with.
#'
#' @param input lines with markers to replace.
#'
#' @return input with markers replaced.
#'
#' @keywords internal
#' @noRd
#'
replace_markers <- function(markers, keywords, input) {
  stopifnot(length(markers) == length(keywords))
  N <- length(markers)

  for (i in 1:N) {
    input <- gsub(markers[[i]], keywords[[i]], input)
  }

  return(input)
}
