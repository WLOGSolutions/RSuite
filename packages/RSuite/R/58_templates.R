#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for managing project/package templates
#----------------------------------------------------------------------------

#'
#' Returns the filepath to the requested project template
#'
#' @param tmpl name of the project template
#'    (type: character).
#'
#' @return filepath to the requested project template
#'
#' @keywords internal
#' @noRd
#'
get_prj_tmpl_dir <- function(tmpl) {
  assert(!is.null(tmpl) && is.character(tmpl) && length(tmpl) == 1 && nchar(tmpl) > 0,
         "Non empty character(1) required for template name")

  prj_tmpl_dir <- tmpl

  if (tmpl == "builtin" && !dir.exists(prj_tmpl_dir)) {
    prj_tmpl_dir <- system.file(file.path("extdata", "prj_template"), package = "RSuite")
  }

  if (!dir.exists(prj_tmpl_dir)) {
    prj_tmpl_dir <- file.path(get_tmpl_dir(), tmpl, "project")
  }

  assert(dir.exists(prj_tmpl_dir), "%s template does not exists", tmpl)
  return(prj_tmpl_dir)
}


#'
#' Returns the filepath to the requested package template
#'
#' @param tmpl name of the package template
#'    (type: character).
#'
#' @return filepath to the requested package template
#'
#' @keywords internal
#' @noRd
#'
get_pkg_tmpl_dir <- function(tmpl) {
  assert(!is.null(tmpl) && is.character(tmpl) && length(tmpl) == 1 && nchar(tmpl) > 0,
         "Non empty character(1) required for name")
  pkg_tmpl_dir <- tmpl

  if (tmpl == "builtin" && !dir.exists(pkg_tmpl_dir)) {
    pkg_tmpl_dir <- system.file(file.path("extdata", "pkg_template"), package = "RSuite")
  }

  if (!dir.exists(pkg_tmpl_dir)) {
    pkg_tmpl_dir <- file.path(get_tmpl_dir(), tmpl, "package")
  }

  assert(dir.exists(pkg_tmpl_dir), "%s template does not exist", tmpl)
  return(pkg_tmpl_dir)
}

#'
#' Checks whether the project template contains required files and directories
#'
#' @param prj_tmpl name of project template
#'    (type: character).
#'
#' @return TRUE if the template satisfies requirements.
#'
#' @keywords internal
#' @noRd
#'

check_prj_tmpl <- function(tmpl) {
  tmpl_dir <- get_prj_tmpl_dir(tmpl)
  assert(dir.exists(tmpl_dir), "%s template does not exist.", tmpl_dir)

  required_files <- c("PARAMETERS")
  files <- list.files(tmpl_dir, include.dirs = TRUE, recursive = FALSE)

  return(all(required_files %in% files))
}


#'
#' Checks whether the package template contains required files and directories
#'
#' @param tmpl name of project template
#'    (type: character).
#'
#' @return TRUE if the template satisfies requirements.
#'
#' @keywords internal
#' @noRd
#'

check_pkg_tmpl <- function(tmpl) {
  tmpl_dir <- get_pkg_tmpl_dir(tmpl)
  assert(dir.exists(tmpl_dir), "%s template does not exist.", tmpl_dir)

  required_files <- c("DESCRIPTION", "NAMESPACE", "NEWS")
  files <- list.files(tmpl_dir, include.dirs = TRUE, recursive = FALSE)

  return(all(required_files %in% files))
}


#'
#' Creates a project based on the given template.
#'
#' @param prj_dir project base directory
#'    (type: character).
#'
#' @param tmpl name of project template
#'    (type: character).
#'
#' @return TRUE if the template satisfies requirements.
#'
#' @keywords internal
#' @noRd
#'
create_prj_structure_from_tmpl <- function(prj_dir, tmpl) {
  assert(check_prj_tmpl(tmpl), "%s does not satisfy project template requirements.", tmpl)

  # copy template
  tmpl_dir <- get_prj_tmpl_dir(tmpl)
  files <- list.files(tmpl_dir, all.files = TRUE, no.. = TRUE)

  success <- file.copy(file.path(tmpl_dir, files), prj_dir, copy.mode = TRUE, recursive = TRUE)
  assert(length(success) > 0, "Failed to copy template files.")

  # now replace markers in files
  files <- list.files(prj_dir, full.names = TRUE, include.dirs = FALSE, recursive = TRUE)
  files <- files[!file.info(files)$isdir]

  keywords <- c(
    ProjectName = basename(prj_dir),
    RSuiteVersion = as.character(utils::packageVersion("RSuite")),
    RVersion = current_rver(), # from 97_rversion.R
    User = as.character(Sys.Date()),
    user = iconv(Sys.info()[["user"]], from = "utf-8", to = "latin1")
  )

  for (f in files) {
    lines <- readLines(con = f, warn = FALSE)
    lines <- replace_markers(keywords, lines)
    writeLines(lines, con = f)
  }

  success <- file.rename(files, replace_markers(keywords, files))
  assert(length(success) > 0, "Failed to rename files in template.")
}


#'
#' Replaces markers in the given input using keywords
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
replace_markers <- function(keywords, input) {
  keyword_data <- data.frame(Marker = names(keywords), Keyword = keywords)
  keyword_data$Marker <- sprintf("__%s__", keyword_data$Marker)

  for (i in seq_len(nrow(keyword_data))) {
    input <- gsub(keyword_data$Marker[i], keyword_data$Keyword[i], input)
  }

  return(input)
}


get_tmpl_dir <- function() {
   cache_base_dir <- get_cache_base_dir() # from 98_shell.R
    tmpl_dir <- file.path(cache_base_dir, "templates")

    if (!dir.exists(tmpl_dir)) {
      dir.create(tmpl_dir, recursive = TRUE)
    }

    return(tmpl_dir)
}
