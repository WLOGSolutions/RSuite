#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for managing project/package templates
#----------------------------------------------------------------------------

#'
#' Returns the filepath to the requested project template. The function
#' looks for the template according to the following rules:
#'     - at first the tmpl parameter is treated as a direct filepath to the template
#'     - if there is no directory under the filepath, the function assumes that template
#'       can be found in the default template directory
#'
#' Additionaly if the function is looking is searching for the 'builtin template' it will
#' look for it in the extdata directory in the RSuite package
#'
#'
#' @param tmpl name of the project template
#'    (type: character).
#'
#' @return filepath to the requested project template
#'    (type: character).
#'
#' @keywords internal
#' @noRd
#'
get_prj_tmpl_dir <- function(tmpl) {
  assert(!is.null(tmpl) && is.character(tmpl) && length(tmpl) == 1 && nchar(tmpl) > 0,
         "Non empty character(1) required for template name")

  # treat the argument as the filepath to the template
  prj_tmpl_dir <- tmpl

  # look for templates in the default template directory
  if (!dir.exists(prj_tmpl_dir)) {
    prj_tmpl_dir <- file.path(get_tmpl_dir(), tmpl, "project")
  }

  # look for the builtin template in the extdata directory
  if (tmpl == "builtin" && !dir.exists(prj_tmpl_dir)) {
    prj_tmpl_dir <- system.file(file.path("extdata", "prj_template"), package = "RSuite")
  }

  return(prj_tmpl_dir)
}


#'
#' Returns the filepath to the requested package template
#'
#' @param tmpl name of the package template
#'    (type: character).
#'
#' @return filepath to the requested package template
#'    (type: character).
#'
#' @keywords internal
#' @noRd
#'
get_pkg_tmpl_dir <- function(tmpl) {
  assert(!is.null(tmpl) && is.character(tmpl) && length(tmpl) == 1 && nchar(tmpl) > 0,
         "Non empty character(1) required for name")

  # treat the argument as the filepath to the template
  pkg_tmpl_dir <- tmpl

  # look for templates in the base cache directory
  if (!dir.exists(pkg_tmpl_dir)) {
    pkg_tmpl_dir <- file.path(get_tmpl_dir(), tmpl, "package")
  }

  # look for the builtin template in the extdata directory
  if (tmpl == "builtin" && !dir.exists(pkg_tmpl_dir)) {
    pkg_tmpl_dir <- system.file(file.path("extdata", "pkg_template"), package = "RSuite")
  }


  return(pkg_tmpl_dir)
}

#'
#' Checks whether the project template contains required files and directories.
#' All project templates are required to contain a PARAMETERS file
#'
#' @param tmpl name of project template
#'    (type: character).
#'
#' @return TRUE if the template satisfies requirements.
#'
#' @keywords internal
#' @noRd
#'

check_prj_tmpl <- function(tmpl) {
  tmpl_dir <- get_prj_tmpl_dir(tmpl)
  if (!dir.exists(tmpl_dir)) {
    pkg_logerror("%s template does not exist", tmpl_dir)
    return(FALSE)
  }

  required_files <- c("PARAMETERS")
  files <- list.files(tmpl_dir, include.dirs = TRUE, recursive = FALSE)

  requirements_check <- required_files %in% files
  result <- all(requirements_check)

  if (!result) {
    pkg_logerror("%s, template does not contain all required files: %s",
                 tmpl, required_files[!requirements_check])
  }

  return(result)
}


#'
#' Checks whether the package template contains required files and directories
#' All package templates are required to contain a DESCRIPTION files:
#'
#' @param tmpl name of the package template
#'    (type: character).
#'
#' @return TRUE if the template satisfies requirements.
#'
#' @keywords internal
#' @noRd
#'

check_pkg_tmpl <- function(tmpl) {
  tmpl_dir <- get_pkg_tmpl_dir(tmpl)
  if (!dir.exists(tmpl_dir)) {
    pkg_logerror("%s template does not exist.", tmpl_dir)
    return(FALSE)
  }

  required_files <- c("NAMESPACE")
  files <- list.files(tmpl_dir, include.dirs = TRUE, recursive = FALSE)

  requirements_check <- required_files %in% files
  result <- all(requirements_check)

  if (!result) {
    pkg_logerror("%s, template does not contain all required files: %s",
                 tmpl_dir, required_files[!requirements_check])
  }

  return(all(required_files %in% files))
}


#'
#' Replaces markers in the given input using keywords
#'
#' @param keywords list of data to replace markers with.
#'    (type: list).
#'
#' @param input lines with markers to replace.
#'    (type: character(N))
#'
#'
#' @return input with markers replaced.
#'    (type: character(N))
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


#'
#' Returns the default template directory:
#'     - Windows systems: $TEMP/.rsuite/templates
#'     - UNIX systems: /etc/.rsuite/templates
#'
#' @return filepath to default template directory.
#'    (type: character)
#'
#' @keywords internal
#' @noRd
#'
get_tmpl_dir <- function() {
  cache_base_dir <- get_cache_base_dir() # from 98_shell.R
  tmpl_dir <- file.path(cache_base_dir, "templates")

  if (!dir.exists(tmpl_dir)) {
    dir.create(tmpl_dir, recursive = TRUE)
  }

  return(tmpl_dir)
}


get_global_tmpl_dir <- function() {
  tmpl_dir <- NULL

  if (.Platform$OS.type == "unix") {
    tmpl_dir <- file.path("/etc/.rsuite/templates")
  }

  return(tmpl_dir)
}


get_templates <- function() {

}


start_prj_template <- function(name, path) {
  # check permission
  assert(file.access(path, -1), "User has no write permissions to %s", path)

  # check if an existing template won't be overwritten
  tmpl_path <- file.path(path, name, "project")
  assert(!dir.exists(tmpl_path), "%s folder already exists.", normalizePath(tmpl_path))

  builtin_template <- system.file(file.path("extdata", "prj_template"), package = "RSuite")
  copy_folder(builtin_template, tmpl_path) # from 14_setup_structure.R

  pkg_loginfo("%s template was created successfully", name)
}
