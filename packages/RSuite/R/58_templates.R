#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for managing project/package templates
#----------------------------------------------------------------------------

#'
#' Retrieves base folder(s) for template.
#'
#' @param tmpl name of the template in user space. (type: character)
#'
#' @return list of paths in search order there template should be searched for.
#'   (type: character)
#'
#' @keywords internal
#' @noRd
#'
.get_base_tmpl_dirs <- function(tmpl) {
  assert(!is.null(tmpl) && is.character(tmpl) && length(tmpl) == 1 && nchar(tmpl) > 0,
         "Non empty character(1) required for template name")

  base_tmpl_dirs <- c()

  # look for templates in the user templates directory
  user_templ_base_dir <- get_user_templ_base_dir(create = FALSE)
  if (!is.null(user_templ_base_dir)) {
    user_tmpl_dir <- file.path(user_templ_base_dir, tmpl)
    base_tmpl_dirs <- c(base_tmpl_dirs, user_tmpl_dir)
  }

  # look for templates in the global templates directory
  base_tmpl_dirs <- c(base_tmpl_dirs, get_global_templ_dir())

  if (tmpl == "builtin") {
    base_tmpl_dirs <- c(base_tmpl_dirs, get_builtin_templ_dir())
  }

  base_tmpl_dirs <- base_tmpl_dirs[dir.exists(base_tmpl_dirs)]
  return(base_tmpl_dirs)
}

#'
#' Retrieves path to project template.
#'
#' @param tmpl name of the project template or it's path. (type: character)
#'
#' @return path to the requested project template or NULL if not found. (type: character)
#'
#' @keywords internal
#' @noRd
#'
get_prj_tmpl_dir <- function(tmpl) {
  assert(!is.null(tmpl) && is.character(tmpl) && length(tmpl) == 1 && nchar(tmpl) > 0,
         "Non empty character(1) required for template name")

  # treat tmpl as the path to the template
  if (dir.exists(tmpl)) {
    return(tmpl)
  }

  prj_templ_dirs <- file.path(.get_base_tmpl_dirs(tmpl), "project")
  prj_templ_dirs <- prj_templ_dirs[dir.exists(prj_templ_dirs)]

  if (length(prj_templ_dirs) == 0) {
    return(NULL)
  }

  return(prj_templ_dirs[[1]]) # take first available in search path
}


#'
#' Retrieves path to package template.
#'
#' @param tmpl name of the project template or it's path. (type: character)
#'
#' @return path to the requested package template or NULL if not found. (type: character)
#'
#' @keywords internal
#' @noRd
#'
get_pkg_tmpl_dir <- function(tmpl) {
  assert(!is.null(tmpl) && is.character(tmpl) && length(tmpl) == 1 && nchar(tmpl) > 0,
         "Non empty character(1) required for name")

  # treat tmpl as the path to the template
  if (dir.exists(tmpl)) {
    return(tmpl)
  }

  pkg_templ_dirs <- file.path(.get_base_tmpl_dirs(tmpl), "package")
  pkg_templ_dirs <- pkg_templ_dirs[dir.exists(pkg_templ_dirs)]

  if (length(pkg_templ_dirs) == 0) {
    return(NULL)
  }

  return(pkg_templ_dirs[[1]])
}

#'
#' Checks whether the project template contains required files and directories
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
    pkg_logerror("%s template does not exist.", tmpl_dir)
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
  if (!dir.exists(tmpl_dir)) {
    pkg_logerror("%s template does not exist.", tmpl_dir)
    return(FALSE)
  }

  required_files <- c("DESCRIPTION")
  files <- list.files(tmpl_dir, include.dirs = TRUE, recursive = FALSE)

  requirements_check <- required_files %in% files
  result <- all(requirements_check)

  if (!result) {
    pkg_logerror("%s, template does not contain all required files: %s",
                 tmpl, required_files[!requirements_check])
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
#' Retrieves folder where user package and project templates are located.
#'
#' Folder path is taken from rsuite.user_templ_path option. If not specified
#' user templates will not be used.
#'
#' @param create if TRUE will create user templates folder (if specified).
#'   (type: logical(1), default: FALSE)
#'
#' @return path to user templates folder retrieved or NULL if not specified or
#'   failed to create.
#'
#' @keywords internal
#' @noRd
#'
get_user_templ_base_dir <- function(create = FALSE) {
  user_templ_base_dir <- getOption("rsuite.user_templ_path", "")
  if (nchar(user_templ_base_dir) == 0) {
    return()
  }

  if (.Platform$OS.type == "windows") {
    user_templ_base_dir <- utils::shortPathName(user_templ_base_dir)
  }

  if (dir.exists(user_templ_base_dir)) {
    return(user_templ_base_dir)
  }

  if (!any(create)) {
    return()
  }

  if (dir.create(user_templ_base_dir, recursive = TRUE, showWarnings = FALSE)) {
    return(user_templ_base_dir)
  }

  pkg_logwarn("Failed to create folder for user project and package templates (%s)", user_templ_base_dir)
  return()
}

#'
#' Retrieves the folder where global package and project templates are located.
#'
#' This path only concerns Linux users as it returns the '/etc/.rsuite/templates' path
#'
#' @return path to global templates folder retrieved or NULL (if does not exist or
#' working on Windows platform)
#'
#' @keywords internal
#' @noRd
#'
get_global_templ_dir <- function() {
  if (.Platform$OS.type != "unix") {
    return(NULL)
  }

  global_tmpl_dir <- "/etc/.rsuite/templates"
  if (!dir.exists(global_tmpl_dir)) {
    return(NULL)
  }
  return(global_tmpl_dir)
}

#'
#' Retrieves builtin template folder.
#'
#' @return path to builtin templates folder. (type: character)
#'
#' @keywords internal
#' @noRd
#'
get_builtin_templ_dir <- function() {
  return(system.file(file.path("extdata", "builtin_templates", "package"), package = "RSuite"))
}

