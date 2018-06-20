#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for managing project/package templates
#----------------------------------------------------------------------------

#'
#' Retrieves base folder for template.
#'
#' @param tmpl name of the template in user space. (type: character)
#'
#' @return path to the requested template or NULL if template is unknown. (type: character)
#'
#' @keywords internal
#' @noRd
#'
.get_base_tmpl_dir <- function(tmpl) {
  assert(!is.null(tmpl) && is.character(tmpl) && length(tmpl) == 1 && nchar(tmpl) > 0,
         "Non empty character(1) required for template name")

  base_tmpl_dir <- NULL

  # look for templates in the user templates directory
  if (tmpl == "builtin") {
    base_tmpl_dir <- system.file(file.path("extdata", "builtin_templates"), package = "RSuite")
  } else {
    user_templ_base_dir <- get_user_templ_base_dir(create = FALSE)
    if (!is.null(user_templ_base_dir)) {
      base_tmpl_dir <- file.path(user_templ_base_dir, tmpl)
    }
  }

  if (is.null(base_tmpl_dir) || !dir.exists(base_tmpl_dir)) {
    return(NULL)
  }

  return(base_tmpl_dir)

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

  base_templ_dir <- .get_base_tmpl_dir(tmpl)
  if (is.null(base_templ_dir)) {
    return(NULL)
  }

  prj_templ_dir <- file.path(base_templ_dir, "project")
  if (!dir.exists(prj_templ_dir)) {
    return(NULL)
  }

  return(prj_templ_dir)
}


#'
#' Retrieves path to package template.
#'
#' @param tmpl name of the project template or it's path. (type: character)
#' @param tmpl name of the package template
#'    (type: character).
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

  base_templ_dir <- .get_base_tmpl_dir(tmpl)
  if (is.null(base_templ_dir)) {
    return(NULL)
  }

  pkg_templ_dir <- file.path(base_templ_dir, "package")
  if (!dir.exists(pkg_templ_dir)) {
    return(NULL)
  }

  return(pkg_templ_dir)
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


get_templates <- function() {
  # look for builtin templates
  builtin_tmpl_dir <- .get_base_tmpl_dir("builtin")
  tmpls <- builtin_tmpl_dir

  # look for templates in the local user's environment
  local_tmpl_dir <- get_user_templ_base_dir(create = FALSE) # from 98_shell.R
  if (!is.null(local_tmpl_dir)) {
    tmpls <- c(tmpls, list.dirs(local_tmpl_dir, recursive = FALSE,
                            full.names = TRUE))
  } else {
    pkg_logwarn("User template folder is not specified. Please set the rsuite.user_templ_path option to point to the folder containing user templates")
  }
  # look for templates in the global environment
  global_tmpl_dir <- get_global_tmpl_dir() # from 58_templates.R
  if (!is.null(global_tmpl_dir)) {
    tmpls <- c(tmpls, list.dirs(get_global_tmpl_dir(),
                                          recursive = FALSE, full.names = TRUE))
  }

  # case if there is no template and if both directories do not exist
  if (length(tmpls) == 0) {
    pkg_loginfo("No templates available")
    return(invisible())
  }

  # look for project/package templates
  prj_tmpl <- tmpls[dir.exists(file.path(tmpls, 'project'))]
  pkg_tmpl <- tmpls[dir.exists(file.path(tmpls, 'package'))]

  # prepare results
  tmpl_names <- basename(c(prj_tmpl, pkg_tmpl))
  tmpl_types <- c(rep("project", length(prj_tmpl)), rep("package", length(pkg_tmpl)))
  tmpl_paths <- c(prj_tmpl, pkg_tmpl)

  templates <- data.frame(
    Name = tmpl_names,
    Type = tmpl_types,
    Path = tmpl_paths
  )

  return(templates)
}

start_prj_template <- function(name, path) {
  # check permissions
  assert(file.access(path, mode = 2) == 0, "User has no write permission to %s", path)

  # create template directory
  tmpl_path <- file.path(path, name)
  if (!dir.exists(tmpl_path)) {
    success <- dir.create(tmpl_path, recursive = TRUE)
    assert(success, "Failed to create directory %s", normalizePath(tmpl_path))
  }

  # finally create project template
  prj_tmpl_path <- file.path(path, name, "project")
  assert(!dir.exists(prj_tmpl_path), "%s template already exists.", normalizePath(prj_tmpl_path))

  builtin_prj_template <- system.file(file.path("extdata", "builtin_templates", "project"), package = "RSuite")
  success <- file.copy(from = builtin_prj_template, to = dirname(prj_tmpl_path), recursive = TRUE, copy.mode = TRUE)
  assert(success, "Failed to copy default builtin template to %s", prj_tmpl_path)

  pkg_loginfo("%s template was created successfully", name)
}



start_pkg_template <- function(name, path) {
  # check permissions
  assert(file.access(path, mode = 2) == 0, "User has no write permission to %s", path)

  # create template directory
  tmpl_path <- file.path(path, name)
  if (!dir.exists(tmpl_path)) {
    success <- dir.create(tmpl_path, recursive = TRUE)
    assert(success, "Failed to create directory %s", normalizePath(tmpl_path))
  }

  pkg_tmpl_path <- file.path(path, name, "package")
  assert(!dir.exists(pkg_tmpl_path), "%s folder already exists.", normalizePath(pkg_tmpl_path))

  builtin_template <- system.file(file.path("extdata", "builtin_templates", "package"), package = "RSuite")
  copy_folder(builtin_template, pkg_tmpl_path)

  pkg_loginfo("%s template was created successfully", name)
}
