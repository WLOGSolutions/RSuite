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

  if (.Platform$OS.type == "linux") {
    glob_tmpl_dir <- file.path("etc", ".rsuite", "templates")
    base_tmpl_dirs <- c(base_tmpl_dirs, glob_tmpl_dir)
  }

  if (tmpl == "builtin") {
    builtin_tmpl_dir <- system.file(file.path("extdata", "builtin_templates"), package = "RSuite")
    base_tmpl_dirs <- c(base_tmpl_dirs, builtin_tmpl_dir)
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

  required_files <- c("DESCRIPTION", "NAMESPACE", "NEWS")
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
    Date = as.character(Sys.Date()),
    User = iconv(Sys.info()[["user"]], from = "utf-8", to = "latin1")
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
#' Returns the default template directory: 'cache_folder'/templates
#'
#' @return filepath to default template directory.
#'    (type: character)
#'
#' @keywords internal
#' @noRd
#'
get_tmpl_dir <- function() {
  tmpl_dir <- get_cache_dir("templates") # from 98_shell.R

  if (.Platform$OS.type == "unix") {
    tmpl_dir <- file.path("/etc/.rsuite/templates")
  }

  if (!dir.exists(tmpl_dir)) {
    dir.create(tmpl_dir, recursive = TRUE)
  }

  return(tmpl_dir)
}
