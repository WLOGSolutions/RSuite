#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for management of project/package templates.
#----------------------------------------------------------------------------

#'
#' Returns all available project/package templates
#'
#' @details
#' Project templates have to include a PARAMETERS file
#' Package templates have to include the following files: DESCRIPTION
#'
#' All templates can be found in folder pointed by rsuite.user_templ_path option.
#'
#' @return names of the registered project and package templates together with their
#' file path
#'
#' @family in templates management
#'
#' @examples
#' tmpl_list_registered()
#'
#' @export
#'
tmpl_list_registered <- function() {
  templ_base_dirs <- c()

  # look for templates in the local user's environment
  user_tmpl_dir <- get_user_templ_base_dir(create = FALSE) # from 58_templates.R
  if (!is.null(user_tmpl_dir)) {
    user_tmpl_dir <- normalizePath(user_tmpl_dir, winslash = "/")
    templ_base_dirs <- c(templ_base_dirs, user_tmpl_dir)
  } else {
    pkg_logwarn(paste0("User template folder is not specified.",
                       " Please set the rsuite.user_templ_path option",
                       " to point to the folder containing user templates"))
  }

  # look for templates in the global environment
  global_templ_dir <- get_global_templ_dir() # from 58_templates.R
  if (!is.null(global_templ_dir)) {
    templ_base_dirs <- c(templ_base_dirs, global_templ_dir)
  }

  if (length(templ_base_dirs) > 0) {
    templ_dirs <- list.dirs(templ_base_dirs, full.names = TRUE, recursive = FALSE)
    templ_names <- basename(templ_dirs)
  } else {
    templ_dirs <- c()
    templ_names <- c()
  }

  templates <- data.frame(
    Name = c(templ_names, "builtin"),
    HasProjectTemplate = c(dir.exists(file.path(templ_dirs, "project")), TRUE),
    HasPackageTemplate = c(dir.exists(file.path(templ_dirs, "package")), TRUE),
    Path = c(templ_dirs, get_builtin_templs_zip()),  # from 58_templates.R
    stringsAsFactors = FALSE
  )
  templates <- templates[order(rownames(templates)), ] # enforce same ordering as in templ_dirs
  return(templates)
}


#'
#' Creates a new template with the specified name, in the specified path.
#'
#' @details
#' Project templates are required to include a PARAMETERS file
#' whereas package templates are required to include a DESCRIPTION file
#'
#' If there is no path argument provided. The function will create the
#' template in working directory.
#'
#' @param name name of the template being created. (type: character(1))
#' @param path path to the directory where the template should be created. If
#'   NULL will use the folder with user template. (type: character(1), default: getwd())
#' @param add_prj if TRUE include project template to the template directory
#' @param add_pkg if TRUE include package template in the template directory
#' @param base_tmpl name of the package and/or project template (or path to it) to use for
#'    template creation. (type: character(1); default: builtin).
#'
#' @family in templates management
#'
#' @examples
#' tmpl_dir <- tempfile("templ_")
#' tmpl_start(basename(tmpl_dir), path = tempdir())
#'
#' @export
#'
tmpl_start <- function(name, path = getwd(), add_prj = TRUE, add_pkg = TRUE, base_tmpl = "builtin") {
  assert(is.character(path) && length(path) == 1, "character(1) expected for path")
  assert(dir.exists(path), "Directory %s does not exists", path)
  assert(file.access(path, mode = 2) == 0, "User has no write permission to %s", path)

  assert(!missing(name), "Template name is required")
  assert(is.character(name) && length(name) == 1 && nchar(name) > 0,
         "non empty character(1) expected for name")
  assert(!grepl("[\\/\"\'<> ]+", name),
         "Invalid template name '%s'. It must not contain special characters", name)

  path <- normalizePath(path, winslash = "/")

  # create template directory
  tmpl_path <- file.path(path, name)
  if (!dir.exists(tmpl_path)) {
    success <- dir.create(tmpl_path, recursive = TRUE)
    assert(success, "Failed to create directory %s", tmpl_path)
  }

  # finally create templates
  # create project template
  if (add_prj) {
    prj_tmpl_path <- file.path(tmpl_path, "project")
    assert(!dir.exists(prj_tmpl_path), "%s template already exists.", prj_tmpl_path)

    prj_base_tmpl_dir <- get_prj_tmpl_dir(base_tmpl) # from 58_templates.R
    assert(!is.null(prj_base_tmpl_dir) || base_tmpl == "builtin",
           "Requested to start project template from unknown base template '%s'.", base_tmpl)

    if (is.null(prj_base_tmpl_dir)) {
      builtin_temp <- get_builtin_templs_temp_base() # from 58_templates.R
      on.exit({
        unlink(builtin_temp, recursive = TRUE, force = TRUE)
      },
      add = TRUE)
      prj_base_tmpl_dir <- file.path(builtin_temp, "project")
    } else {
      validate_prj_tmpl_struct(prj_base_tmpl_dir) # from 58_templates.R
    }

    success <- copy_folder(prj_base_tmpl_dir, prj_tmpl_path) # from 98_shell.R
    assert(success, "Failed to create project template at %s", prj_tmpl_path)
    pkg_loginfo("%s project template was created successfully", name)
  }

  # create package template
  if (add_pkg) {
    pkg_tmpl_path <- file.path(tmpl_path, "package")
    assert(!dir.exists(pkg_tmpl_path), "%s folder already exists.", pkg_tmpl_path)

    pkg_base_tmpl_dir <- get_pkg_tmpl_dir(base_tmpl) # from 58_templates.R
    assert(!is.null(pkg_base_tmpl_dir) || base_tmpl == "builtin",
           "Requested to start package template from unknown base template '%s'.", base_tmpl)

    if (is.null(pkg_base_tmpl_dir)) {
      builtin_temp <- get_builtin_templs_temp_base() # from 58_templates.R
      on.exit({
        unlink(builtin_temp, recursive = TRUE, force = TRUE)
      },
      add = TRUE)
      pkg_base_tmpl_dir <- file.path(builtin_temp, "package")
    } else {
      validate_pkg_tmpl_struct(pkg_base_tmpl_dir) # from 58_templates.R
    }

    success <- copy_folder(pkg_base_tmpl_dir, pkg_tmpl_path) # from 98_shell.R
    assert(success, "Failed to create package template at %s", pkg_tmpl_path)
    pkg_loginfo("%s package template was created successfully", name)
  }

  pkg_loginfo("%s template was created successfully", name)
}


#'
#' Registers the template specified with the path argument.
#'
#' @details
#' All templates have specific requirements:
#'     Project templates have to contain a PARAMETERS file.
#'     Package templates have to contain a DESCRIPTION file.
#'
#' The user's local template directory is taken from the
#' rsuite.user_templ_path option. The global template is specified
#' as '/etc/.rsuite/templates' and only concerns Linux platforms
#'
#' @family miscellaneous
#'
#' @param path path to the directory where the template should be created
#' (type: character, default: NA)
#'
#' @param global flag specifying if the template will be registered in the user's
#' local template directory (taken from rsuite.user_templ_path) or in the global
#' template directory (/etc/.rsuite/templates on Linux platforms)
#'
#' @examples
#' \donttest{
#'   # setup
#'   old_option_value <- getOption("rsuite.user_templ_path")
#'   tmpl_dir <- tempfile("user_templates_")
#'   dir.create(tmpl_dir, recursive = TRUE, showWarnings = FALSE)
#'
#'   options(rsuite.user_templ_path = tmpl_dir)
#'   user_templ <- tempfile("usr_templ_")
#'
#'   # initialize template from builtin
#'   tmpl_start(basename(user_templ), path = tempdir())
#'   # register it
#'   tmpl_register(user_templ)
#'
#'   # clean up
#'   options(rsuite.user_templ_path = old_option_value)
#'   unlink(tmpl_dir, recursive = TRUE, force = TRUE)
#'   unlink(user_templ, recursive = TRUE, force = TRUE)
#' }
#'
#' @export
#'
tmpl_register <- function(path = NULL, global = FALSE) {
  assert(!is.null(path), "No template path specified.")
  assert(dir.exists(path), "Directory %s does not exist.", path)

  path <- rsuite_fullUnifiedPath(path, short = FALSE)
  if (global) {
    tmpl_dir <- get_global_templ_dir()
    assert(!is.null(tmpl_dir), "Global templates directory is not available.")
  } else{
    tmpl_dir <- get_user_templ_base_dir(create = TRUE) # from 58_templates.R
    assert(!is.null(tmpl_dir), "Local templates directory is not defined(rsuite.user_templ_path)")
  }

  if (dir.exists(file.path(tmpl_dir, basename(path)))) {
    pkg_loginfo("Overwriting existing template: %s", basename(path))
  }

  success <- file.copy(from = path, to = tmpl_dir, recursive = TRUE) # from 14_setup_structure.R
  assert(all(success), "Failed to copy %s to %s", path, tmpl_dir)

  pkg_loginfo("%s template was registered successfully", path)
}
