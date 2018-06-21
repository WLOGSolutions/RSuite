#'
#' Returns all available of project/package templates
#'
#' @details
#' Project templates have to include a PARAMETERS file
#' Package templates have to include the following files: DESCRIPTION
#'
#' All templates can be found in folder pointed by rsuite.user_templ_path option.
#'
#' @return names of registered project and package templates together with their
#' filepath
#'
#' @family miscellaneous
#'
#' @examples
#' template_get()
#'
#' @export
#'
template_get <- function() {
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

  # look for builtin templates
  builtin_templ_dir <- get_builtin_templ_dir() # from 58_templates.R
  templ_dirs <- c(templ_dirs, builtin_templ_dir)
  templ_names <- c(templ_names, "builtin")

  templates <- data.frame(
    Name = templ_names,
    HasProjectTemplate = dir.exists(file.path(templ_dirs, "project")),
    HasPackageTemplate = dir.exists(file.path(templ_dirs, "package")),
    Path = templ_dirs,
    stringsAsFactors = FALSE
  )
  templates <- templates[order(rownames(templates)), ] # enforce same ordering as in templ_dirs
  return(templates)
}

#'
#' Creates a new project template with the specified name, in the specified path.
#'
#' @details
#' Project templates are required to include a PARAMETERS file
#'
#' If there is no path argument provided. The function will create the
#' template in the default template folder in the user's local environment
#'
#' @param name name of the template being created. (type: character(1))
#'
#' @param path path to the directory where the template should be created. If
#'   NULL will use folder with user template. (type: character(1), default: NULL)
#'
#' @family miscellaneous
#'
#' @examples
#' template_prjadd("prjtemplate", path = tempdir())
#'
#' @export
#'
template_prjadd <- function(name, path = NULL) {
  if (is.null(path)) {
    path <- get_user_templ_base_dir(create = TRUE) # from 58_templates.R
    assert(!is.null(path),
           paste0("User template folder is not specified.",
                  " Please set the rsuite.user_templ_path option to point to the folder containing user templates"))
  }

  assert(is.character(path) && length(path) == 1, "character(1) expected for path")
  assert(dir.exists(path), "Directory %s does not exists", path)
  assert(file.access(path, mode = 2) == 0, "User has no write permission to %s", path)

  assert(!missing(name), "Template name is required")
  assert(is.character(name) && length(name) == 1 && nchar(name) > 0,
         "non empty character(1) expected for name")
  assert(!grepl("[\\/\"\'<>_]+ ", name),
         "Invalid template name '%s'. It must not contain special characters", name)

  path <- normalizePath(path, winslash = "/")

  # create template directory
  tmpl_path <- file.path(path, name)
  if (!dir.exists(tmpl_path)) {
    success <- dir.create(tmpl_path, recursive = TRUE)
    assert(success, "Failed to create directory %s", tmpl_path)
  }

  # finally create project template
  prj_tmpl_path <- file.path(path, name, "project")
  assert(!dir.exists(prj_tmpl_path), "%s template already exists.", prj_tmpl_path)

  builtin_prj_template <- file.path(get_builtin_templ_dir(), # from 58_templates.R
                                    "project")
  success <- file.copy(from = builtin_prj_template, to = dirname(prj_tmpl_path), recursive = TRUE, copy.mode = TRUE)
  assert(success, "Failed to copy default builtin template to %s", prj_tmpl_path)

  pkg_loginfo("%s template was created successfully", name)
}

#'
#' Creates a new package template with the specified name, in the specified path.
#'
#' @details
#' Package templates are required to include a DESCRIPTION file
#'
#' If there is no path argument provided. The function will create the
#' template in the default template folder in the user's local environment
#'
#' @family miscellaneous
#'
#' @param name name of the template being created. (type: character(1))
#'
#' @param path path to the directory where the template should be created. If
#'   NULL will use folder with user template. (type: character(1), default: NULL)
#'
#' @examples
#' template_pkgadd("pkgtemplate", path = tempdir())
#'
#' @export
#'
template_pkgadd <- function(name, path = NULL) {
  if (is.null(path)) {
    path <- get_user_templ_base_dir(create = TRUE) # from 58_templates.R
    assert(!is.null(path),
           paste0("User template folder is not specified.",
                  " Please set the rsuite.user_templ_path option to point to the folder containing user templates"))
  }

  assert(is.character(path) && length(path) == 1, "character(1) expected for path")
  assert(dir.exists(path), "Directory %s does not exist", path)
  assert(file.access(path, mode = 2) == 0, "User has no write permission to %s.", path)

  assert(!missing(name), "Template name is required")
  assert(is.character(name) && length(name) == 1 && nchar(name) > 0,
         "Non empty character(1) expected for name")
  assert(!grepl("[\\/\"\'<>_ ]+", name),
         "Invalid template name '%s'. It must not contain special characters", name)

  path <- normalizePath(path, winslash = "/")

  # create template directory
  tmpl_path <- file.path(path, name)
  if (!dir.exists(tmpl_path)) {
    success <- dir.create(tmpl_path, recursive = TRUE)
    assert(success, "Failed to create directory %s", tmpl_path)
  }

  pkg_tmpl_path <- file.path(path, name, "package")
  assert(!dir.exists(pkg_tmpl_path), "%s folder already exists.", pkg_tmpl_path)

  builtin_template <- file.path(get_builtin_templ_dir(), "package") # from 58_templates.R
  success <- copy_folder(builtin_template, pkg_tmpl_path) # from 98_shell.R
  assert(success, "Failed to create template at %s", pkg_tmpl_path)

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
#' as '/etc/.rsuite/templates' and only concerns linux platforms
#'
#' @family miscellaneous
#'
#' @param path path to the directory where the template should be created
#' (type: character, default: NA)
#'
#' @param global flag specifying if the template will be registerd in the user's
#' local template directory (taken from rsuite.user_templ_path) or in the global
#' template directory (/etc/.rsuite/templates on Linxu platforms)
#'
#' @examples
#' template_register("../MyExistingTemplate")
#'
#' @export
#'
template_register <- function(path = NULL, global = FALSE) {
  assert(!is.null(path), "No template path specified.")
  assert(dir.exists(path), "Directory %s does not exist.", path)

  if (global) {
    tmpl_dir <- get_global_tmpl_dir()
    assert(!is.null(tmpl_dir), "Global template directory error.")
  } else{
    tmpl_dir <- get_user_templ_base_dir(create = TRUE) # from 58_templates.R
    assert(!is.null(tmpl_dir), "Local templates directory is not defined(rsuite.user_templ_path)")
  }

  success <- file.copy(from = path, to = tmpl_dir, recursive = TRUE) # from 14_setup_structure.R
  assert(all(success), "Failed to copy %s to %s", path, tmpl_dir)

  pkg_loginfo("%s template was registered successfully", path)
}
