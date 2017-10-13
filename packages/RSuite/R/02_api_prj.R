#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Package API related to projects.
#----------------------------------------------------------------------------

#'
#' @keywords internal
#'
#' Detect project base dir in parents of path.
#'
detect_prj_path <- function(path) {
  stopifnot(is.character(path) && length(path) == 1)
  stopifnot(dir.exists(path))

  prj_path <- normalizePath(path)
  while(!file.exists(file.path(prj_path, "PARAMETERS"))) {
    parent_path <- dirname(prj_path)
    assert(parent_path != prj_path, "Failed to detect project base folder from %s", path)
    prj_path <- parent_path
  }

  return(prj_path)
}

#'
#' @keywords internal
#'
#' Validates if prj is project.
#' If prj is NULL tries initilize project at working dir.
#' If fails tries loaded project.
#' If no loaded project asserts that could not detect project out of context.
#'
safe_get_prj <- function(prj, prj_param_name = "prj") {
  assert(is.null(prj) || is_prj(prj),
         "Project(rsuite_project object) expected as %s", prj_param_name)

  if (!is.null(prj)) {
    return(prj)
  }
  prj <- tryCatch({
    prj_init()
  }, error = function(e) {
    NULL
  })

  if (!is.null(prj)) {
    return(prj)
  }
  prj <- get_loaded_prj()
  if (!is.null(prj)) {
    return(prj)
  }

  assert(!is.null(prj), "Could not detect project out of context")
  return(prj)
}


#'
#' Loads project settings without loading it into environment.
#'
#' Project parameters are searched and loaded. If project has been loaded
#' previously from the path the same project instance will be used without
#' reloading.
#' It project is the first one loaded it will became default project (used then
#' NULL is passed as project for project management functions).
#'
#' @param path path to start searching project base folder from. Search is
#'   performed upwards folder structure. Should be existing directory.
#'   (type: character, defalt: getwd())
#' @return object of type rsuite_project
#'
#' @examples
#' prj <- prj_init() # tries to init project somethere from working folder up.
#'
#' @export
#'
prj_init <- function(path = getwd()) {
  assert(is.character(path) && length(path) == 1,
         "character(1) expected for path parameter")
  assert(dir.exists(path),
         "Existing directory path is expected for path parameter")

  prj_path <- detect_prj_path(path)

  prj <- find_prj(prj_path)
  if (!is.null(prj)) {
    return(prj)
  }

  prj <- list(
    load_params = function() {
      assert(dir.exists(prj_path), "Project folder seams to be gone: %s", prj_path)
      load_prj_parameters(prj_path)
    },
    path = prj_path
  )
  class(prj) <- 'rsuite_project'

  prj <- register_prj(prj)
  invisible(prj)
}

#'
#' Checks if object is rsuite project.
#'
#' @param prj object to check for beeing rsuite project.
#' @return TRUE if prj is rsuite project.
#'
#' @export
#'
is_prj <- function(prj) {
  class(prj) == 'rsuite_project'
}

#'
#' Creates project structure at specified path.
#'
#' Project is not loaded, just created
#'
#' If name passed folder under such name will be created and project structure
#' will be placed under it. If not passed folder under path will contain project
#' structure and project name will be assumed to be basename of path.
#'
#' @param name name of project to create. It must not contain special characters
#'   like \\/\"\'<> otherwise project folder could not be created. It can be NULL.
#'   If so project will be created at path directly with name of the forst folder.
#'   (type: character).
#' @param path path to folder there project structure should be created.
#' @param skip_rc if TRUE skip adding project under revision control.
#'   (type: logical, default: FALSE)
#'
#' @return rsuite_project object for project just created.
#'
#' @export
#'
prj_start <- function(name = NULL, path = getwd(), skip_rc = FALSE) {
  assert(is.character(path) && length(path) == 1, "character(1) expected for path")
  assert(dir.exists(path), "Directory %s does not exists", path)
  assert(is.logical(skip_rc), "logical(1) expected for skip_rc")

  if (is.null(name)) {
    assert(dirname(path) != path, "Cannot create project at root directory")
    prj_dir <- path
  } else {
    assert(is.character(name) && length(name) == 1 && nchar(name) > 0,
           "non empty character(1) expected for name")
    assert(!grepl("[\\/\"\'<>]+", name),
           "Invalid project name %s. It must not contain special characters", name)
    prj_dir <- file.path(path, name)
  }

  if (!dir.exists(prj_dir)) {
    created <- dir.create(prj_dir)
    assert(created, "Failed to create project directory at %s", path)
  }

  check_project_structure(prj_dir) # from 14_setup_structure.R

  pkg_loginfo("Project %s started.", basename(prj_dir))

  prj <- prj_init(path = prj_dir)

  if (!skip_rc) {
    rc_adapter <- detect_rc_adapter(prj_dir)
    if (!is.null(rc_adapter)) {
      pkg_loginfo("Puting project %s under %s control ...", basename(prj_dir), rc_adapter$name)
      rc_adapter_prj_struct_add(rc_adapter, prj$load_params())
      pkg_loginfo("... done")
    } else {
      pkg_logwarn("Failed to detect RC manager for %s", basename(prj_dir))
    }
  }

  invisible(prj)
}

#'
#' Creates package structure inside project.
#'
#' @param name name of package to create. It must not contain special characters
#'    like \\/\"\'<> otherwise package folder could not be created. It must not
#'    contain _ also as it is requirement enforced on R package names. Folder
#'    must not exist.
#'    (type: character).
#' @param prj project object to create package in. If not passed will init
#'    project from working directory. (type: rsuite_project, default: NULL)
#' @param skip_rc if TRUE skip adding package under revision control.
#'    (type: logical, default: FALSE)
#'
#' @export
#'
prj_start_package <- function(name, prj = NULL, skip_rc = FALSE) {
  assert(!is.null(name) && is.character(name) && length(name) == 1 && nchar(name) > 0,
         "Non empty character(1) required for name")
  assert(!grepl("[\\/\"\'<>_]+", name),
         "Invalid package name %s. It must not contain special characters or underscore", name)
  prj <- safe_get_prj(prj)

  params <- prj$load_params()
  pkg_dir <- file.path(params$pkgs_path, name)

  assert(!dir.exists(pkg_dir), "Package folder exists already: %s", pkg_dir)

  create_package_structure(pkg_dir) # from 14_setup_structure.R

  pkg_loginfo("Package %s started in project %s.", name, params$project)

  if (!skip_rc) {
    rc_adapter <- detect_rc_adapter(pkg_dir)
    if (!is.null(rc_adapter)) {
      pkg_loginfo("Puting package %s under %s control ...", name, rc_adapter$name)
      rc_adapter_pkg_struct_add(rc_adapter, params, name)
      pkg_loginfo("... done")
    } else {
      pkg_logwarn("Failed to detect RC manager for %s", name)
    }
  }
}


#'
#' Loads project into environment to use so all master scripts can be used run.
#'
#' @param prj project to load or NULL to use path for new project
#'   initialization. If not path passed project will be inited from working
#'   folder. (type: rsuite_project, default: NULL)
#' @param path if prj is NULL, path will be used to init new project to load.
#'   If passed must be existing folder path. (type: character)
#'
#' @return previously loaded project or NULL if no project have been loaded.
#'
#' @export
#'
prj_load <- function(path, prj = NULL) {
  if (!is.null(prj)) {
    assert(is_prj(prj), "Project(rsuite_project object) expected for prj")
  } else if (missing(path)) {
    prj <- prj_init(path = getwd())
  } else {
    assert(is.character(path) && length(path) == 1,
           "character(1) expected for path parameter")
    assert(dir.exists(path),
           "Existing directory path is expected for path parameter")

    prj_path <- detect_prj_path(path)
    prj <- find_prj(prj_path)
    if (is.null(prj)) {
      prj <- prj_init(path)
    }
  }

  stopifnot(!is.null(prj) && is_prj(prj))

  params <- prj$load_params()
  prev_prj <- set_loaded_prj(prj)

  cur_lpath <- .libPaths()
  cur_lpath <- cur_lpath[!grepl("[\\/]deployment[\\/](libs|sbox)[\\/]?$", cur_lpath)]

  .libPaths(c(params$sbox_path, params$lib_path, cur_lpath))

  invisible(prev_prj)
}

#'
#' Installs all project dependencies.
#'
#' Logs all messages from build onto rsuite logger. Use logging::setLevel to
#'   control logs verbosity. DEBUG level turns on build and download messages.
#'
#' @param prj project to collect dependencies for if not passed will build
#'    project for working directory. (type: rsuite_project, default: NULL)
#' @param clean if TRUE clear environment before installing package dependecies.
#'   (type: logical, default: FALSE)
#'
#' @return TRUE if all build successfully.
#'
#' @export
#'
prj_install_deps <- function(prj = NULL, clean = FALSE) {
  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()

  prev_library <- .Library
  prev_lpath <- .libPaths()
  on.exit({
    .Library <- prev_library
    .libPaths(prev_lpath)
  })
  .libPaths(params$lib_path)
  .Library <- NULL
  
  if (clean) {
    pkg_loginfo("Cleaning up local environment...")
    unlink(file.path(params$lib_path, "*"), recursive = TRUE, force = TRUE)
    pkg_loginfo("Cleaning up local environment... done")
  }

  install_prj_deps(params)
}


#'
#' Checks if all dependencies installed are required by project packages or
#'  master scripts and removed those which are not required any more.
#'
#' @param prj project to clean dependencies of. If not passed will use project
#'    base in working directory. (type: rsuite_project, default: NULL)
#'
#' @export
#'
prj_clean_deps <- function(prj = NULL) {
  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()
  clean_prj_deps(params) # from 11_install_prj_deps.R
}


#'
#' Builds project installing dependencies first if they changed.
#'
#' Logs all messages from build onto rsuite logger. Use logging::setLevel to
#'   control logs verbosity. DEBUG level turns on build and download messages.
#'
#' @param prj project to build if not passed will build project for working
#'    directory. (type: rsuite_project, default: NULL)
#' @param type type of packages to build. If NULL will build platform default.
#'    (type: character)
#'
#' @export
#'
prj_build <- function(prj = NULL, type = NULL) {
  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()
  if (is.null(type)) {
    type <- params$pkgs_type
  }
  build_install_tagged_prj_packages(params, # from 12_build_install_prj_pacakges.R
                                    revision = NULL,
                                    build_type = type)
}

#'
#' Prepares deployment zip tagged with version.
#'
#' It collects all dependecies and project packages installed in local project
#' environment together with master scripts and artifacts and zips them into
#' single zip file.
#'
#' Zip package generated is stamped with version. It can be enforced with zip_ver
#' parameter (zip will have suffix <zip_ver>x in the case). If version is not
#' enforced it is detected out of ZipVersion setting in project PARAMETERS file or
#' from maximal project packages version number. In that case revision numer is
#' appended to version: version number will be <zip_ver>_<rc_ver>. Check for
#' changes in project sources is performed for zip package consistency.
#'
#' Before building zip package project is built. If revision number detected
#' project packages will have version altered: revision will be added as least
#' number to package version.
#'
#' Logs all messages from build onto rsuite logger. Use logging::setLevel to
#'   control logs verbosity. DEBUG level turns on build and download messages.
#'
#' @param prj project object to zip. if not passed will zip loaded project or
#'    default whichever exists. Will init default project from working
#'    directory if no default project exists. (type: rsuite_project, default: NULL)
#' @param path folder path to put output zip into. The folder must exist.
#'    (type: characted: default: getwd())
#' @param zip_ver if passed enforce version of zip package to passed value.
#'    Expected form of version is DD.DD. (type: character, default: NULL)
#'
#' @export
#'
prj_zip <- function(prj = NULL, path = getwd(), zip_ver = NULL) {
  assert(dir.exists(path), "Existing folder expected for path")

  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()
  ver_inf <- detect_zip_version(params, zip_ver) # from 15_zip_project.R
  build_install_tagged_prj_packages(params, # from 12_build_install_prj_pacakges.R
                                    ver_inf$rev,
                                    params$pkgs_type)

  zip_project(params, ver_inf$ver, path) # from 15_zip_project.R
}

