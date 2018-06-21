#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities to support RSuite as a whole: like adapters registration or RSuite
# version management.
#----------------------------------------------------------------------------

#'
#' Checks if newer version of RSuite is available.
#'
#' @return NULL if newer version is not available or newest available version number.
#'
#' @family miscellaneous
#'
#' @examples
#' \donttest{
#'   # print latest version available or NULL if latest is currently installed
#'   rsuite_check_version()
#' }
#'
#' @export
#'
rsuite_check_version <- function() {
  pkgs <- suppressWarnings({
    utils::available.packages(repos = "https://wlog-rsuite.s3.amazonaws.com", filters = list())
  })
  pkgs <- data.frame(pkgs, stringsAsFactors = FALSE, row.names = NULL)[, c("Package", "Version")]
  pkgs <- pkgs[pkgs$Package == "RSuite", ]
  if (!nrow(pkgs)) {
    return()
  }

  max_norm_ver <- gsub("-", ".", max(norm_version(pkgs$Version)))
  cur_norm_ver <- gsub("-", ".", norm_version(as.character(utils::packageVersion("RSuite"))))
  if (max_norm_ver <= cur_norm_ver) {
    return()
  }

  max_ver <- denorm_version(max(norm_version(pkgs$Version)))
  return(max_ver)
}

#'
#' Updates RSuite to newest available version.
#'
#' @param lib.dir folder path to install RSuite into. Folder must exist.
#'   (type: character(1); default: \code{Sys.getevn("R_LIBS_USER")})
#'
#' @return TRUE if updated (invisible).
#'
#' @family miscellaneous
#'
#' @examples
#' \donttest{
#'   lib_dir <- tempfile("Rsuite_")
#'   dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
#'
#'   rsuite_update(lib_dir)
#' }
#'
#' @export
#'
rsuite_update <- function(lib.dir = Sys.getenv("R_LIBS_USER")) {
  ver <- rsuite_check_version()
  if (is.null(ver)) {
    pkg_loginfo("Latest version of RSuite installed.")
    return(invisible(FALSE))
  }
  pkg_loginfo("Installing v%s of RSuite...", ver)

  rver <- current_rver() # from 97_rversion.R

  pkg_types <- unique(c(.Platform$pkgType, "source"))
  repo_infos <- build_repo_infos( # from 53_repositories.R
    spec = list(
      CRAN = "https://cloud.r-project.org/",
      S3 = "https://wlog-rsuite.s3.amazonaws.com"),
    types = pkg_types,
    rver = rver)
  log_repo_infos(repo_infos)     # from 53_repositories.R

  rsuite_ver <- vers.build("RSuite", vmin = ver, vmax = ver)

  pkg_loginfo("Resolving dependencies (for R %s)...", rver)
  avail_vers <- resolve_dependencies(rsuite_ver, # from 11_install_prj_deps.R
                                     repo_infos = repo_infos, pkg_types = pkg_types)
  stopifnot(avail_vers$has_avails())

  prev_lib_path <- .libPaths()
  tryCatch({
    .libPaths(lib.dir) # install it globally or in user env

    install_dependencies(avail_vers,
                         lib_dir = .libPaths()[[1]], # install into default location
                         rver = rver,
                         check_repos_consistency = FALSE)
  },
  finally = {
    .libPaths(prev_lib_path)
  })

  pkg_loginfo("All done.")
  invisible(TRUE)
}

#'
#' Registers repository adapter to use for projects.
#'
#' @param repo_adapter object complying rsuite_repo_adapter signature.
#'
#' @family miscellaneous
#'
#' @examples
#' \donttest{
#'   repo_adapter <- repo_adapter_create_base("Own") # create your custom adapter
#'   class(repo_adapter) <- c("repo_adapter_own", class(repo_adapter))
#'   rsuite_register_repo_adapter(repo_adapter)
#' }
#'
#' @export
#'
rsuite_register_repo_adapter <- function(repo_adapter) {
  assert(!is.null(repo_adapter) && is_repo_adapter(repo_adapter),
         "Repo adapter object expected for repo_adapter")
  assert(is.null(find_repo_adapter(repo_adapter$name)),
         "Repo adapter '%s' is already registered", repo_adapter$name)

  reg_repo_adapter(repo_adapter$name, repo_adapter)
}

#'
#' Gets all names of known repository adapters.
#'
#' @return names of registered repository management adapters as character vector.
#'
#' @family miscellaneous
#'
#' @examples
#' rsuite_get_repo_adapter_names()
#'
#' @export
#'
rsuite_get_repo_adapter_names <- function() {
  reg_repo_adapter_names()
}


#'
#' Registers RC (revision control) adapter to use for projects.
#'
#' @param rc_adapter object complying rsuite_rc_adapter signature.
#'
#' @family miscellaneous
#'
#' @examples
#' rc_adapter <- rc_adapter_create_base("Own") # create your custom adapter
#' class(rc_adapter) <- c("rc_adapter_own", class(rc_adapter))
#'
#' # register it
#' rsuite_register_rc_adapter(rc_adapter)
#'
#' # unregister it
#' rsuite_unregister_rc_adapter("Own")
#'
#' @export
#'
rsuite_register_rc_adapter <- function(rc_adapter) {
  assert(!is.null(rc_adapter) && is_rc_adapter(rc_adapter),
         "RC adapter object expected for rc_adapter")
  assert(is.null(find_rc_adapter(rc_adapter$name)),
         "RC adapter '%s' is already registered", rc_adapter$name)

  reg_rc_adapter(rc_adapter$name, rc_adapter)
}

#'
#' Unregisters RC (revision control) adapter.
#'
#' @param name RC adapter name to unregister.
#'
#' @family miscellaneous
#'
#' @examples
#' rc_adapter <- rc_adapter_create_base("Own") # create your custom adapter
#' class(rc_adapter) <- c("rc_adapter_own", class(rc_adapter))
#'
#' # register it
#' rsuite_register_rc_adapter(rc_adapter)
#'
#' # unregister it
#' rsuite_unregister_rc_adapter("Own")
#'
#' @export
#'
rsuite_unregister_rc_adapter <- function(name) {
  assert(!is.null(name) && is.character(name) && length(name) == 1 && nchar(name) > 0,
         "Non empty character(1) required for name")
  reg_rc_adapter(name, NULL)
}


#'
#' Gets all names of known RC (revision control) adapters.
#'
#' @return names of registered rc adapters as character vector.
#'
#' @family miscellaneous
#'
#' @examples
#' rsuite_get_rc_adapter_names()
#'
#' @export
#'
rsuite_get_rc_adapter_names <- function() {
  reg_rc_adapter_names()
}



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
#' rsuite_get_templates()
#'
#' @export
#'
rsuite_get_templates <- function() {
  result <- get_templates()
  return(result)
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
#' @param name name of the template being created
#' (type: character)
#'
#' @param path path to the directory where the template should be created
#' (type: character, default: NA)
#'
#' @family miscellaneous
#'
#' @examples
#' rsuite_start_prj_template("prjtemplate")
#'
#' @export
#'
rsuite_start_prj_template <- function(name = NULL,
                                      path = NA) {
  if (is.na(path)) {
    path <- get_user_templ_base_dir(create = TRUE) # from 98_shell.R
    assert(!is.null(path),
           paste("User template folder is not specified.",
                  "Please set the rsuite.user_templ_path option to point to the folder containing user templates",
                  sep = " "))
  }

  assert(is.character(path) && length(path) == 1, "character(1) expected for path")
  assert(dir.exists(path), "Directory %s does not exists", path)
  assert(!is.null(name), "No template name specified")
  assert(is.character(name) && length(name) == 1 && nchar(name) > 0,
         "non empty character(1) expected for name")
  assert(!grepl("[\\/\"\'<>_]+", name),
         "Invalid template name %s. It must not contain special characters", name)

  start_prj_template(name, path) # from 58_templates
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
#' @param name name of the template being created
#' (type: character)
#'
#' @param path path to the directory where the template should be created
#' (type: character, default: NA)
#'
#' @examples
#' rsuite_start_pkg_template("pkgtemplate")
#'
#' @export
#'
rsuite_start_pkg_template <- function(name = NULL,
                                      path = NA) {
  if (is.na(path)) {
    path <- get_user_templ_base_dir(create = TRUE) # from 98_shell.R
    assert(!is.null(path), paste0(
      "User template folder is not specified.",
      "Please set the rsuite.user_templ_path option to point to the folder containing user templates",
      sep = " "))
  }

  assert(is.character(path) && length(path) == 1, "character(1) expected for path")
  assert(dir.exists(path), "Directory %s does not exist", path)
  assert(!is.null(name), "No template name specified")
  assert(is.character(name) && length(name) == 1 && nchar(name) > 0,
         "non empty character(1) expected for name")
  assert(!grepl("[\\/\"\'<>_]+", name),
         "Invalid template name %s. It must not contain special characters", name)

  start_pkg_template(name, path)
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
#' rsuite_register_template("../MyExistingTemplate")
#'
#' @export
#'
rsuite_register_template <- function(path = NULL, global = FALSE) {
  assert(!is.null(path), "No template path specified.")
  assert(dir.exists(path), "Directory does not exist.")

  if (global) {
    tmpl_dir <- get_global_tmpl_dir()
    assert(!is.null(tmpl_dir), "Global template directory error.")
  } else{
    tmpl_dir <- get_user_templ_base_dir(create = TRUE)
    assert(!is.null(tmpl_dir), "Local templates directory is not defined(rsuite.user_templ_path)")
  }

  success <- file.copy(from = path, to = tmpl_dir, recursive = TRUE) # from 14_setup_structure.R
  assert(all(success), "Faile to copy %s to %s", path, tmpl_dir)
  pkg_loginfo("%s template was registered successfully", path)
}
