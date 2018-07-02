#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Package API related to system requirements.
#----------------------------------------------------------------------------

#'
#' Prints out all system requirements from dependencies and project packages.
#'
#' @param prj project object to collect sys requirements for. If not passed will
#'    use loaded project or default whichever exists. Will init default project
#'    from working directory if no default project exists.
#'    (type: rsuite_project, default: NULL)
#' @return named list with named with package names and containing system
#'    requirements as value.
#'
#' @family in SYSREQS
#'
#' @examples
#' # create exemplary project base folder
#' prj_base <- tempfile("example_")
#' dir.create(prj_base, recursive = TRUE, showWarnings = FALSE)
#'
#' # start project
#' prj <- prj_start("my_project", skip_rc = TRUE, path = prj_base)
#'
#' # add package to the project
#' prj_start_package("mypackage", prj = prj)
#'
#' # add system requirements specification
#' write("SystemRequirements: some requirement",
#'       file = file.path(prj$path, "packages", "mypackage", "DESCRIPTION"),
#'       append = TRUE)
#'
#' # list content of pkgzip created
#' sysreqs_collect(prj)
#'
#' @export
#'
sysreqs_collect <- function(prj = NULL) {
  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()
  # no need to check R version here. We will not build anything

  pkg_loginfo("Detecting repositories (for R %s)...", params$r_ver)

  repo_infos <- get_all_repo_infos(params) # from 53_repositories.R
  log_repo_infos(repo_infos) # from 53_repositories.R

  avail_vers <- resolve_prj_deps(repo_infos, params)
  pkg_loginfo("Detected %s dependencies. Processing...", length(vers.get_names(avail_vers)))

  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = TRUE)
  on.exit({
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
  },
  add = TRUE)

  result <- list()

  dloaded <- pkg_download(avail_vers$avails, dest_dir = tmp_dir)
  for (r in seq_len(nrow(dloaded))) {
    dl <- dloaded[r, ]
    dcf <- get_pkg_desc(dl$Package, dl$Path) # from 51_pkg_info.R

    if (!is.null(dcf$SystemRequirements)) {
      result[[dl$Package]] <- gsub("\n", " ", dcf$SystemRequirements)
    }
  }

  prj_packages <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  if (length(prj_packages) > 0) {
    pkg_loginfo("Processing project packages...")
    for (pkg in names(prj_packages)) {
      dcf <- get_pkg_desc(pkg, file.path(params$pkgs_path, prj_packages[[pkg]])) # from 51_pkg_info.R

      if (!is.null(dcf$SystemRequirements)) {
        result[[pkg]] <- gsub("\n", " ", dcf$SystemRequirements)
      }
    }
  }

  pkg_loginfo("Done.")
  return(result)
}

#'
#' Checks for system requirements availability.
#'
#' Collects system requirements with \code{\link{sysreqs_collect}}
#' and performs checks for their existence. Fill fail if some system
#' requirements are not satisfied.
#'
#' @param prj project object to check sys requirements for. If not passed will
#'    use loaded project or default whichever exists. Will init default project
#'    from working directory if no default project exists.
#'    (type: rsuite_project, default: NULL)
#'
#' @family in SYSREQS
#'
#' @examples
#' # create exemplary project base folder
#' prj_base <- tempfile("example_")
#' dir.create(prj_base, recursive = TRUE, showWarnings = FALSE)
#'
#' # start project
#' prj <- prj_start("my_project", skip_rc = TRUE, path = prj_base)
#'
#' # add dependency to XML
#' write("library(XML)",
#'       file = file.path(prj$path, "R", "master.R"),
#'       append = TRUE)
#'
#' \donttest{
#'   # check if requirements or XML are satisfied
#'   sysreqs_check(prj)
#' }
#'
#' @export
#'
sysreqs_check <- function(prj = NULL) {
  sysreqs <- sysreqs_collect(prj)
  if (length(sysreqs) == 0) {
    pkg_loginfo("No system requirements detected to check.")
    return(invisible())
  }

  recipe <- build_sysreqs_check_recipe() # from 57_sysreqs_recipies.R
  recipe <- sysreqs_recipe_collect_all(recipe, sysreqs) # from 57_sysreqs_recipies.R
  result <- perform(recipe)

  assert(length(result$notools) + length(result$nolibs) == 0,
         "Some system requirements are not satisfied")

  pkg_loginfo("All system requirements satisfied.")
}


#'
#' Updates system to satisfy detected requirements.
#'
#' Collects system requirements with \code{\link{sysreqs_collect}}
#' and builds/installs them.
#'
#' @param prj project object to handle sys requirements for. If not passed will
#'    use loaded project or default whichever exists. Will init default project
#'    from working directory if no default project exists.
#'    (type: rsuite_project, default: NULL)
#'
#' @family in SYSREQS
#'
#' @examples
#' # create exemplary project base folder
#' prj_base <- tempfile("example_")
#' dir.create(prj_base, recursive = TRUE, showWarnings = FALSE)
#'
#' # start project
#' prj <- prj_start("my_project", skip_rc = TRUE, path = prj_base)
#'
#' # add dependency to XML
#' write("library(XML)",
#'       file = file.path(prj$path, "R", "master.R"),
#'       append = TRUE)
#'
#' \donttest{
#'   # check if requirements or XML are satisfied
#'   sysreqs_install(prj)
#' }
#'
#' @export
#'
sysreqs_install <- function(prj = NULL) {
  sysreqs <- sysreqs_collect(prj)
  if (length(sysreqs) == 0) {
    pkg_loginfo("No system requirements detected to install.")
    return(invisible())
  }

  params <- safe_get_prj(prj)$load_params()

  recipe <- build_sysreqs_install_recipe(params$prj_path) # from 57_sysreqs_recipies.R
  recipe <- sysreqs_recipe_collect_all(recipe, sysreqs) # from 57_sysreqs_recipies.R
  perform(recipe)
}

#'
#' Creates script to update system to satisfy project requirements.
#'
#' Collects system requirements with \code{\link{sysreqs_collect}} and creates
#' script to builds/install them. It creates .cmd script for Windows and bash
#' script for Linuxes.
#'
#' @param prj project object to process sys requirements for. If not passed will
#'    use loaded project or default whichever exists. Will init default project
#'    from working directory if no default project exists.
#'    (type: rsuite_project, default: NULL)
#'
#' @return invisible path to script file created or NULL if no system
#'   requirements detected.
#'
#' @examples
#' \donttest{
#'   # create exemplary project base folder
#'   prj_base <- tempfile("example_")
#'   dir.create(prj_base, recursive = TRUE, showWarnings = FALSE)
#'
#'   # start project
#'   prj <- prj_start("my_project", skip_rc = TRUE, path = prj_base)
#'
#'   # add dependency to XML
#'   write("library(XML)",
#'         file = file.path(prj$path, "R", "master.R"),
#'         append = TRUE)
#'
#'   # generate script
#'   sysreqs_fpath <- sysreqs_script(prj)
#'
#'   # present script contents
#'   cat(readLines(sysreqs_fpath), sep = "\n")
#' }
#'
#' @export
#'
sysreqs_script <- function(prj = NULL) {
  sysreqs <- sysreqs_collect(prj)
  if (length(sysreqs) == 0) {
    pkg_loginfo("No system requirements detected to install.")
    return(invisible())
  }

  params <- safe_get_prj(prj)$load_params()

  recipe <- build_sysreqs_script_recipe(params$prj_path) # from 57_sysreqs_recipies.R
  recipe <- sysreqs_recipe_collect_all(recipe, sysreqs) # from 57_sysreqs_recipies.R
  script <- perform(recipe)

  pkg_loginfo("Building script created at %s.", script)

  return(invisible(script))
}
