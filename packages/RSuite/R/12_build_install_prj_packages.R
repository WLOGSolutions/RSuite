#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities to support project packages building.
#----------------------------------------------------------------------------

#'
#' Builds and installs project packages.
#'
#' Performes check if all dependencies are installed in local project environment.
#'
#' If revision passed (not NULL) tags project packages with revision as least
#' version number before building.
#'
#' @param params project parameters(type: rsuite_project_params)
#' @param revision revision to tag packages with before building (type: character)
#' @param build_type type of packages to build. (type: character)
#'
#' @keywords internal
#'
build_install_tagged_prj_packages <- function(params, revision, build_type) {
  if (!is.null(revision)) {
    bkp_info <- backup_pkgdesc_files(params$pkgs_path) # from 51_pkg_info.R

    pkg_revs <- lapply(X = retrieve_project_pkgsvers(params$pkgs_path), # from 51_pkg_info.R
                       FUN = function(ver) { paste0(ver, "-", revision) })
    update_project_pkgsvers(params$pkgs_path, pkg_revs) # from 51_pkg_info.R

    on.exit({ restore_pkgdesc_files(bkp_info) }, add = T)
  }

  prj_pkgs <- build_project_pkgslist(params$pkgs_path)

  # check if environment has to be rebuilt
  uninstDeps <- collect_uninstalled_direct_deps(params) # from 52_dependencies.R
  uninstDeps <- vers.rm(uninstDeps, prj_pkgs)
  assert(vers.is_empty(uninstDeps),
         paste0("Some dependencies are not installed in project env: %s.",
                " Please, install dependencies(Call RSuite::prj_install_deps)"),
         paste(vers.get_names(uninstDeps), collapse = ", "))

  build_install_prj_packages(params, build_type)

  out_path <- rsuite_contrib_url(params$irepo_path, build_type, params$r_ver)
  rsuite_write_PACKAGES(out_path, build_type)
}

#'
#' Clears internal repository, builds project packages, installs them
#' into environment. Raises exception if failed to build anything.
#'
#' @keywords internal
#'
build_install_prj_packages <- function(params, build_type) {
  # Cleaning previous build results
  intern_repo_path <- params$get_intern_repo_path()
  unlink(file.path(intern_repo_path, "*"), recursive = TRUE, force = TRUE)

  contrib_url <- rsuite_contrib_url(intern_repo_path, type = build_type, rver = params$r_ver)
  if (!dir.exists(contrib_url)) {
    dir.create(contrib_url, recursive=TRUE)
    rsuite_write_PACKAGES(contrib_url, type = build_type)
  }

  project_packages <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  if (length(project_packages) == 0) {
    pkg_loginfo("Nothing to be done.")
    return(invisible())
  }

  # Adding packages to repo
  void <- lapply(X = project_packages,
                 FUN = function(p) {
                   pkg_loginfo("Installing %s (for R %s) ...", p, params$r_ver)
                   pkg_path <- file.path(params$pkgs_path, p)

                   pkg_file <- pkg_build(pkg_path,
                                         dest_dir = contrib_url,
                                         binary = (build_type != "source"),
                                         rver = params$r_ver,
                                         libpath = params$lib_path)
                   if (is.null(pkg_file)) {
                     return() # Failed to build package
                   }
                   rsuite_write_PACKAGES(contrib_url, build_type)

                   # remove package if installed
                   pkg_remove(p, lib_dir = params$lib_path)

                   repo_url <- sprintf("file:///%s", params$get_intern_repo_path())
                   # this type = "source" does not matter, it is passed just to prevent complaining
                   #  on windows that "both" type cannot be used with repos = NULL
                   pkg_install(pkg_file,
                               lib_dir = params$lib_path,
                               type = "source",
                               repos = NULL,
                               rver = params$r_ver)
                 })

  failed <- setdiff(project_packages, installed.packages(params$lib_path)[, "Package"])
  assert(!length(failed),
         "Failed to install project packages: %s", paste(failed, collapse = ", "))

  pkg_loginfo("Successfuly build %s packages", length(project_packages))
}
