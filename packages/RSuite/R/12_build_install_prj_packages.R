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
#' @param skip_build_steps steps to skip while building packages.
#'   (type: character, default: NULL)
#' @param rebuild if TRUE will force rebuild of packages even if no changes found.
#'   (type: logical, default: FALSE)
#' @param check_repos_consistency if TRUE will check if installed dependencies
#'   are built for another R ver. (type: logical, default: TRUE if R is stable)
#'
#' @keywords internal
#' @noRd
#'
build_install_tagged_prj_packages <- function(params, revision, build_type,
                                              skip_build_steps = NULL,
                                              rebuild = FALSE,
                                              check_repos_consistency = is_r_stable()) {
  if (!is.null(revision)) {
    bkp_info <- backup_pkgdesc_files(params$pkgs_path) # from 51_pkg_info.R

    pkg_revs <- lapply(X = retrieve_project_pkgsvers(params$pkgs_path), # from 51_pkg_info.R
                       FUN = function(ver) paste0(ver, "-", revision))
    update_project_pkgsvers(params$pkgs_path, pkg_revs) # from 51_pkg_info.R

    on.exit(restore_pkgdesc_files(bkp_info), add = TRUE)
  }

  prj_pkgs <- build_project_pkgslist(params$pkgs_path)

  # check if environment has to be rebuilt
  uninst_deps <- collect_uninstalled_direct_deps(params, check_rver = check_repos_consistency) # from 52_dependencies.R
  uninst_deps <- vers.rm(uninst_deps, prj_pkgs)
  assert(vers.is_empty(uninst_deps),
         paste0("Some dependencies are not installed in project env: %s.",
                "Please, install dependencies(Call RSuite::prj_install_deps)"),
         paste(vers.get_names(uninst_deps), collapse = ", "))

  build_install_prj_packages(params, build_type, skip_build_steps, rebuild)

  out_path <- rsuite_contrib_url(params$irepo_path, build_type, params$r_ver)
  rsuite_write_PACKAGES(out_path, build_type)
}

#'
#' Clears internal repository, builds project packages, installs them
#' into environment. Raises exception if failed to build anything.
#'
#' @keywords internal
#' @noRd
#'
build_install_prj_packages <- function(params, build_type, skip_build_steps = NULL, rebuild = FALSE) {
  intern_repo_path <- params$get_intern_repo_path()
  contrib_url <- rsuite_contrib_url(intern_repo_path, type = build_type, rver = params$r_ver)
  if (!dir.exists(contrib_url)) {
    dir.create(contrib_url, recursive = TRUE)
    rsuite_write_PACKAGES(contrib_url, type = build_type)
  }

  prj_packages <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  if (length(prj_packages) == 0) {
    pkg_loginfo("Nothing to be done.")
    return(invisible())
  }

  # Adding packages to repo
  for (pkg_dir in names(prj_packages)) {
    pkg_name <- prj_packages[[pkg_dir]]

    pkg_loginfo("Installing %s (for R %s) ...", pkg_name, params$r_ver)

    has_changed <- has_package_changed(pkg_dir, params, build_type) # from 55_pkg_source_md5.R
    pkg_path <- file.path(params$pkgs_path, pkg_dir)

    if (any(rebuild) || has_changed) {
      pkg_file <- pkg_build(pkg_path, # from 50_pkg_deployment.R
                            dest_dir = contrib_url,
                            binary = (build_type != "source"),
                            rver = params$r_ver,
                            libpath = params$lib_path,
                            sboxpath = params$sbox_path,
                            skip_build_steps = skip_build_steps)
      if (is.null(pkg_file)) {
        next # Failed to build package
      }
      rsuite_write_PACKAGES(contrib_url, build_type)
      save_package_m5sums(pkg_dir, params, build_type) # from 55_pkg_source_md5.R - for later checks
    } else {
      pkg_file <- file.path(contrib_url, get_package_build_name(pkg_path, build_type))
      pkg_loginfo("... not changed; installing previously built %s.", pkg_file)
    }

    # remove package if installed
    pkg_remove(pkg_name, lib_dir = params$lib_path)

    # this type = "source" does not matter, it is passed just to prevent complaining
    #  on windows that "both" type cannot be used with repos = NULL
    pkg_install(pkg_file, # from 50_pkg_deployment.R
                lib_dir = params$lib_path,
                type = "source",
                repos = NULL,
                rver = params$r_ver)
  }

  failed <- setdiff(prj_packages, utils::installed.packages(params$lib_path)[, "Package"])
  assert(!length(failed),
         "Failed to install project packages: %s", paste(failed, collapse = ", "))

  pkg_loginfo("Successfuly installed %s packages", length(prj_packages))
}
