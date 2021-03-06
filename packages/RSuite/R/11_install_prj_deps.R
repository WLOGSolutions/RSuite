#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities to support pkg_build_deps
#----------------------------------------------------------------------------

#'
#' Builds local project environment.
#'
#' It collects packages the project depends on and installs them in local
#' project environment. Dependencies are collected from packages and master
#' scripts.
#'
#' @param params project parameters. (type: rsuite_project_params)
#' @param sups specifies which supportive packages should be installed. One of
#' \describe{
#'  \item{none}{Do not install supportive packages}
#'  \item{vanilla}{Install only base supportive packages(like devtools & roxygen2)}
#'  \item{all}{Install all packages in suggests}
#'  }
#'  (type: character(1), default: all)
#' @param relock if TRUE allows to update the env.lock file
#'   (type: logical, default: FALSE)
#' @param check_repos_consistency if TRUE will prevent installing
#'   packages built for another R ver. (type: logical, default: TRUE if R is stable)
#'
#' @keywords internal
#' @noRd
#'
install_prj_deps <- function(params,
                             sups = "all",
                             relock = FALSE,
                             check_repos_consistency = is_r_stable()) {
  pkg_loginfo("Detecting repositories (for R %s)...", params$r_ver)

  repo_infos <- get_all_repo_infos(params) # from 53_repositories.R
  log_repo_infos(repo_infos) # from 53_repositories.R

  avail_vers <- resolve_prj_deps(repo_infos, params)

  avail_vers <- lock_prj_deps(avail_vers, params, relock) # from 52_dependencies.R

  install_dependencies(avail_vers,
                       lib_dir = params$lib_path,
                       rver = params$r_ver,
                       check_repos_consistency = check_repos_consistency)

  if (sups == "none") {
    return(invisible())
  }

  avail_sup_vers <- resolve_prj_sups(repo_infos, params, vanilla = (sups == "vanilla"))
  if (!is.null(avail_sup_vers)) {
    install_support_pkgs(avail_sup_vers,
                         sbox_dir = params$sbox_path,
                         lib_dir = params$lib_path,
                         rver = params$r_ver,
                         check_repos_consistency = check_repos_consistency)
  }
}

#'
#' Runs overall project support package resolving.
#' Resolves all the support package dependencies.
#'
#' @param repo_infos list of description of repositories (object of rsuite_repo_info)
#'    to use for dependencies detection.
#' @param params object of rsuite_project_params
#' @param only_source detect only source type dependencies
#' @param vanilla if TRUE resolves only base supportive packages. (type: logical(1))
#'
#' @return version object describing available project dependencies.
#'
#' @keywords internal
#' @noRd
#'
resolve_prj_sups <- function(repo_infos, params, only_source = FALSE, vanilla = FALSE) {
  pkg_types <- "source"
  if (!only_source) {
    pkg_types <- c(params$bin_pkgs_type, pkg_types)
  }

  pkg_logdebug("Collecting project support(%s) packages (for R %s)...",
               ifelse(any(vanilla), "Base", "All"), params$r_ver)
  prj_sup_vers <- collect_prj_support_pkgs(params, vanilla)   # from 52_dependencies.R

  # remove project packages
  project_packages <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  prj_sup_vers <- vers.rm(prj_sup_vers, project_packages)

  # remove already installed packages
  installed <- get_loadable_packages(vers.get_names(prj_sup_vers),
                                     ex_liblocs = c(params$sbox_path, params$lib_path),
                                     rver = params$r_ver)
  prj_sup_vers <- vers.rm_acceptable(prj_sup_vers, installed)

  if (!vers.is_empty(prj_sup_vers)) {
    pkg_logdebug("Resolving support packages (with deps) (for R %s)...", params$r_ver)

    # prepare additional requirements based on installed packages
    installed_pkgs <- as.data.frame(utils::installed.packages(lib.loc = params$lib_path),
                                    stringAsFactors = FALSE)
    installed_vers <- do.call("vers.union",
                              by(installed_pkgs, seq_len(nrow(installed_pkgs)),
                                 FUN = function(pkg) {
                                   vers.build(pkg$Package, vmin = pkg$Version, vmax = pkg$Version)
                                 }))

    avail_vers <- resolve_dependencies(prj_sup_vers, repo_infos = repo_infos,
                                       pkg_types = pkg_types, extra_reqs = installed_vers)
    stopifnot(avail_vers$has_avails())
    return(avail_vers)
  }

  pkg_logdebug("No project support packages required.")
  return()

}

#'
#' Runs overall project dependency resolving.
#' Resolves all the project dependencies (from packages and master scripts).
#'
#' @param repo_infos list of description of repositories (object of rsuite_repo_info)
#'    to use for dependencies detection.
#' @param params object of rsuite_project_params
#' @param only_source detect only source type dependencies
#'
#' @return version object describing available project dependencies.
#'
#' @keywords internal
#' @noRd
#'
resolve_prj_deps <- function(repo_infos, params, only_source = FALSE) {
  pkg_types <- "source"
  if (!only_source) {
    pkg_types <- c(params$bin_pkgs_type, pkg_types)
  }


  pkg_loginfo("Collecting project dependencies (for R %s)...", params$r_ver)
  prj_dep_vers <- collect_prj_direct_deps(params)        # from 52_dependencies.R

  project_packages <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  prj_dep_vers <- vers.rm(prj_dep_vers, project_packages)

  # if locked?
  if (file.exists(params$lock_path)) {
    # apply lock requirements
    env_lock_vers <- get_lock_env_vers(params) # from 52_dependencies.R
    env_lock_vers <- vers.rm(env_lock_vers, project_packages)
    env_lock_vers <- vers.rm(env_lock_vers, # remove non required locks
                             setdiff(vers.get_names(env_lock_vers), vers.get_names(prj_dep_vers)))

    # if lock requirements introduce some unfesibles assume some
    #  requirements added altered in packages itself. So do not try
    #  enforce lock requirements for them. Leave the problem to be resolved
    #  later: while handling relocking etc.
    prj_dep_candidate <- vers.union(prj_dep_vers, env_lock_vers)
    candidate_unfeasibles <- vers.get_unfeasibles(prj_dep_candidate)
    if (length(candidate_unfeasibles) == 0) {
      prj_dep_vers <- prj_dep_candidate
    } else {
      prj_dep_vers <- vers.union(prj_dep_vers, vers.rm(env_lock_vers, candidate_unfeasibles))
    }
  }

  pkg_loginfo("Resolving dependencies (for R %s)...", params$r_ver)
  avail_vers <- resolve_dependencies(prj_dep_vers, repo_infos = repo_infos, pkg_types = pkg_types)

  stopifnot(avail_vers$has_avails())
  return(avail_vers)
}

#'
#' Retrieves packages among passed installed and loadable for specific rver.
#'
#' @param pkgs packages to check if installed and loadable. (type: character(N))
#' @param ex_liblocs path(s) to extra folders to look for installed packages. (type: character(N))
#' @param rver R version to retrieve installed packages for. (type: character(1))
#'
#' @return data.frame with columns Package, Version, Build
#'
#' @keywords internal
#' @noRd
#'
get_loadable_packages <- function(pkgs, ex_liblocs, rver) {
  in_file <- tempfile(fileext = ".RData")
  save(pkgs, file = in_file)

  ou_file <- tempfile(fileext = ".RData")
  on.exit({
    unlink(ou_file, force = TRUE)
  },
  add = TRUE)

  get_result <- run_rscript(
    c("installed <- utils::installed.packages()",
      "installed <- as.data.frame(installed, stringsAsFactors = F)[, c('Package', 'Version', 'Built')]",
      "load(%s)",
      "installed <- installed[installed$Package %%in%% pkgs, ]",
      "installed <- installed[!duplicated(installed$Package), ]",
      paste0("loadable <- unlist(lapply(",
             "   X = installed$Package,",
             "   FUN = function(pkg) {",
             "     tryCatch({",
             "      suppressPackageStartupMessages(library(pkg, character.only = TRUE));",
             "      pkg",
             "     },",
             "     error = function(e) NULL)",
             "   }))"),
      "installed <- installed[installed$Package %%in%% loadable, ]",
      "save(installed, %s)"),
    rscript_arg("file", in_file),
    rscript_arg("file", ou_file),
    ex_libpath = ex_liblocs,
    rver = rver)
  if (!is.null(get_result)) {
    if (get_result == FALSE) {
      pkg_logwarn("Get loadable aborted")
    } else {
      pkg_logwarn("Get loadable failed: %s", get_result)
    }

    return(NULL)
  }

  installed <- NULL # to prevent warning
  load(ou_file)
  return(installed)
}


#'
#' Installs supportive packages specified by avail_vers.
#'
#' @param avail_vers version object describing resolved dependencies to install.
#' @param sbox_dir directory to install into. Must not be NULL.
#' @param lib_dir directory there dependencies have been installed. Must not be NULL.
#' @param rver R version to install dependencies for. (type: character)
#' @param check_repos_consistency if TRUE will prevent installing
#'   packages built for another R ver. (type: logical, default: TRUE)
#'
#' @keywords internal
#' @noRd
#'
install_support_pkgs <- function(avail_vers, sbox_dir, lib_dir, rver,
                                 check_repos_consistency = TRUE) {
  stopifnot(is.versions(avail_vers))
  stopifnot(avail_vers$has_avails())
  stopifnot(is_nonempty_char1(sbox_dir))
  stopifnot(is_nonempty_char1(lib_dir))

  remove_installed <- function(vers, check_built_rver) {
    installed <- get_loadable_packages(vers.get_names(vers),
                                       ex_liblocs = c(sbox_dir, lib_dir),
                                       rver = rver)
    if (any(check_built_rver)) {
      installed <- installed[majmin_rver(installed$Built) == majmin_rver(rver), ]
    }
    return(vers.rm_acceptable(vers, installed))
  }

  avail_vers <- remove_installed(avail_vers, is_r_stable())
  if (vers.is_empty(avail_vers)) {
    pkg_logdebug("No support packages to install.")
    return(invisible())
  }

  pkg_loginfo("Detected %s support packages to install. Installing...", length(vers.get_names(avail_vers)))

  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = TRUE)
  on.exit({
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
  },
  add = TRUE)

  avail_deps <- vers.pick_available_pkgs(avail_vers)
  dloaded <- pkg_download(avail_deps, dest_dir = tmp_dir)
  # sort them for installation (in dependency order)
  dloaded <- dloaded[ pkg_inst_order(dloaded$Package, db = avail_deps), ]

  # this type = "source" does not matter, it is passed just to prevent complaining
  #  on windows that "both" type cannot be used with repos = NULL
  pkg_install(dloaded$Path,
              lib_dir = sbox_dir,
              type = "source",
              repos = NULL,
              rver = rver,
              check_repos_consistency = check_repos_consistency,
              ex_libpath = lib_dir)

  avail_vers <- remove_installed(avail_vers, check_repos_consistency)
  assert(vers.is_empty(avail_vers),
         "Failed to install some support packages: %s", paste(vers.get_names(avail_vers), collapse = ", "))
  pkg_loginfo("All support packages successfully installed.")
}

#'
#' Installs dependencies specified by avail_vers.
#'
#' @param avail_vers version object describing resolved dependencies to install.
#' @param lib_dir directory to install into. Must not be NULL.
#' @param rver R version to install dependencies for. (type: character)
#' @param check_repos_consistency If TRUE binary consistency with rver will be
#'   checked after installation of each package.
#'   (type: logical(1), default: TRUE)
#'
#' @keywords internal
#' @noRd
#'
install_dependencies <- function(avail_vers, lib_dir, rver,
                                 check_repos_consistency = TRUE) {
  stopifnot(is.versions(avail_vers))
  stopifnot(avail_vers$has_avails())
  stopifnot(is_nonempty_char1(lib_dir))

  remove_installed <- function(vers, check_built_rver, notify_on_update) {
    installed <- as.data.frame(utils::installed.packages(lib.loc = lib_dir),
                               stringsAsFactors = FALSE)[, c("Package", "Version", "Built")]
    if (any(check_built_rver)) {
      installed <- installed[majmin_rver(installed$Built) == majmin_rver(rver), ]
    }

    avails <- vers$get_avails()
    if (nrow(avails) != 0) {
      present <- merge(installed, avails, by = c("Package", "Version"))
      missing <- installed[!(installed$Package %in% present$Package), ]

      if (length(missing$Package) != 0 && notify_on_update) {
        pkg_loginfo("Following installed packages will be updated: %s", missing$Package)
      }

      # remove deprecated packages from installed so they get updated
      installed <- installed[!installed$Package %in% missing$Package, ]
    }

    return(vers.rm_acceptable(vers, installed))
  }

  avail_vers <- remove_installed(avail_vers, is_r_stable(), notify_on_update = TRUE)
  if (vers.is_empty(avail_vers)) {
    pkg_loginfo("No dependencies to install.")
    return(invisible())
  }

  pkg_loginfo("Detected %s dependencies to install. Installing...", length(vers.get_names(avail_vers)))

  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = TRUE)
  on.exit({
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
  },
  add = TRUE)

  avail_deps <- vers.pick_available_pkgs(avail_vers)
  dloaded <- pkg_download(avail_deps, dest_dir = tmp_dir)
  # sort them for installation (in dependency order)
  dloaded <- dloaded[ pkg_inst_order(dloaded$Package, db = avail_deps), ]

  # this type = "source" does not matter, it is passed just to prevent complaining
  #  on windows that "both" type cannot be used with repos = NULL
  pkg_install(dloaded$Path,
              lib_dir = lib_dir,
              type = "source",
              repos = NULL,
              rver = rver,
              check_repos_consistency = check_repos_consistency)

  avail_vers <- remove_installed(avail_vers, check_repos_consistency, notify_on_update = FALSE)
  assert(vers.is_empty(avail_vers),
         "Failed to install some dependencies: %s", paste(vers.get_names(avail_vers), collapse = ", "))
  pkg_loginfo("All dependencies successfully installed.")
}

#'
#' Resolves dependencies using provided repos.
#' Raises assertion if failes to find some of dependencies.
#'
#' @param vers versions object describing packages to resolve missings for.
#' @param repo_infos list of description of repositories (object of rsuite_repo_info)
#'    to resolve dependencies with
#' @param pkg_types types of packages which are tried for dependencies in order
#'    to check. (type: character)
#' @param extra_reqs additional version requirements, those will be used while
#' checking vers requirements subdependencies. (type: versions, default: NULL)
#'
#' @return versions object describing all resolved dependencies.
#'
#' @keywords internal
#' @noRd
#'
resolve_dependencies <- function(vers, repo_infos, pkg_types, extra_reqs = NULL) {
  stopifnot(is.versions(vers))
  stopifnot(is.null(extra_reqs) || is.versions(extra_reqs))
  stopifnot(is.character(pkg_types) && length(pkg_types) >= 1)

  curr_cr <- check_res.build(missing = vers.rm_base(vers))
  while (check_res.has_missing(curr_cr)) {
    has_found_new_deps <- FALSE
    for (rp in repo_infos) {
      for (tp in pkg_types) {
        if (!check_res.has_missing(curr_cr)) {
          # all satisfied already
          next
        }

        tp_cr <- collect_all_subseq_deps(cr = curr_cr, # from 52_dependencies.R
                                         repo_info = rp,
                                         type = tp,
                                         extra_reqs = extra_reqs)

        if (!any(curr_cr$get_missing_names() %in% tp_cr$get_found_names())) {
          # nothing found: check next pkg_type
          next
        }

        infeasibles <- vers.get_unfeasibles(check_res.get_missing(tp_cr))
        if (length(infeasibles) > 0) {
          pkg_logwarn("Infeasible dependency requerements detected:")

          base_reqs <- vers.get(vers, infeasibles)
          by(base_reqs, seq_len(nrow(base_reqs)), function(breq) {
            pkg_logwarn(". %s base %s", breq$pkg, breq$vmin, breq$vmax)
          })

          othr_pkgs <- setdiff(infeasibles, base_reqs$pkg)
          if (length(othr_pkgs) > 0) {
            pkg_logwarn(". %s", othr_pkgs)
          }

          dependant_pkgs <- setdiff(vers.get_names(check_res.get_missing(tp_cr)), infeasibles)
          if (length(dependant_pkgs) > 0) {
            pkg_logwarn(". probably required by %s", dependant_pkgs)
          }

          stop(sprintf("Infeasible dependency requerements detected for %s",
                       paste(infeasibles, collapse = ", ")))
        }

        new_deps_found <- setdiff(tp_cr$get_found_names(), curr_cr$get_missing_names())
        new_deps_missing <- setdiff(tp_cr$get_missing_names(), curr_cr$get_missing_names())
        if (length(c(new_deps_missing, new_deps_found)) != 0) {
          has_found_new_deps <- TRUE
        }

        curr_cr <- check_res.join(tp_cr, curr_cr)
      }
    }

    if (!has_found_new_deps) {
      break
    }
  }

  assert(!check_res.has_missing(curr_cr),
         "Required dependencies are not available: %s",
         paste(curr_cr$get_missing_names(), collapse = ", "))

  return(check_res.get_found(curr_cr))
}


#'
#' Resolves packages using provided repos.
#' Raises assertion if failes to find some of packages.
#'
#' @param vers versions object describing packages to resolve.
#' @param repo_infos list of description of repositories (object of rsuite_repo_info)
#'    to resolve packages with
#' @param pkg_types types of packages which are tried in order to check. (type: character)
#'
#' @return versions object describing all resolved packages.
#'
#' @keywords internal
#' @noRd
#'
resolve_packages <- function(vers, repo_infos, pkg_types) {
  stopifnot(is.versions(vers))
  stopifnot(is.character(pkg_types) && length(pkg_types) >= 1)

  curr_miss <- vers
  found <- vers.build(avails = data.frame())

  for (ri in repo_infos) {
    for (tp in pkg_types) {
      contrib_url <- ri$get_contrib_url(tp) # from 53_repositories.R

      tp_avails <- vers.collect(contrib_url)
      tp_avails <- vers.rm(tp_avails,
                           pkg_names = setdiff(vers.get_names(tp_avails),
                                               vers.get_names(curr_miss)))

      if (!any(vers.get_names(curr_miss) %in% vers.get_names(tp_avails))) {
        next
      }

      found <- vers.union(found, tp_avails)
      curr_miss <- vers.rm(curr_miss, vers.get_names(tp_avails))
      if (vers.is_empty(curr_miss)) {
        break
      }
    }

    if (vers.is_empty(curr_miss)) {
      break
    }
  }

  assert(vers.is_empty(curr_miss),
         "Packages are not available: %s", paste(vers.get_names(curr_miss), collapse = ", "))
  return(found)
}

#'
#' Detects order of installation of packages as a permutation of the input vector
#'
#' @param pkgs vector of package names to permute
#' @param db data frame of available packages as returned by available.packages
#'
#' @return numeric vector which is defining permutation of packages ta align
#' them in order of installation (less dependent to more dependent).
#'
#' @keywords internal
#'
pkg_inst_order <- function(pkgs, db) {
  pkg2deps <- tools::package_dependencies(pkgs, db = db)
  for (nm in names(pkg2deps)) {
    pkg2deps[[nm]] <- intersect(pkg2deps[[nm]], db$Package)
  }

  idxs <- seq_along(pkgs)

  result <- c()
  processed <- c()
  while (length(setdiff(pkgs, processed)) > 0) {
    next_set <- pkgs[unlist(lapply(X = pkg2deps[pkgs],
                                   FUN = function(deps) length(setdiff(deps, processed)) == 0))
                     & !(pkgs %in% processed)]
    assert(length(next_set) > 0,
           "Dependencies for %s package(s) could not be satisfied",
           paste(setdiff(pkgs, processed), collapse = ", "))

    result <- c(result, idxs[pkgs %in% next_set]) # append to result their indexes
    processed <- c(processed, next_set)
  }

  stopifnot(length(result) == length(pkgs))
  return(result)
}

#'
#' Cleans installed packages that are not required from the local project environment.
#'
#' @param params project parameters(type: rsuite_project_params)
#'
#' @keywords internal
#'
clean_prj_deps <- function(params) {
  all_installed <- collect_installed_pkgs(params) # from 52_dependencies.R
  installed <- all_installed$valid

  required <- collect_prj_required_dep_names(params, installed) # from 53_dependencies.R
  proj_pkgs <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R

  to_clean <- c(setdiff(installed$Package, required), # non required
                setdiff(all_installed$invalid[, "Package"], proj_pkgs))  # invalid

  if (!length(to_clean)) {
    pkg_loginfo("All installed packages are required by the project.")
  } else {
    pkg_loginfo("Found %s package(s) to remove: %s",
                length(to_clean), paste(to_clean, collapse = ", "))

    pkg_remove(to_clean, lib_dir = params$lib_path) # from 50_pkg_deployment.R

    pkg_loginfo("All removed")
  }
}
