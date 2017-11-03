#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities to support pkg_build_deps
#----------------------------------------------------------------------------

#'
#' Builds local project environment.
#'
#' @param params prject parameters(type: rsuite_project_params)
#' @param ... parameters passed to installation process
#'
#' It collects packages the project depends on and installs them in local
#' project environment. ependencies are collected from packages and master
#' scripts.
#'
#' @keywords internal
#'
install_prj_deps <- function(params, ...) {
  pkg_loginfo("Detecting repositories (for R %s)...", params$r_ver)

  repo_infos <- get_all_repo_infos(params) # from 53_repositories.R
  log_repo_infos(repo_infos) # from 53_repositories.R

  avail_vers <- resolve_prj_deps(repo_infos, params)
  install_dependencies(avail_vers, lib_dir = params$lib_path, rver = params$r_ver)
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
#'
resolve_prj_deps <- function(repo_infos, params, only_source = F) {
  if(only_source) {
    pkg_types <- "source"
  } else {
    pkg_types <- c(params$bin_pkgs_type, "source")
  }


  pkg_loginfo("Collecting project dependencies (for R %s)...", params$r_ver)
  prjDepVers <- collect_prj_direct_deps(params)        # from 52_dependencies.R

  project_packages <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  prjDepVers <- vers.rm(prjDepVers, project_packages)

  pkg_loginfo("Resolving dependencies (for R %s)...", params$r_ver)
  avail_vers <- resolve_dependencies(prjDepVers, repo_infos = repo_infos, pkg_types = pkg_types)
  stopifnot(avail_vers$has_avails())
  return(avail_vers)
}

#'
#' Installs dependencies specified by typedVers from reposistories specified by
#'   repo_infos. First tries to install fst_type packages then  base and aux types
#'   as specified by typedVers.
#'
#' @param avail_vers version object describing resolved dependencies to install.
#' @param lib_dir directory to install into. Must not be NULL.
#' @param rver R version to install dependencies for. (type: character)
#'
#' @keywords internal
#'
install_dependencies <- function(avail_vers, lib_dir, rver) {
  stopifnot(is.versions(avail_vers))
  stopifnot(avail_vers$has_avails())
  stopifnot(is_nonempty_char1(lib_dir))

  remove_installed <- function(vers) {
    installed <- as.data.frame(installed.packages(lib.loc = lib_dir), stringsAsFactors = F)[, c("Package", "Version", "Built")]
    installed <- installed[majmin_rver(installed$Built) == majmin_rver(rver), ]
    return(vers.rm_acceptable(vers, installed))
  }

  avail_vers <- remove_installed(avail_vers)
  if (vers.is_empty(avail_vers)) {
    pkg_loginfo("No dependencies to install.")
    return(invisible())
  }

  pkg_loginfo("Detected %s dependencies to install. Installing...", length(vers.get_names(avail_vers)))

  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = T)
  on.exit({ unlink(tmp_dir, recursive = T, force = T) }, add = T)

  avail_deps <- vers.pick_available_pkgs(avail_vers)
  dloaded <- pkg_download(avail_deps, dest_dir = tmp_dir)
  # sort them for installation (in dependency order)
  dloaded <- dloaded[ pkg_inst_order(dloaded$Package, db = avail_deps), ]

  # this type = "source" does not matter, it is passed just to prevent complaining
  #  on windows that "both" type cannot be used with repos = NULL
  pkg_install(dloaded$Path, lib_dir = lib_dir, type = "source", repos = NULL, rver = rver)

  avail_vers <- remove_installed(avail_vers)
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
#'
#' @return versions object describing all resolved dependencies.
#'
#' @keywords internal
#'
resolve_dependencies <- function(vers, repo_infos, pkg_types) {
  stopifnot(is.versions(vers))
  stopifnot(is.character(pkg_types) && length(pkg_types) >= 1)

  curr_cr <- check_res.build(missing = vers)
  all_deps <- check_res.get_missing(curr_cr)
  while(!setequal(vers.get_names(all_deps), curr_cr$get_found_names())) {
    curr_missings <- vers.rm(all_deps, curr_cr$get_found_names())
    for(ri in repo_infos) {
      for(tp in pkg_types) {
        tp_cr <- collect_all_subseq_deps(vers = curr_missings, # from 52_dependencies.R
                                         repo_infos = list(ri),
                                         type = tp)
        if (!any(vers.get_names(curr_missings) %in% tp_cr$get_found_names())) {
          next
        }

        all_deps <- vers.union(all_deps,
                               vers.drop_avails(check_res.get_found(tp_cr)),
                               check_res.get_missing(tp_cr))

        # remove new dependencies: these must be searched again from the beginning of pkg_types
        tp_cr <- check_res.exclude(tp_cr, setdiff(tp_cr$get_found_names(), vers.get_names(curr_missings)))
        tp_cr <- check_res.exclude(tp_cr, setdiff(tp_cr$get_missing_names(), vers.get_names(curr_missings)))
        curr_cr <- check_res.join(tp_cr, curr_cr)

        curr_missings <- vers.rm(curr_missings, curr_cr$get_found_names())
        if (vers.is_empty(curr_missings)) {
          break
        }
      }

      if (vers.is_empty(curr_missings)) {
        break
      }
    }

    assert(vers.is_empty(curr_missings),
           "Required dependencies are not available: %s",
           paste(vers.get_names(curr_missings), collapse = ", "))
  }

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
#'
resolve_packages <- function(vers, repo_infos, pkg_types) {
  stopifnot(is.versions(vers))
  stopifnot(is.character(pkg_types) && length(pkg_types) >= 1)

  curr_miss <- vers
  found <- vers.build(avails = data.frame())

  for(ri in repo_infos) {
    for(tp in pkg_types) {
      contrib_urls <- retrieve_contrib_urls(list(ri), tp) # from 53_repositories.R

      tp_avails <- vers.collect(contrib_urls)
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
#' Detects order of installation of packages as permutation of input vector
#'
#' @param pkgs vector of package names to permutate
#' @param db data frame of available packages as returned by available.packages
#'
#' @return numeric vertor which is defining permutation of packages ta align
#' them in order of installation (less dependent to more dependent).
#'
#' @keywords internal
#'
pkg_inst_order <- function(pkgs, db) {
  pkg2deps <- tools::package_dependencies(pkgs, db = db)
  for(nm in names(pkg2deps)) {
    pkg2deps[[nm]] <- intersect(pkg2deps[[nm]], db$Package)
  }

  idxs <- 1:length(pkgs)

  result <- c()
  processed <- c()
  while(length(setdiff(pkgs, processed)) > 0) {
    nextSet <- pkgs[unlist(lapply(X = pkg2deps[pkgs],
                                  FUN = function(deps) length(setdiff(deps, processed)) == 0))
                    & !(pkgs %in% processed)]
    assert(length(nextSet) > 0,
           "Dependencies for %s package(s) could not be satisfied",
           paste(setdiff(pkgs, processed), collapse = ", "))

    result <- c(result, idxs[pkgs %in% nextSet]) # append to result their indexes
    processed <- c(processed, nextSet)
  }

  stopifnot(length(result) == length(pkgs))
  return(result)
}

#'
#' Cleans unrequired installed packages from project local environment.
#'
#' @param params prject parameters(type: rsuite_project_params)
#'
#' @keywords internal
#'
clean_prj_deps <- function(params) {
  all_installed <- data.frame(utils::installed.packages(lib.loc = params$lib_path), stringsAsFactors = F)

  is_rver_valid <- majmin_rver(all_installed$Built) == majmin_rver(params$r_ver)
  installed <- all_installed[is_rver_valid, ]

  deps <- collect_prj_direct_deps(params) # from 52_dependencies.R

  # to satisfy collect_all_subseq_deps requirements
  installed$Repository <- rep(params$lib_path, nrow(installed))
  installed$File <- rep(NA, nrow(installed))

  cr <- collect_all_subseq_deps(deps, all_pkgs = installed) # from 52_dependencies.R

  proj_pkgs <- build_project_pkgslist(params$pkgs_path)

  required <- c(cr$get_found_names(), proj_pkgs)
  to_clean <- c(setdiff(installed$Package, required), # non required
                setdiff(all_installed[!is_rver_valid, "Package"], proj_pkgs))  # invalid

  if (!length(to_clean)) {
    pkg_loginfo("All installed packages are required by the project.")
  } else {
    pkg_loginfo("Found %s package(s) to remove: %s",
                length(to_clean), paste(to_clean, collapse = ", "))

    pkg_remove(to_clean, lib_dir = params$lib_path) # from 50_pkg_deployment.R

    pkg_loginfo("All removed")
  }
}
