#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities to support pkg_build_deps
#----------------------------------------------------------------------------

#'
#' @keywords internal
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
install_prj_deps <- function(params, ...) {
  pkg_loginfo("Detecting repositories (for R %s)...", params$r_ver)

  repo_infos <- get_all_repo_infos(params) # from 53_repositories.R
  log_repo_infos(repo_infos) # from 53_repositories.R

  avail_vers <- resolve_prj_deps(repo_infos, params)
  install_dependencies(avail_vers, lib_dir = params$lib_path, rver = params$r_ver)
}

#'
#' @keywords internal
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
resolve_prj_deps <- function(repo_infos, params, only_source = F) {
  if(only_source) {
    pkg_types <- "source"
  } else {
    pkg_types <- c(params$bin_pkgs_type, "source")
  }


  pkg_loginfo("Collecting project dependencies (for R %s)...", params$r_ver)
  prjDepVers <- collect_prj_direct_deps(params)        # from 52_dependencies.R

  project_packages <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  prjDepVers <- prjDepVers$rm(project_packages)

  pkg_loginfo("Resolving dependencies (for R %s)...", params$r_ver)
  avail_vers <- resolve_missings(prjDepVers, repo_infos = repo_infos, pkg_types = pkg_types)
  stopifnot(avail_vers$has_avails())
  return(avail_vers)
}

#'
#' @keywords internal
#'
#' Installs dependencies specified by typedVers from reposistories specified by
#'   repo_infos. First tries to install fst_type packages then  base and aux types
#'   as specified by typedVers.
#'
#' @param avail_vers version object describing resolved dependencies to install.
#' @param lib_dir directory to install into. Must not be NULL.
#' @param rver R version to install dependencies for. (type: character)
#'
install_dependencies <- function(avail_vers, lib_dir, rver) {
  stopifnot(is.versions(avail_vers))
  stopifnot(avail_vers$has_avails())
  stopifnot(is_nonempty_char1(lib_dir))

  remove_installed <- function(vers) {
    installed <- as.data.frame(installed.packages(lib.loc = lib_dir), stringsAsFactors = F)[, c("Package", "Version", "Built")]
    installed <- installed[majmin_rver(installed$Built) == majmin_rver(rver), ]
    return(vers$rm_acceptable(installed))
  }
  
  avail_vers <- remove_installed(avail_vers)
  if (avail_vers$is_empty()) {
    pkg_loginfo("No dependencies to install.")
    return(invisible())
  }
  
  pkg_loginfo("Detected %s dependencies to install. Installing...", length(avail_vers$get_names()))

  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = T)
  on.exit({ unlink(tmp_dir, recursive = T, force = T) }, add = T)
  
  avail_deps <- avail_vers$pick_available_pkgs()
  dloaded <- pkg_download(avail_deps, dest_dir = tmp_dir)
  # sort them for installation (in dependency order)
  dloaded <- dloaded[ pkg_inst_order(dloaded$Package, db = avail_deps), ]
  
  # this type = "source" does not matter, it is passed just to prevent complaining
  #  on windows that "both" type cannot be used with repos = NULL
  pkg_install(dloaded$Path, lib_dir = lib_dir, type = "source", repos = NULL, rver = rver)

  avail_vers <- remove_installed(avail_vers)
  assert(avail_vers$is_empty(),
         "Failed to install some dependencies: %s", paste(avail_vers$get_names(), collapse = ", "))
  pkg_loginfo("All dependencies successfully installed.")
}

#'
#' @keywords internal
#'
#' Resolves missing dependencies using provided repos.
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
resolve_missings <- function(vers, repo_infos, pkg_types) {
  stopifnot(is.versions(vers))
  stopifnot(is.character(pkg_types) && length(pkg_types) >= 1)

  curr_psr <- build.pkgSearchResults(missing = vers)
  all_deps <- curr_psr$get_missing()
  while(!setequal(all_deps$get_names(), curr_psr$get_found_names())) {
    curr_missings <- all_deps$rm(curr_psr$get_found_names())
    for(ri in repo_infos) {
      for(tp in pkg_types) {
        tp_psr <- collect_all_subseq_deps(vers = curr_missings, # from 52_dependencies.R
                                          repo_infos = list(ri),
                                          type = tp)
        if (!any(curr_missings$get_names() %in% tp_psr$get_found_names())) {
          next
        }
        
        all_deps <- versions.union(all_deps, tp_psr$get_found()$rm_avails(), tp_psr$get_missing())
        
        # remove new dependencies: these must be searched again from the beginning of pkg_types
        tp_psr <- tp_psr$exclude(setdiff(tp_psr$get_found_names(), curr_missings$get_names()))
        tp_psr <- tp_psr$exclude(setdiff(tp_psr$get_missing_names(), curr_missings$get_names()))
        curr_psr <- tp_psr$join(curr_psr)
        
        curr_missings <- curr_missings$rm(curr_psr$get_found_names())
        if (curr_missings$is_empty()) {
          break
        }
      }
      
      if (curr_missings$is_empty()) {
        break
      }
    }
    
    assert(curr_missings$is_empty(), 
           "Required dependencies are not available: %s", paste(curr_missings$get_names(), collapse = ", "))
  }
  
  return(curr_psr$get_found())
}


#'
#' @keywords internal
#'
#' Detects order of installation of packages as permutation of input vector
#'
#' @param pkgs vector of package names to permutate
#' @param db data frame of available packages as returned by available.packages
#'
#' @return numeric vertor which is defining permutation of packages ta align
#' them in order of installation (less dependent to more dependent).
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
#' @keywords internal
#'
#' Cleans unrequired installed packages from project local environment.
#'
#' @param params prject parameters(type: rsuite_project_params)
#'
clean_prj_deps <- function(params) {
  all_installed <- data.frame(utils::installed.packages(lib.loc = params$lib_path), stringsAsFactors = F)

  is_rver_valid <- majmin_rver(all_installed$Built) == majmin_rver(params$r_ver)
  installed <- all_installed[is_rver_valid, ]

  deps <- collect_prj_direct_deps(params) # from 52_dependencies.R

  # to satisfy collect_all_subseq_deps requirements
  installed$Repository <- rep(params$lib_path, nrow(installed))
  installed$File <- rep(NA, nrow(installed))
  
  psr <- collect_all_subseq_deps(deps, all_pkgs = installed) # from 52_dependencies.R

  proj_pkgs <- build_project_pkgslist(params$pkgs_path)

  required <- c(psr$found$get_names(), proj_pkgs) # from 51_pkg_info.R
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
