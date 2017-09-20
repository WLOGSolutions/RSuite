#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for retrieving project package, master scripts and all further
# dependencies.
#----------------------------------------------------------------------------

#'
#' @keywords internal
#'
#' Detects direct uninstalled project dependencies
#'
#' @param params object of rsuite_project_params class
#'
#' @return object of versions class containing direct project dependencies
#'   which are not installed in prject local environment.
#'
collect_uninstalled_direct_deps <- function(params) {
  depVers <- collect_prj_direct_deps(params)
  installed <- as.data.frame(installed.packages(params$lib_path), stringsAsFactors = F)[, c("Package", "Version")]
  depVers <- depVers$rm_acceptable(installed)
}

#'
#' @keywords internal
#'
#' Detects all project (project packages and master scripts) direct dependencies.
#'
#' @param params object of rsuite_project_params class
#'
#' @return object of versions class containing all direct project dependencies
#'   together with their version requirements.
#'
collect_prj_direct_deps <- function(params) {
  pkgVers <- collect_pkgs_direct_deps(params)
  mscVers <- collect_mscs_direct_deps(params)
  prjVers <- versions.union(pkgVers, mscVers)
  return(prjVers)
}

#'
#' @keywords internal
#'
#' Looks for package DESCRIPTION files and retrieves direct dependencies from them.
#'
#' @param params object of rsuite_project_params class
#' @param prj_pkgs detect dependencies only for specified project packages. If 
#'   NULL detect for all. (type: character, default: NULL)
#'
#' @return object of versions class containing all direct dependencies of project
#'   packages together with their version requirements.
#'
collect_pkgs_direct_deps <- function(params) {
  prj_packages <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  pkgs_vers <- do.call("versions.union",
                       lapply(X = prj_packages,
                              FUN = function(name) {
                                collect_single_pkg_direct_deps(params, name)
                              }))

  colliding <- pkgs_vers$get_colliding()
  assert(length(colliding) == 0,
         "Packages with colliding versions detected: %s", paste(colliding, collapse = ", "))

  # Remove base packages from dependencies
  base_pkgs <- installed.packages(lib.loc = .Library, priority = "base")[, "Package"]
  pkgs_vers <- pkgs_vers$rm(base_pkgs)

  # Check R version
  req_r_ver <- pkgs_vers$get('R')
  if (nrow(req_r_ver)) {
    cur_r_ver <- sprintf("%s.%s", R.version$major, R.version$minor)
    assert((is.na(req_r_ver$vmin) || req_r_ver$vmin <= cur_r_ver)
           && (is.na(req_r_ver$vmax) || req_r_ver$vmax >= cur_r_ver),
           "R version(%s) does not meet requirements: it must be in range %s .. %s",
           cur_r_ver, req_r_ver$vmin, req_r_ver$vmax)
  }

  # Get rid of R version as it is checked
  pkgs_vers <- pkgs_vers$rm('R')
  return(pkgs_vers)
}


#'
#' @keywords internal
#'
#' Retrieve dependencies with requirements for a single project package
#'
#' @param params object of rsuite_project_params class
#' @param pkg_name package name (type: character)
#'
#' @return object of versions class containing all direct dependencies of package
#'   together with their version requirements.
#'
collect_single_pkg_direct_deps <- function(params, pkg_name) {
  pkgs <- desc_retrieve_dependencies(params$pkgs_path, pkg_name) # from 51_pkg_info.R
  pdfs <- lapply(X = gsub("^\\s+|\\s+$", "", pkgs),
                 FUN = function(pdesc) {
                   pdesc <- gsub("\\s+", "", pdesc)
                   ver_op <- sub(pattern = ".+\\(([><=]+).+\\)", replacement = "\\1", x = pdesc)
                   if (ver_op == pdesc) { # no version infor
                     return(versions.build(pdesc))
                   }
                   ver <- sub(pattern = ".+\\([><=]+(.+)\\)", replacement = "\\1", x = pdesc)
                   pdesc <- sub(patter = "(.+)\\(.+\\)", replacement = "\\1", x = pdesc)
                   if (ver_op == "==") {
                     return(versions.build(pdesc, vmin = ver, vmax = ver))
                   }
                   if (ver_op == ">=") {
                     return(versions.build(pdesc, vmin = ver, vmax = NA))
                   }
                   if (ver_op == "<=") {
                     return(versions.build(pdesc, vmin = NA, vmax = ver))
                   }

                   assert(FALSE,
                          "Usupported version specification(%s %s) for %s in DESCRIPTION of %s",
                          vep_op, ver, pdesc, pkg_name)
                 })
  do.call("versions.union", pdfs)
}


#'
#' @keywords internal
#'
#' Looks for master scripts and retrieves all direct dependencies from them.
#'
#' @param params object of rsuite_project_params class
#'
#' @return object of versions class containing all direct dependencies of master
#'   scripts. Of cause master scrips cannot enforce version requirements so
#'   versions object does not contain requirements on package versions.
#'
collect_mscs_direct_deps <- function(params) {
  script_files <- list.files(path = params$script_path, pattern = "*.(r|R|Rmd)$",
                             recursive = TRUE, full.names = TRUE)
  pkgs <- unlist(
    lapply(X = script_files,
           FUN = function(sf) {
             lns <- readLines(sf)
             loads <- lns[grepl("^\\s*(require|library)\\((.+)\\)", lns)]
             loads <- gsub("\\s+", "", loads) # remove extra spaces
             gsub("^(require|library)\\(['\"]?([^,'\"]+)['\"]?(,.+)?\\).*$", "\\2", loads)
           }))
  mscs_vers <- versions.build(unique(pkgs))
  
  # Remove base packages from dependencies
  base_pkgs <- installed.packages(lib.loc = .Library, priority = "base")[, "Package"]
  mscs_vers <- mscs_vers$rm(base_pkgs)
  
  return(mscs_vers)
}


#'
#' @keywords internal
#'
#' Retrieves all subsequent dependencies for packages described by version
#' object passed.
#'
#' @param vers version object describing packages to retrieve all subsequent
#'    dependencies for.
#' @param repo_infos list of description of repositories  to search for
#'    dependencies in. Unused if avail_pkgs passed. (object of rsuite_repo_info)
#' @param type type of packages to search. . Unused if avail_pkgs passed.
#' @param all_pkgs matrix same as available.packages return.
#'
#' @return pkgSearchResult object containing packages from vers and all their
#'    subsequent dependencies.
#'
collect_all_subseq_deps <- function(vers, repo_infos, type, all_pkgs = NULL) {
  stopifnot(is.versions(vers))

  if (is.null(all_pkgs)) {
    stopifnot(!missing(repo_infos))
    stopifnot(!missing(type))

    contrib_urls <- retrieve_contrib_urls(repo_infos, type)    # from 53_repositories.R
    avail_vers <- versions.collect(contrib_urls)
    all_pkgs <- avail_vers$get_avails()
  } else {
    avail_pkgs <- as.data.frame(all_pkgs, stringsAsFactors = F)
    avail_vers <- versions.collect(pkgs = avail_pkgs)
  }

  vers_psr <- vers$diff(avail_vers)
  if (!vers_psr$has_found()) {
    return(vers_psr)
  }

  base_pkgs <- installed.packages(lib.loc = .Library, priority = "base")[, "Package"]
  dep_pkgs <- vers_psr$get_found_names()
  while(TRUE) {
    suppressWarnings({ # supress warnings on non recognized packages
      next_deps <- tools::package_dependencies(packages = dep_pkgs,
                                               db = all_pkgs,
                                               which = c("Depends", "Imports", "LinkingTo"),
                                               recursive = TRUE)
    })

    next_deps <- unique(unname(unlist(next_deps)))
    next_deps <- setdiff(next_deps, base_pkgs)
    next_deps <- setdiff(next_deps, dep_pkgs)
    if (!length(next_deps)) {
      break;
    }
    dep_pkgs <- c(dep_pkgs, next_deps)
  }

  dep_pkgs <- setdiff(dep_pkgs, vers_psr$get_found_names())
  if (!length(dep_pkgs)) {
    return(vers_psr)
  }

  dep_vers <- versions.build(dep_pkgs)
  dep_psr <- dep_vers$diff(avail_vers)

  return(vers_psr$union(dep_psr))
}
