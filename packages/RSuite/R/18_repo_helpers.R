#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for retrieving dependencies and preparing temporary repository for
#   building PKGZIP and uploading to package repository.
#----------------------------------------------------------------------------

#'
#' Collects packages with all dependencies of specified type from repositories
#'   specified in params.
#'
#' Will raise exception if failes to find some packages requested or their
#'   dependencies.
#'
#' It will filter dependencies against repository is passed. It means that
#'   dependencies found in the repository will not be included in output if they
#'   satisfy version requirements.
#'
#' @param vers packages with requirements to collect and detect dependencies of.
#'   (type: versions)
#' @param pkg_type type of packages to collect. (type: character(1))
#' @param params project parameters to use for repositories detection to collect
#'   packages from. (type: rsuite_project_params)
#' @param filter_repo repository address to not include dependencies available in.
#'     If NULL will not filter dependencies.  (type: character(1), default: NULL)
#' @param rver R version to collect packages for. If NULL will use R version
#'     specified in params. (type: character(1), default: NULL)
#'
#' @return version object with avails containing all the packages and their
#'   dependencies (except filtered).
#'
#' @keywords internal
#' @noRd
#'
collect_dependencies <- function(vers, pkg_type, params, filter_repo, rver = NULL) {
  stopifnot(is.versions(vers))
  stopifnot(is_nonempty_char1(pkg_type))
  stopifnot(class(params) == "rsuite_project_params")
  if (!is.null(filter_repo)) {
    stopifnot(is_nonempty_char1(filter_repo))
  }
  if (is.null(rver)) {
    rver <- params$r_ver
  }
  stopifnot(is_nonempty_char1(rver))

  resp_types <- get_respected_types(pkg_type, params$bin_pkgs_type)

  pkg_loginfo("Detecting repositories (for R %s)...", rver)
  repo_infos <- get_all_repo_infos(params, rver = rver) # from 53_repositories.R
  if (!is.null(filter_repo)) {
    filter_ris <- build_repo_infos(spec = list(Url = filter_repo), # from 53_repositories.R
                                   types = resp_types, rver = rver)
    repo_infos <- c(filter_ris, repo_infos)
  }
  log_repo_infos(repo_infos) # from 53_repositories.R

  pkg_loginfo("Resolving dependencies (for R %s)...", rver)
  avail_vers <- resolve_dependencies(vers = vers, # from 11_install_prj_deps.R
                                     repo_infos = repo_infos,
                                     pkg_types = resp_types)
  if (!is.null(filter_repo)) {
    filter_contrib_url <- repo_infos[[1]]$get_contrib_url(pkg_type) # from 53_repositories.R
    filter_pkgs <- vers.collect(filter_contrib_url) # from versions.R
    avail_vers <- vers.rm_acceptable(avail_vers, filter_pkgs$avails)
  }
  return(avail_vers)
}

#'
#' Collects packages of specified type from repositories specified in params.
#'
#' Will raise exception if failes to find some packages requested.
#'
#' @param vers packages with requirements to collect. (type: versions)
#' @param pkg_type type of packages to collect. (type: character(1))
#' @param params project parameters to use for repositories detection to collect
#'   packages from. (type: rsuite_project_params)
#' @param rver R version to collect packages for. If NULL will use R version
#'     specified in params. (type: character(1), default: NULL)
#'
#' @param return versions object with avail describing packages found.
#'
#' @keywords internal
#' @noRd
#'
collect_packages <- function(vers, pkg_type, params, rver = NULL) {
  stopifnot(is.versions(vers))
  stopifnot(is_nonempty_char1(pkg_type))
  stopifnot(class(params) == "rsuite_project_params")
  if (is.null(rver)) {
    rver <- params$r_ver
  }
  stopifnot(is_nonempty_char1(rver))

  resp_types <- get_respected_types(pkg_type, params$bin_pkgs_type)

  pkg_loginfo("Detecting repositories (for R %s)...", rver)
  repo_infos <- get_all_repo_infos(params, rver = rver) # from 53_repositories.R
  log_repo_infos(repo_infos) # from 53_repositories.R

  avail_vers <- resolve_packages(vers = vers, # from 11_install_prj_deps.R
                                 repo_infos = repo_infos, pkg_types = resp_types)
  return(avail_vers)
}

#'
#' Prepares repository and downloads (builds if required) packages into it.
#'
#' @param ver versions object specifying which packages to include into repository.
#' @param tmp_path path to temporary repository to prepare. It must not exist.
#'   Repository will be initialized for requested pkg_type for R version got from
#'   params. (type: character(1))
#' @param pkg_type type of packages to include into repository. If it's not source
#'   source packages from vers will be built. (type: character(1))
#' @param params project parameters object. (type: rsuite_project_params)
#' @param rver R version to prepare repository for. If NULL will use R version
#'   specified in params. (type: character(1), default: NULL)
#'
#' @keywords internal
#' @noRd
#'
temp_repo_prepare <- function(vers, tmp_path, pkg_type, params, rver = NULL) {
  stopifnot(is.versions(vers))
  stopifnot(is_nonempty_char1(tmp_path))
  stopifnot(is_nonempty_char1(pkg_type))
  stopifnot(class(params) == "rsuite_project_params")
  if (is.null(rver)) {
    rver <- params$r_ver
  }
  stopifnot(is_nonempty_char1(rver))

  pkg_loginfo("Preparing temp repository for packages ...")

  tmp_mgr <- repo_manager_dir_create(tmp_path, pkg_type, rver)
  repo_manager_init(tmp_mgr)

  if (vers.is_empty(vers)) {
    return()
  }

  avail_pkgs <- deduce_package_files(vers.pick_available_pkgs(vers)) # from 51_pkg_info.R
  pkgs_infos <- get_package_url_infos(sprintf("%s/%s", avail_pkgs$Repository, avail_pkgs$File)) # from 51_pkg_info.R

  exp_pkg_type <- pkg_type
  if (grepl("^mac[.]binary[.]", pkg_type)) {
    exp_pkg_type <- "mac.binary"
  }
  pkg_download(avail_pkgs[pkgs_infos$Type == exp_pkg_type, ],
               dest_dir = rsuite_contrib_url(tmp_path, pkg_type, rver))
  if (pkg_type != "source" && any(pkgs_infos$Type == "source")) {
    src_avail_pkgs <- avail_pkgs[pkgs_infos$Type == "source", ]
    build_source_packages(src_avail_pkgs, # from 17_build_src_packages.R
                          tmp_path, pkg_type, params, rver)
  }
}

#'
#' Copies project packages built into temporary repository.
#'
#' Will raise exception if failes to copy some of project files detected.
#'
#' @param pkgs names of packages to copy (type: character(N))
#' @param tmp_path path to temporary repository to copy to. It must exist and be
#'  inited. (type: character(1))
#' @param pkg_type type of packages to copy into repository. (type: character(1))
#' @param params project parameters object. (type: rsuite_project_params)
#'
#' @keywords internal
#' @noRd
#'
temp_repo_copy_proj_pkgs <- function(pkgs, tmp_path, pkg_type, params) {
  stopifnot(is.character(pkgs))
  stopifnot(is_nonempty_char1(tmp_path))
  stopifnot(dir.exists(tmp_path))
  stopifnot(is_nonempty_char1(pkg_type))
  stopifnot(class(params) == "rsuite_project_params")

  # copy into temp repo package files built
  pkgs_fpath <- list.files(rsuite_contrib_url(params$irepo_path, pkg_type, params$r_ver),
                           pattern = sprintf("^(%s)_.*", paste(pkgs, collapse = "|")),
                           full.names = TRUE)
  dest_curl <- rsuite_contrib_url(tmp_path, pkg_type, params$r_ver)
  success <- file.copy(from = pkgs_fpath, to = dest_curl)
  assert(all(success),
         "Some project packages failed to copy to temp repository: %s",
         paste(pkgs_fpath[success], collapse = ", "))
}


#'
#' Writes PACKAGES into temporary repository.
#'
#' @param tmp_path path to temporary repository write PACKAGES into. It must exist
#'  and be inited. (type: character(1))
#' @param pkg_type type of packages to copy into repository. (type: character(1))
#' @param rver R version to prepare repository for. If NULL will use default
#'   R version. (type: character(1), default: NULL)
#'
#' @keywords internal
#' @noRd
#'
temp_repo_write_PACKAGES <- function(tmp_path, pkg_type, rver = NULL) {
  dest_curl <- rsuite_contrib_url(tmp_path, pkg_type, rver = rver)
  rsuite_write_PACKAGES(dest_curl, type = pkg_type)
}
