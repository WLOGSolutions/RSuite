#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Package API related to repository management.
#----------------------------------------------------------------------------

#'
#' Starts managment over repository.
#'
#' @param ra_name name of repository to re-initialize adapter. (type: character)
#' @param ... repository specific parameters. See repo_adapter_create_manager
#'   for concrete implementation of repo adapter for more details.
#' @return repo manager object.
#'
#' @export
#'
repo_mng_start <- function(ra_name, ...) {
  assert(is_nonempty_char1(ra_name), "Non empty character(1) expected for ra_name")

  repo_adapter <- find_repo_adapter(ra_name)
  assert(!is.null(repo_adapter), "RC adapter %s is not registered", ra_name)

  repo_manager <- repo_adapter_create_manager(repo_adapter, ...)

  pkg_loginfo("Repo manager for %s created.", ra_name)
  return(repo_manager)
}

#'
#' Stops managment over repository.
#'
#' @param repo_manager repo manager object retrieved with repo_mgr_start.
#'   (type: rsuite_repo_manager)
#'
#' @export
#'
repo_mng_stop <- function(repo_manager) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")

  repo_manager_destroy(repo_manager)
  pkg_loginfo("Repo manager destroyed.")
}

#'
#' Removes cache of available.packages.
#'
#' @param contrib_url contrib url with cached results for available.packages.
#'
#' @keywords internal
#'
.remove_avails_cache <- function(contrib_url) {
  cache_file <- file.path(tempdir(), paste0("repos_", URLencode(contrib_url, TRUE), ".rds"))
  unlink(cache_file, force = T)
}

#'
#' Retrieve list of packages available in repository.
#'
#' @param repo_manager repo manager to retrieve package list from.
#'   (type: rsuite_repo_manager)
#' @param pkg_type type of packages to retrieve list of.
#'   (type: character, default to platform default pakcage type)
#' @param no.cache it TRUE will delete cached list before retrieving.
#'   (type: locical(1), default: FALSE)
#'
#' @return data.frame of the same structure as available.packages returns.
#'
#' @export
#'
repo_mng_list <- function(repo_manager, pkg_type = .Platform$pkgType, no.cache = FALSE) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")
  assert(is.logical(no.cache), "logical(1) expected for no.cache")

  repo_info <- repo_manager_get_info(repo_manager)
  assert(pkg_type %in% repo_info$types,
         "Package type %s is not supported by the manager. Types supported: %s",
         pkg_type, paste(repo_info$types, collapse = ", "))

  c_url <- rsuite_contrib_url(repos = repo_info$url, type = pkg_type)
  if (any(no.cache)) {
    .remove_avails_cache(c_url)
  }

  avail_pkgs <- available.packages(contriburl = c_url, type = pkg_type, repos = NULL, filters = list())
  avail_pkgs <- data.frame(avail_pkgs, stringsAsFactors = F, row.names = NULL)

  return(avail_pkgs)
}

#'
#' Removes packages specified from repository.
#'
#' @param repo_manager repo manager to remove packages from.
#'   (type: rsuite_repo_manager)
#' @param toremove data.frame with same structure as available.packages returns.
#'   At lease Package and Version columns must be present. (type: data.frame)
#' @param pkg_type type of packages to remove. (type: character, default: .Platform$pkgType)
#'
#' @export
#'
repo_mng_remove <- function(repo_manager, toremove, pkg_type = .Platform$pkgType) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  assert(is.data.frame(toremove) && all(c("Package", "Version") %in% colnames(toremove)),
         "data.frame with at least Package and Version columns expected for toremove")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")

  avails <- repo_mng_list(repo_manager, pkg_type, no.cache = T)
  avails <- merge(x = toremove[, c('Package', 'Version')], y = avails[, c('Package', 'Version', 'Repository')],
                  by.x = c("Package", "Version"), by.y = c("Package", "Version"),
                  all.x = T)

  unknown <- avails[is.na(avails$Repository), ]
  assert(nrow(unknown) == 0,
         "Packages required to remove are not available in repository: %s",
         paste(sprintf("%s (%s)", unknown$Package, unknown$Version), collapse = ", "))

  pkg_loginfo("Removing %s packages from repository ...", nrow(toremove))
  res <- repo_manager_remove(repo_manager, toremove, pkg_type)

  c_url <- rsuite_contrib_url(repos = repo_manager_get_info(repo_manager)$url, type = pkg_type)
  .remove_avails_cache(c_url)

  res$Removed <- T
  res <- merge(x = toremove[, c('Package', 'Version')], y = res[, c('Package', 'Version', 'Removed')],
               by.x = c("Package", "Version"), by.y = c("Package", "Version"),
               all.x = T)

  failed <- res[is.na(res$Removed), ]
  assert(nrow(failed) == 0,
         "Failed to remove packages: %s",
         paste(sprintf("%s (%s)", failed$Package, failed$Version), collapse = ", "))

  pkg_loginfo("Removing %s packages from repository ... done", nrow(toremove))
}

#'
#' Builds and uploads project package(s) into managed repository.
#'
#' If not specified to skip RC it will detect revision version and tag packages
#' before uploading. In that case check for changes in project sources is
#' performed for consistency and project packages will be rebuilt with version
#' altered: revision will be added as least number to package version.
#'
#' Logs all messages onto rsuite logger. Use logging::setLevel to control logs
#' verbosity.
#'
#' @param repo_manager repo manager to use for uploading. (type: rsuite_repo_manager)
#' @param pkgs vector of project packages which should be uploaded into repository
#'   or NULL to upload all project packages (type: characted, default: NULL)
#' @param prj project object to use.If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param skip_rc if TRUE skip detection of package revision and package tagging.
#'    (type: logical, default: FALSE)
#' @param pkg_type type of packages to upload (type: character, default: platform default)
#'
#' @export
#'
repo_upload_prj_packages <- function(repo_manager,
                                     pkgs = NULL,
                                     prj = NULL,
                                     skip_rc = FALSE,
                                     pkg_type = .Platform$pkgType) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")

  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()
  mgr_info <- repo_manager_get_info(repo_manager)

  stopifnot(!is.null(mgr_info$rver))
  assert(mgr_info$rver == params$r_ver,
         "Repository is managed for another R vertion (%s) than the project (%s)",
         mgr_info$rver, params$r_ver)

  assert(pkg_type %in% mgr_info$types,
         "Repository is not managed for %s. Types manageable: %s",
         pkg_type, paste(mgr_info$types, collapse = ", "))

  revision <- NULL
  if (!skip_rc) {
    revision <- detect_consistent_revision(params) # from 15_zip_project.R
    assert(!is.null(revision),
           "Project is not under revision control so revision number cannot be detected")
  }

  build_install_tagged_prj_packages(params, # from 12_build_install_prj_pacakges.R
                                    revision,
                                    build_type = pkg_type,
                                    pkgs = pkgs)

  # sync the repository
  repo_manager_upload(repo_manager, params$irepo_path, pkg_type)

  # clear cache
  c_url <- rsuite_contrib_url(repos = mgr_info$url, type = pkg_type)
  .remove_avails_cache(c_url)
}

#'
#' Uploads file passed into managed repository.
#'
#' Logs all messages onto rsuite logger. Use logging::setLevel to control logs
#' verbosity.
#'
#' @param repo_manager repo manager to use for uploading. (type: rsuite_repo_manager)
#' @param files vector of files to upload. (type: character)
#'
#' @export
#'
repo_upload_package_files <- function(repo_manager, files) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  assert(!missing(files) && is.character(files) && length(files) > 0,
         "Non empty character(N) expected for files")
  assert(all(file.exists(files)),
         "Some files requested to upload do not exist: %s",
         paste(files[!file.exists(files)], collapse = ", "))

  mgr_info <- repo_manager_get_info(repo_manager)

  pkg_infos <- get_package_files_info(files) # from 54_pkg_info.R
  pkg_infos[is.na(pkg_infos$RVersion)]$RVersion <- mgr_info$rver
  pkg_infos$RVersion <- majmin_rver(pkg_infos$RVersion)

  stopifnot(!is.null(mgr_info$rver))
  assert(all(pkg_infos$RVersion == mgr_info$rver),
         "Some files are build for non managed version (R %s): %s",
         mgr_info$rver,
         paste(pkg_infos[pkg_infos$RVersion != mgr_info$rver, ]$File, collapse = ", "))

  assert(length(setdiff(pkg_infos$Type, mgr_info$types)) == 0,
         "Some files are of non supported types (supported: %s): %s",
         paste(mgr_info$types, collapse = ", "),
         paste(pkg_infos[!(pkg_infos$Type %in% mgr_info$types), ]$File, collapse = ", "))

  pkg_types <- unique(pkg_infos$Type)

  tmp_path <- tempfile("upl_temp_repo")
  on.exit({ unlink(tmp_path, recursive = T, force = T) }, add = TRUE)

  pkg_loginfo("Preparing temp repository for %s types ...",
              paste(pkg_types, collapse = ", "))
  tmp_mgr <- repo_manager_dir_create(tmp_path, pkg_types, mgr_info$rver)
  repo_manager_init(tmp_mgr)

  for(tp in pkg_types) {
    dest_path <- rsuite_contrib_url(tmp_path, tp, mgr_info$rver)
    src_files <- pkg_infos[pkg_infos$Type == tp, ]$File
    success <- file.copy(from = src_files, to = dest_path)
    assert(success,
           "Failed to copy to temporary repository: %s",
           paste(src_files, collapse = ", "))

    rsuite_write_PACKAGES(dest_path, tp)
  }

  pkg_loginfo("done.")

  # sync the temp repository
  repo_manager_upload(repo_manager, tmp_path, pkg_types)
  for(tp in pkg_types) {
    c_url <- rsuite_contrib_url(repos = mgr_info$url, type = tp)
    .remove_avails_cache(c_url)
  }
}


#'
#' Uploads external packages passed into managed repository. It uses project
#' to detect repositories to look for packages in.
#'
#' Logs all messages onto rsuite logger. Use logging::setLevel to control logs
#' verbosity.
#'
#' @param repo_manager repo manager to use for uploading. (type: rsuite_repo_manager)
#' @param pkgs vector of names of external packages which should be included in
#'   PKGZIP. (type: characted)
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param pkg_type type of packages to upload (type: character, default: platform default)
#'
#' @export
#'
repo_upload_ext_packages <- function(repo_manager,
                                     pkgs,
                                     prj = NULL,
                                     pkg_type = .Platform$pkgType) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  assert(!missing(pkgs) && is.character(pkgs) && length(pkgs) > 0,
         "Non empty character(N) expected for pkgs")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")

  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()

  mgr_info <- repo_manager_get_info(repo_manager)
  stopifnot(!is.null(mgr_info$rver))

  pkg_loginfo("Detecting repositories (for R %s)...", mgr_info$rver)

  repo_infos <- get_all_repo_infos(params, rver = mgr_info$rver) # from 53_repositories.R
  log_repo_infos(repo_infos) # from 53_repositories.R

  assert(pkg_type %in% mgr_info$types,
         "Repository is not managed for %s. Types manageable: %s",
         pkg_type, paste(mgr_info$types, collapse = ", "))

  stopifnot(!is.null(mgr_info$rver))
  contrib_urls <- retrieve_contrib_urls(repo_infos,  # from 53_repositories.R
                                        type = pkg_type)

  # retrieves latest versions
  avail_pkgs <- vers.get_available_pkgs(pkgs, contrib_urls)

  unknown_pkgs <- setdiff(pkgs, avail_pkgs$Package)
  if (pkg_type != "source") { # try source packages to build
    src_contrib_urls <- retrieve_contrib_urls(repo_infos, "source") # from 53_repositories.R
    src_avail_pkgs <- vers.get_available_pkgs(unknown_pkgs, src_contrib_urls)
    unknown_pkgs <- setdiff(unknown_pkgs, src_avail_pkgs$Package)
  } else {
    src_avail_pkgs <- data.frame(Package = as.character())
  }
  assert(!length(unknown_pkgs),
         "Some packages are not available for %s type: %s",
         pkg_type, paste(unknown_pkgs, collapse = ", "))

  all_names <- avail_pkgs$Package[order(avail_pkgs$Package)]

  tmp_path <- tempfile("pkgzip_temp_repo")
  on.exit({ unlink(tmp_path, recursive = T, force = T) }, add = TRUE)

  pkg_loginfo("Preparing temp repository for packages ...")

  tmp_mgr <- repo_manager_dir_create(tmp_path, pkg_type, mgr_info$rver)
  repo_manager_init(tmp_mgr)

  pkg_download(avail_pkgs,
               dest_dir = rsuite_contrib_url(tmp_path, pkg_type, mgr_info$rver))
  if (nrow(src_avail_pkgs) > 0) {
    build_source_packages(src_avail_pkgs, # from 17_build_src_packages.R
                          tmp_path, pkg_type, params, mgr_info$rver)
  }

  pkg_loginfo("... done.")

  repo_manager_upload(repo_manager, tmp_path, pkg_type)
  c_url <- rsuite_contrib_url(repos = mgr_info$url, type = pkg_type)
  .remove_avails_cache(c_url)
}


#'
#' Uploads PKGZIP into managed repository.
#'
#' Logs all messages onto rsuite logger. Use logging::setLevel to control logs
#' verbosity.
#'
#' @param repo_manager repo manager to use for uploading. (type: rsuite_repo_manager)
#' @param pkgzip PKGZIP path to upload. It must exist. (type: character(1))
#'
#' @export
#'
repo_upload_pkgzip <- function(repo_manager, pkgzip) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  assert(is_nonempty_char1(pkgzip),
         "Non empty character(1) expected for pkgzip")
  assert(file.exists(pkgzip), "Existing PKGZIP expected for pkgzip")

  prj <- safe_get_prj(NULL)
  stopifnot(!is.null(prj))

  params <- prj$load_params()

  mgr_info <- repo_manager_get_info(repo_manager)

  tmp_path <- tempfile("upl_temp_repo")
  on.exit({ unlink(tmp_path, recursive = T, force = T) }, add = TRUE)

  pkg_loginfo("Preparing temp repository ...")

  success <- unzip_folder(dest_dir = tmp_path, zip_file_path = pkgzip)
  assert(success, "Failed to unzip file (unzip returned non 0 return status).")

  types <- c("source", "win.binary", "mac.binary", "binary")
  types <- Filter(x = types,
                  f = function(tp) { dir.exists(rsuite_contrib_url(tmp_path, tp, mgr_info$rver)) })

  pkg_loginfo("... done. It contains following types: %s", paste(types, collapse = ", "))

  repo_manager_upload(repo_manager, tmp_path, types)
  for(tp in types) {
    c_url <- rsuite_contrib_url(repos = mgr_info$url, type = tp)
    .remove_avails_cache(c_url)
  }
}


#'
#' Loads package from github repository, packages it into package file and uploads.
#'
#' It will search dependencies in provided project repositories.
#'
#' Logs all messages onto rsuite logger. Use logging::setLevel to control logs
#' verbosity.
#'
#' @param repo_manager repo manager to use for uploading. (type: rsuite_repo_manager)
#' @param repo repository address in format username/repo[/subdir][\@ref|#pull]. See
#'   devtools::install_github for more information.
#' @param ... github specific parametrs passed to devtools::install_github.
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param pkg_type type of packages to upload (type: character, default: platform default)
#'
#' @export
#'
repo_upload_github_package <- function(repo_manager, repo, ..., prj = NULL, pkg_type = .Platform$pkgType) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  assert(is_nonempty_char1(repo), "Non empty character(1) expected for repo")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")

  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()

  mgr_info <- repo_manager_get_info(repo_manager)

  bld_prj <- prj_start(name = basename(tempfile(pattern = "srcrepo_proj_")),
                       path = tempdir(),
                       skip_rc = T)
  on.exit({ unlink(bld_prj$path, recursive = T, force = T) }, add = TRUE)

  prj_config_set_rversion(rver = mgr_info$rver, prj = bld_prj)
  prj_config_set_repo_adapters(make_detached_repos(params), prj = bld_prj)

  pkg_name <- get_srcrepo_package(bld_prj, "github", repo, ...)

  unlink(list.files(bld_prj$load_params()$script_path, # not to include default packages
                    pattern = ".+[.]R$", full.names = T),
         force = T)
  prj_install_deps(bld_prj)

  repo_upload_prj_packages(repo_manager, pkgs = pkg_name, prj = bld_prj,
                           skip_rc = T, pkg_type = pkg_type)
}
