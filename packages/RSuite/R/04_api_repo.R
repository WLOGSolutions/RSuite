#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Package API related to repository management.
#----------------------------------------------------------------------------

#'
#' Starts management over the repository.
#'
#' Creates object to manage the repository.
#'
#' @param ra_name name of the repository to whose adapter will be re-initialized. (type: character)
#' @param ... repository specific parameters. See repo_adapter_create_manager
#'   for the concrete implementation of repo adapter for more details.
#' @return repo manager object.
#'
#' @family in repository management
#'
#' @examples
#' # create exemplary project base folder
#' prj_base <- tempfile("example_")
#' dir.create(prj_base, recursive = TRUE, showWarnings = FALSE)
#'
#' # start project
#' prj <- prj_start("my_project", skip_rc = TRUE, path = prj_base)
#'
#' # set it to use in project repository and CRAN
#' prj_config_set_repo_adapters(c("Dir", "CRAN"), prj = prj)
#'
#' # start managing in project repository
#' rmgr <- repo_mng_start("Dir", prj = prj, ix = 1)
#'
#' # stop repository management
#' repo_mng_stop(rmgr)
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
#' Initializes repository (creates its structure).
#'
#' @param repo_manager repo manager object retrieved with repo_mgr_start.
#'   (type: rsuite_repo_manager)
#'
#' @family in repository management
#'
#' @examples
#' # create exemplary project base folder
#' prj_base <- tempfile("example_")
#' dir.create(prj_base, recursive = TRUE, showWarnings = FALSE)
#'
#' # start project
#' prj <- prj_start("my_project", skip_rc = TRUE, path = prj_base)
#'
#' # set it to use in project repository and CRAN
#' prj_config_set_repo_adapters(c("Dir", "CRAN"), prj = prj)
#'
#' # start managing in project repository
#' rmgr <- repo_mng_start("Dir", prj = prj, ix = 1)
#'
#' # initialize its structure
#' repo_mng_init(rmgr)
#'
#' # stop repository management
#' repo_mng_stop(rmgr)
#'
#' @export
#'
repo_mng_init <- function(repo_manager) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  repo_manager_init(repo_manager)
}

#'
#' Stops management over the repository.
#'
#' @param repo_manager repo manager object retrieved with repo_mgr_start.
#'   (type: rsuite_repo_manager)
#'
#' @family in repository management
#'
#' @examples
#' # create exemplary project base folder
#' prj_base <- tempfile("example_")
#' dir.create(prj_base, recursive = TRUE, showWarnings = FALSE)
#'
#' # start project
#' prj <- prj_start("my_project", skip_rc = TRUE, path = prj_base)
#'
#' # set it to use in project repository and CRAN
#' prj_config_set_repo_adapters(c("Dir", "CRAN"), prj = prj)
#'
#' # start managing in project repository
#' rmgr <- repo_mng_start("Dir", prj = prj, ix = 1)
#'
#' # stop repository management
#' repo_mng_stop(rmgr)
#'
#' @export
#'
repo_mng_stop <- function(repo_manager) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")

  repo_manager_destroy(repo_manager)
  pkg_loginfo("Repo manager destroyed.")
}

#'
#' Retrieve the list of available packages in the repository.
#'
#' @param repo_manager repo manager to retrieve package list from.
#'   (type: rsuite_repo_manager)
#' @param pkg_type type of packages to retrieve list of.
#'   (type: character, default to platform default package type)
#' @param no.cache it TRUE will delete cached list before retrieving.
#'   (type: logical(1), default: FALSE)
#'
#' @return data.frame of the same structure as available.packages returns.
#'
#' @family in repository management
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
#'   # set it to use in project repository and CRAN
#'   prj_config_set_repo_adapters(c("Dir", "CRAN"), prj = prj)
#'
#'   # start managing in project repository
#'   rmgr <- repo_mng_start("Dir", prj = prj, ix = 1)
#'
#'   # upload logging package from CRAN into the repository
#'   repo_upload_ext_packages(rmgr, pkgs = "logging", prj = prj)
#'
#'   # list available packages
#'   repo_mng_list(rmgr)
#'
#'   # stop repository management
#'   repo_mng_stop(rmgr)
#' }
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

  if (any(no.cache)) {
    clear_available_packages_cache(repo_info$url, # from 53_repositories.R
                                   type = pkg_type, rver = repo_info$rver)
  }

  avail_pkgs <- get_available_packages(repo_info$url,  # from 53_repositories.R
                                       type = pkg_type, rver = repo_info$rver)
  return(avail_pkgs)
}

#'
#' Removes packages from the repository.
#'
#' @param repo_manager repo manager to remove packages from.
#'   (type: rsuite_repo_manager)
#' @param toremove data.frame with same structure as available.packages returns.
#'   At lease Package and Version columns must be present. (type: data.frame)
#' @param pkg_type type of packages to remove.
#'   (type: character, default: .Platform$pkgType)
#'
#' @family in repository management
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
#'   # set it to use in project repository and CRAN
#'   prj_config_set_repo_adapters(c("Dir", "CRAN"), prj = prj)
#'
#'   # start managing in project repository
#'   rmgr <- repo_mng_start("Dir", prj = prj, ix = 1)
#'
#'   # upload logging package from CRAN into the repository
#'   repo_upload_ext_packages(rmgr, pkgs = "logging", prj = prj)
#'
#'   # list available packages before removal
#'   avail_pkgs <- repo_mng_list(rmgr)
#'   avail_pkgs
#'
#'   # remove logging from the repository
#'   repo_mng_remove(rmgr, avail_pkgs[avail_pkgs$Package == "logging", ])
#'
#'   # list available packages after removal
#'   repo_mng_list(rmgr)
#'
#'   # stop repository management
#'   repo_mng_stop(rmgr)
#' }
#'
#' @export
#'
repo_mng_remove <- function(repo_manager, toremove, pkg_type = .Platform$pkgType) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  assert(is.data.frame(toremove) && all(c("Package", "Version") %in% colnames(toremove)),
         "data.frame with at least Package and Version columns expected for toremove")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")

  avails <- repo_mng_list(repo_manager, pkg_type, no.cache = TRUE)
  avails <- merge(x = toremove[, c("Package", "Version")],
                  y = avails[, c("Package", "Version", "Repository")],
                  by.x = c("Package", "Version"), by.y = c("Package", "Version"),
                  all.x = TRUE)

  unknown <- avails[is.na(avails$Repository), ]
  assert(nrow(unknown) == 0,
         "Packages required to remove are not available in repository: %s",
         paste(sprintf("%s (%s)", unknown$Package, unknown$Version), collapse = ", "))

  pkg_loginfo("Removing %s packages from repository ...", nrow(toremove))
  res <- repo_manager_remove(repo_manager, toremove, pkg_type)

  mgr_info <- repo_manager_get_info(repo_manager)
  clear_available_packages_cache(mgr_info$url, # from 53_repositories.R
                                 type = pkg_type, rver = mgr_info$rver)

  res$Removed <- TRUE
  res <- merge(x = toremove[, c("Package", "Version")], y = res[, c("Package", "Version", "Removed")],
               by.x = c("Package", "Version"), by.y = c("Package", "Version"),
               all.x = TRUE)

  failed <- res[is.na(res$Removed), ]
  assert(nrow(failed) == 0,
         "Failed to remove packages: %s",
         paste(sprintf("%s (%s)", failed$Package, failed$Version), collapse = ", "))

  pkg_loginfo("Removing %s packages from repository ... done", nrow(toremove))
}

#'
#' Builds and uploads project package(s) into the repository.
#'
#' @details
#' If not specified to skip RC it will detect revision version and tag packages
#' before uploading. In that case, a check for changes in the project sources is
#' performed for consistency and project packages will be rebuilt with version
#' altered: revision will be added as the least number to package version.
#'
#' Logs all messages onto the rsuite logger. Use \code{logging::setLevel} to
#' control logs verbosity.
#'
#' @param repo_manager repo manager to use for uploading. (type: rsuite_repo_manager)
#' @param pkgs vector of project packages which should be uploaded into the repository
#'   or NULL to upload all project packages (type: character, default: NULL)
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param skip_rc if TRUE skip detection of package revision and package tagging.
#'    (type: logical, default: FALSE)
#' @param pkg_type type of packages to upload (type: character, default: platform default)
#' @param with_deps If TRUE will include pkgs dependencies while uploading into the
#'    repository. Packages in repository satisfying pkgs requirements will not be
#'    included. (type: logical, default: FALSE)
#' @param skip_build_steps character vector with steps to skip while building
#'    project packages. Can contain following entries:
#' \describe{
#'   \item{specs}{Process packages specifics}
#'   \item{docs}{Try build documentation with roxygen}
#'   \item{imps}{Perform imports validation}
#'   \item{tests}{Run package tests}
#'   \item{rcpp_attribs}{Run rppAttribs on the package}
#'   \item{vignettes}{Build package vignettes}
#' }
#' (type: character(N), default: NULL).
#'
#' @family in repository management
#'
#' @examples
#' \donttest{
#'   # create exemplary project base folder
#'   prj_base <- tempfile("example_")
#'   dir.create(prj_base, recursive = TRUE, showWarnings = FALSE)
#'
#'   # start src project
#'   src_prj <- prj_start("my_project_src", skip_rc = TRUE, path = prj_base)
#'
#'   # create project package
#'   prj_start_package("mypackage", prj = src_prj, skip_rc = TRUE)
#'
#'   # build project environment
#'   prj_install_deps(prj = src_prj)
#'
#'   # start dest project
#'   dst_prj <- prj_start("my_project_dst", skip_rc = TRUE, path = prj_base)
#'
#'   # set dest to use in project repository and CRAN
#'   prj_config_set_repo_adapters(c("Dir", "CRAN"), prj = dst_prj)
#'
#'   # start managing in project repository
#'   rmgr <- repo_mng_start("Dir", prj = dst_prj, ix = 1)
#'
#'   # upload mypackage from src into dest's in project repository
#'   repo_upload_prj_packages(rmgr, prj = src_prj, skip_rc = TRUE)
#'
#'   # list available packages
#'   repo_mng_list(rmgr)
#'
#'   # stop repository management
#'   repo_mng_stop(rmgr)
#' }
#'
#' @export
#'
repo_upload_prj_packages <- function(repo_manager,
                                     pkgs = NULL,
                                     prj = NULL,
                                     skip_rc = FALSE,
                                     pkg_type = .Platform$pkgType,
                                     with_deps = FALSE,
                                     skip_build_steps = NULL) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")
  assert(is.logical(with_deps), "Logical value expected for with_deps")
  if (!is.null(skip_build_steps)) {
    assert(is.character(skip_build_steps)
           && all(skip_build_steps %in% c("spec", "docs", "imps", "tests", "rcpp_attribs", "vignettes")),
           paste("character(N) expected for skip_build_steps containing entities",
                 "spec, docs, imps, tests, rcpp_attribs or vignettes"))
  }

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

  prj_pkgs <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  assert(length(prj_pkgs) > 0, "The project does not have packages to upload into repository")

  if (is.null(pkgs)) {
    pkgs <- prj_pkgs
  } else {
    assert(all(pkgs %in% prj_pkgs),
           "Packages requested to upload are not present in project: %s",
           paste(setdiff(pkgs, prj_pkgs), collapse = ", "))
    pkgs <- prj_pkgs[prj_pkgs %in% pkgs]
  }

  revision <- NULL
  if (!skip_rc) {
    revision <- detect_consistent_revision(params) # from 15_zip_project.R
    assert(!is.null(revision),
           "Project is not under revision control so revision number cannot be detected")
  }

  if (any(with_deps)) {
    raw_vers <- lapply(X = names(pkgs),
                       FUN = function(pkg_dir) {
                         collect_single_pkg_direct_deps(params, pkg_dir, pkgs[[pkg_dir]])
                       })
    dep_vers <- do.call("vers.union", raw_vers)
    inproj_deps <- intersect(prj_pkgs, vers.get_names(dep_vers))
    avail_vers <- collect_dependencies(vers.rm(dep_vers, inproj_deps), # from 18_repo_helpers.R
                                       pkg_type, params, mgr_info$url)

    pkgs <- c(pkgs, inproj_deps) # include also in-project dependencies
  } else {
    avail_vers <- vers.build(avails = data.frame())
  }

  pkg_loginfo("Building project packages ...")
  build_install_tagged_prj_packages(params, # from 12_build_install_prj_pacakges.R
                                    revision,
                                    build_type = pkg_type,
                                    skip_build_steps = skip_build_steps)

  tmp_path <- tempfile("pkgzip_temp_repo")
  on.exit({
    unlink(tmp_path, recursive = TRUE, force = TRUE)
  },
  add = TRUE)

  temp_repo_prepare(avail_vers, tmp_path, pkg_type, params) # from 18_repo_helpers.R
  temp_repo_copy_proj_pkgs(pkgs, tmp_path, pkg_type, params) # from 18_repo_helpers.R
  temp_repo_write_PACKAGES(tmp_path, pkg_type, rver = params$r_ver) # from 18_repo_helpers.R

  pkg_loginfo("... done. Uploading to repository ...")
  repo_manager_upload(repo_manager, tmp_path, pkg_type)

  clear_available_packages_cache(mgr_info$url, # from 53_repositories.R
                                 type = pkg_type, rver = mgr_info$rver)
}

#'
#' Uploads package file(s) into the managed repository.
#'
#' @details
#' Logs all messages onto the rsuite logger. Use \code{logging::setLevel} to control
#' logs verbosity.
#'
#' @param repo_manager repo manager to use for uploading. (type: rsuite_repo_manager)
#' @param files vector of files to upload. (type: character)
#'
#' @family in repository management
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
#'   # set it to use in project repository and CRAN
#'   prj_config_set_repo_adapters(c("Dir", "CRAN"), prj = prj)
#'
#'   # start managing in project repository
#'   rmgr <- repo_mng_start("Dir", prj = prj, ix = 1)
#'
#'   # download logging package
#'   pkg_fpath <- utils::download.packages("logging",
#'                                         repos = "https://cloud.r-project.org/",
#'                                         destdir = tempdir(),
#'                                         type = "source")[1,2]
#'
#'   # upload downloaded package into the repository
#'   repo_upload_package_files(rmgr, files = pkg_fpath)
#'
#'   # list available packages
#'   repo_mng_list(rmgr, pkg_type = "source")
#'
#'   # stop repository management
#'   repo_mng_stop(rmgr)
#' }
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
  on.exit({
    unlink(tmp_path, recursive = TRUE, force = TRUE)
  },
  add = TRUE)

  pkg_loginfo("Preparing temp repository for %s types ...",
              paste(pkg_types, collapse = ", "))
  tmp_mgr <- repo_manager_dir_create(tmp_path, pkg_types, mgr_info$rver)
  repo_manager_init(tmp_mgr)

  for (tp in pkg_types) {
    dest_path <- rsuite_contrib_url(tmp_path, tp, mgr_info$rver)
    src_files <- pkg_infos[pkg_infos$Type == tp, ]$Path
    success <- file.copy(from = src_files, to = dest_path)
    assert(success,
           "Failed to copy to temporary repository: %s",
           paste(src_files, collapse = ", "))

    rsuite_write_PACKAGES(dest_path, tp)
  }

  pkg_loginfo("done.")

  # sync the temp repository
  repo_manager_upload(repo_manager, tmp_path, pkg_types)
  for (tp in pkg_types) {
    clear_available_packages_cache(mgr_info$url, # from 53_repositories.R
                                   type = tp, rver = mgr_info$rver)
  }
}


#'
#' Uploads external packages into the managed repository.
#'
#' It uses the project to detect repositories to look for external packages in.
#'
#' @details
#' Logs all messages onto rsuite logger. Use \code{logging::setLevel} to
#' control logs verbosity.
#'
#' @param repo_manager repo manager to use for uploading. (type: rsuite_repo_manager)
#' @param pkgs vector of names of external packages which should be included in
#'   PKGZIP. (type: character)
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param pkg_type type of packages to upload (type: character, default: platform default)
#' @param with_deps If TRUE will include pkgs dependencies while uploading into the
#'    repository. Packages in repository satisfying pkgs requirements will not be
#'    included. (type: logical, default: FALSE)
#'
#' @family in repository management
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
#'   # set it to use in project repository and CRAN
#'   prj_config_set_repo_adapters(c("Dir", "CRAN"), prj = prj)
#'
#'   # start managing in project repository
#'   rmgr <- repo_mng_start("Dir", prj = prj, ix = 1)
#'
#'   # upload logging package from CRAN into the repository
#'   repo_upload_ext_packages(rmgr, "logging", prj = prj, pkg_type = "source")
#'
#'   # list available packages
#'   repo_mng_list(rmgr, pkg_type = "source")
#'
#'   # stop repository management
#'   repo_mng_stop(rmgr)
#' }
#'
#' @export
#'
repo_upload_ext_packages <- function(repo_manager,
                                     pkgs,
                                     prj = NULL,
                                     pkg_type = .Platform$pkgType,
                                     with_deps = FALSE) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  assert(!missing(pkgs) && is.character(pkgs) && length(pkgs) > 0,
         "Non empty character(N) expected for pkgs")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")
  assert(is.logical(with_deps), "Logical value expected for with_deps")

  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()

  mgr_info <- repo_manager_get_info(repo_manager)
  stopifnot(!is.null(mgr_info$rver))

  assert(pkg_type %in% mgr_info$types,
         "Repository is not managed for %s. Types manageable: %s",
         pkg_type, paste(mgr_info$types, collapse = ", "))

  if (any(with_deps)) {
    avail_vers <- collect_dependencies(vers.build(pkgs), # from 18_repo_helpers.R
                                       pkg_type, params, mgr_info$url,
                                       rver = mgr_info$rver)
  } else {
    avail_vers <- collect_packages(vers.build(pkgs), # from 18_repo_helpers.R
                                   pkg_type, params,
                                   rver = mgr_info$rver)
  }

  tmp_path <- tempfile("pkgzip_temp_repo")
  on.exit({
    unlink(tmp_path, recursive = TRUE, force = TRUE)
  },
  add = TRUE)

  temp_repo_prepare(avail_vers, # from 18_repo_helpers.R
                    tmp_path, pkg_type, params,
                    rver = mgr_info$rver)
  temp_repo_write_PACKAGES(tmp_path, pkg_type, rver = mgr_info$rver) # from 18_repo_helpers.R

  pkg_loginfo("... done. Uploading to repository ...")

  repo_manager_upload(repo_manager, tmp_path, pkg_type)
  clear_available_packages_cache(mgr_info$url, # from 53_repositories.R
                                 type = pkg_type, rver = mgr_info$rver)
}


#'
#' Uploads PKGZIP into the managed repository.
#'
#' @details
#' Logs all messages onto the rsuite logger. Use \code{logging::setLevel} to
#' control logs verbosity.
#'
#' @param repo_manager repo manager to use for uploading. (type: rsuite_repo_manager)
#' @param pkgzip PKGZIP path to upload. It must exist. (type: character(1))
#'
#' @family in repository management
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
#'   # set it to use in project repository and CRAN
#'   prj_config_set_repo_adapters(c("Dir", "CRAN"), prj = prj)
#'
#'   # start managing in project repository
#'   rmgr <- repo_mng_start("Dir", prj = prj, ix = 1)
#'
#'   # create PKGZIP containing logging package
#'   pkgzip_fpath <- pkgzip_build_ext_packages("logging", prj = prj, pkg_type = "source",
#'                                             path = tempdir())
#'
#'   # upload PKGZIP into the repository
#'   repo_upload_pkgzip(rmgr, pkgzip_fpath)
#'
#'   # list available packages
#'   repo_mng_list(rmgr, pkg_type = "source")
#'
#'   # stop repository management
#'   repo_mng_stop(rmgr)
#' }
#'
#' @export
#'
repo_upload_pkgzip <- function(repo_manager, pkgzip) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  assert(is_nonempty_char1(pkgzip),
         "Non empty character(1) expected for pkgzip")
  assert(file.exists(pkgzip), "Existing PKGZIP expected for pkgzip")

  mgr_info <- repo_manager_get_info(repo_manager)

  tmp_path <- tempfile("upl_temp_repo")
  on.exit({
    unlink(tmp_path, recursive = TRUE, force = TRUE)
  },
  add = TRUE)

  pkg_loginfo("Preparing temp repository ...")

  success <- unzip_folder(dest_dir = tmp_path, zip_file_path = pkgzip)
  assert(success, "Failed to unzip file (unzip returned non 0 return status).")

  types <- c("source", "win.binary", "mac.binary", "binary", .Platform$pkgType)
  types <- Filter(x = types,
                  f = function(tp) {
                    dir.exists(rsuite_contrib_url(tmp_path, tp, mgr_info$rver))
                  })

  pkg_loginfo("... done. It contains following types: %s", paste(types, collapse = ", "))

  repo_manager_upload(repo_manager, tmp_path, types)
  for (tp in types) {
    clear_available_packages_cache(mgr_info$url, # from 53_repositories.R
                                   type = tp, rver = mgr_info$rver)
  }
}


#'
#' Loads package from the GitHub repository.
#'
#' It will download GitHub repository, build package into package file and will
#' upload it into the repository. It will search dependencies in provided
#' project's repositories.
#'
#' @details
#' Logs all messages onto rsuite logger. Use \code{logging::setLevel} to
#' control logs verbosity.
#'
#' @param repo_manager repo manager to use for uploading. (type: rsuite_repo_manager)
#' @param repo repository address in format username/repo[/subdir][\@ref|#pull]. See
#'   \code{devtools::install_github} for more information.
#' @param ... GitHub specific parameters passed to \code{devtools::install_github}.
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param pkg_type type of packages to upload (type: character, default: platform default)
#' @param with_deps If TRUE will include pkgs dependencies while uploading into the
#'    repository. Packages in repository satisfying pkgs requirements will not be
#'    included. (type: logical, default: FALSE)
#' @param skip_build_steps character vector with steps to skip while building
#'    project packages. Can contain following entries:
#' \describe{
#'   \item{specs}{Process packages specifics}
#'   \item{docs}{Try build documentation with roxygen}
#'   \item{imps}{Perform imports validation}
#'   \item{tests}{Run package tests}
#'   \item{rcpp_attribs}{Run rppAttribs on the package}
#'   \item{vignettes}{Build package vignettes}
#' }
#' (type: character(N), default: NULL).
#' @param keep_sources if TRUE downloaded package sources will not be removed
#'   after building. (type: logical, default: FALSE)
#'
#' @family in repository management
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
#'   # set it to use in project repository and CRAN
#'   prj_config_set_repo_adapters(c("Dir", "CRAN"), prj = prj)
#'
#'   # start managing in project repository
#'   rmgr <- repo_mng_start("Dir", prj = prj, ix = 1)
#'
#'   # upload logging package from cran repository
#'   repo_upload_github_package(rmgr, repo = "cran/logging",
#'                              prj = prj, pkg_type = "source")
#'
#'   # list available packages
#'   repo_mng_list(rmgr, pkg_type = "source")
#'
#'   # stop repository management
#'   repo_mng_stop(rmgr)
#' }
#'
#' @export
#'
repo_upload_github_package <- function(repo_manager, repo, ...,
                                       prj = NULL,
                                       pkg_type = .Platform$pkgType,
                                       with_deps = FALSE,
                                       skip_build_steps = NULL,
                                       keep_sources = FALSE) {
  .repo_upload_srcrepo_package(repo_manager, "github", repo, ...,
                               prj = prj,
                               pkg_type = pkg_type,
                               with_deps = with_deps,
                               skip_build_steps = skip_build_steps,
                               keep_sources = keep_sources)
}

#'
#' Loads package from the Bioconductor repository.
#'
#' It will download Bioconductor repository, build package into package file and will
#' upload it into the repository. It will search dependencies in provided
#' project's repositories.
#'
#' @details
#' Logs all messages onto rsuite logger. Use \code{logging::setLevel} to
#' control logs verbosity.
#'
#' @param repo_manager repo manager to use for uploading. (type: rsuite_repo_manager)
#' @param repo repository address in format [username:password@][release/]repo[#revision]. See
#'   \code{devtools::install_bioc} for more information.
#' @param ... Bioconductor specific parameters passed to \code{devtools::install_bioc}.
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param pkg_type type of packages to upload (type: character, default: platform default)
#' @param with_deps If TRUE will include pkgs dependencies while uploading into the
#'    repository. Packages in repository satisfying pkgs requirements will not be
#'    included. (type: logical, default: FALSE)
#' @param skip_build_steps character vector with steps to skip while building
#'    project packages. Can contain following entries:
#' \describe{
#'   \item{specs}{Process packages specifics}
#'   \item{docs}{Try build documentation with roxygen}
#'   \item{imps}{Perform imports validation}
#'   \item{tests}{Run package tests}
#'   \item{rcpp_attribs}{Run rppAttribs on the package}
#'   \item{vignettes}{Build package vignettes}
#' }
#' (type: character(N), default: NULL).
#' @param keep_sources if TRUE downloaded package sources will not be removed
#'   after building. (type: logical, default: FALSE)
#'
#' @family in repository management
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
#'   # set it to use in project repository and CRAN
#'   prj_config_set_repo_adapters(c("Dir", "CRAN"), prj = prj)
#'
#'   # start managing in project repository
#'   rmgr <- repo_mng_start("Dir", prj = prj, ix = 1)
#'
#'   # upload logging package from cran repository
#'   repo_upload_bioc_package(rmgr, repo = "BiocGenerics",
#'                            prj = prj, pkg_type = "source")
#'
#'   # list available packages
#'   repo_mng_list(rmgr, pkg_type = "source")
#'
#'   # stop repository management
#'   repo_mng_stop(rmgr)
#' }
#'
#' @export
#'
repo_upload_bioc_package <- function(repo_manager, repo, ...,
                                     prj = NULL,
                                     pkg_type = .Platform$pkgType,
                                     with_deps = FALSE,
                                     skip_build_steps = NULL,
                                     keep_sources = FALSE) {
  .repo_upload_srcrepo_package(repo_manager, "bioc", repo, ...,
                               prj = prj,
                               pkg_type = pkg_type,
                               with_deps = with_deps,
                               skip_build_steps = skip_build_steps,
                               keep_sources = keep_sources)
}

#'
#' Loads package from the source repository of specified type.
#'
#' It will download source repository, build package into package file and will
#' upload it into the repository. It will search dependencies in provided
#' project's repositories.
#'
#' @param repo_manager repo manager to use for uploading. (type: rsuite_repo_manager)
#' @param srcrepo_type type of src repository (one of: github, git, svn, bioc, bitbucket, url).
#' @param repo src repository specific reference. see apropriate devtools::<type>_install
#'   documentation. (type: character)
#' @param ... src repository specific parameters. see apropriate devtools::<type>_install
#'   documentation.
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param pkg_type type of packages to upload (type: character, default: platform default)
#' @param with_deps If TRUE will include pkgs dependencies while uploading into the
#'    repository. Packages in repository satisfying pkgs requirements will not be
#'    included. (type: logical, default: FALSE)
#' @param skip_build_steps character vector with steps to skip while building
#'    project packages. Can contain following entries:
#' \describe{
#'   \item{specs}{Process packages specifics}
#'   \item{docs}{Try build documentation with roxygen}
#'   \item{imps}{Perform imports validation}
#'   \item{tests}{Run package tests}
#'   \item{rcpp_attribs}{Run rppAttribs on the package}
#'   \item{vignettes}{Build package vignettes}
#' }
#' (type: character(N), default: NULL).
#' @param keep_sources if TRUE downloaded package sources will not be removed
#'   after building. (type: logical, default: FALSE)
#'
#' @keywords internal
#' @noRd
#'
.repo_upload_srcrepo_package <- function(repo_manager, srcrepo_type, repo, ...,
                                         prj = NULL,
                                         pkg_type = .Platform$pkgType,
                                         with_deps = FALSE,
                                         skip_build_steps = NULL,
                                         keep_sources = FALSE) {
  assert(is_repo_manager(repo_manager), "Repo manager expected for repo_manager")
  assert(is_nonempty_char1(srcrepo_type), "Non empty character(1) expected for srcrepo_type")
  assert(is_nonempty_char1(repo), "Non empty character(1) expected for repo")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")
  assert(is.logical(with_deps), "logical expected for with_deps")
  if (!is.null(skip_build_steps)) {
    assert(is.character(skip_build_steps)
           && all(skip_build_steps %in% c("spec", "docs", "imps", "tests", "rcpp_attribs", "vignettes")),
           paste("character(N) expected for skip_build_steps containing entities",
                 "spec, docs, imps, tests, rcpp_attribs or vignettes"))
  }
  assert(is.logical(keep_sources), "logical expected for keep_sources")

  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()

  mgr_info <- repo_manager_get_info(repo_manager)

  bld_prj_path <- tempfile(pattern = "srcrepo_proj_")
  if (!any(keep_sources)) {
    on.exit({
      unlink(bld_prj_path, recursive = TRUE, force = TRUE)
    },
    add = TRUE)
  } else {
    bld_prj_path <- file.path(dirname(dirname(bld_prj_path)), basename(bld_prj_path))
    pkg_loginfo("Will keep build project sources at %s", bld_prj_path)
  }

  bld_prj <- prj_start(name = basename(bld_prj_path),
                       path = dirname(bld_prj_path),
                       skip_rc = TRUE)

  prj_config_set_rversion(rver = mgr_info$rver, prj = bld_prj)
  prj_config_set_repo_adapters(make_detached_repos(params), prj = bld_prj)

  pkg_info <- get_srcrepo_package(bld_prj, srcrepo_type, repo, ...)

  unlink(list.files(bld_prj$load_params()$script_path, # not to include default packages
                    pattern = ".+[.]R$", full.names = TRUE),
         force = TRUE)
  prj_install_deps(bld_prj, vanilla_sups = TRUE)

  repo_upload_prj_packages(repo_manager, pkgs = pkg_info$name, prj = bld_prj,
                           skip_rc = TRUE, pkg_type = pkg_type,
                           with_deps = with_deps,
                           skip_build_steps = skip_build_steps)
}
