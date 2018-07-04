#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Package API related to PKGZIP building.
#----------------------------------------------------------------------------

#'
#' Build PKGZIP name out of package names with optional prefix and suffix.
#'
#' @param pkg_names names of packages (type: character(N))
#' @param pref prefix to add before pkg_names (type: character(1))
#' @param suff suffix to add after pkg_names (type: character(1))
#'
#' @return name of PKGZIP file.
#'
#' @keywords internal
#' @noRd
#'
.pkgzip_file_name <- function(pkg_names, pref = "", suff = "") {
  pkg_names <- unique(pkg_names)
  pkg_names <- pkg_names[order(pkg_names)]

  if (nchar(pref) > 0) {
    pref <- paste0(pref, "_")
  }
  if (nchar(suff) > 0) {
    suff <- paste0("_", suff)
  }

  if (length(pkg_names) > 5) {
    zip_file_name <- sprintf("%s_pkgzip_%s%s_and_%sothers%s.zip",
                             Sys.Date(),
                             pref,
                             paste(pkg_names[1:5], collapse = "_"), length(pkg_names) - 5,
                             suff)
  } else {
    zip_file_name <- sprintf("%s_pkgzip_%s%s%s.zip",
                             Sys.Date(),
                             pref,
                             paste(pkg_names, collapse = "_"),
                             suff)
  }
  return(zip_file_name)
}

#'
#' Builds PKGZIP out of project packages.
#'
#' @details
#' PKGZIP will be tagged with the same way as project zip.
#'
#' Logs all messages onto rsuite logger. Use  \code{logging::setLevel} to control logs
#' verbosity.
#'
#' @param pkgs vector of project packages which should be included in PKGZIP
#'   or NULL to include all project packages (type: character, default: NULL)
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param zip_ver if passed enforce the version of PKGZIP package to the passed value.
#'    Expected form of version is DD.DD. (type: character, default: NULL)
#' @param pkg_type type of packages to build (type: character, default: platform default)
#' @param path folder path to put output zip into. The folder must exist.
#'    (type: character: default:  \code{getwd()})
#' @param with_deps If TRUE will include dependencies pkgs dependencies into final zip.
#'    (type: logical, default: FALSE)
#' @param filter_repo repository address to not include dependencies available in.
#'     In a project, dependencies will never be filtered. If NULL will not filter
#'     dependencies. Will be omitted if with_deps is FALSE. (type: character(1), default: NULL)
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
#' @return created pkgzip file path (invisible).
#'
#' @family in PKGZIP building
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
#'   # start package in my_project
#'   prj_start_package("mypackage", skip_rc = TRUE, prj = prj)
#'
#'   # build project environment and install supportives
#'   prj_install_deps(prj = prj, vanilla_sups = TRUE)
#'
#'   # build PKGZIP
#'   pkgzip_fpath <- pkgzip_build_prj_packages(prj = prj, path = tempdir())
#'
#'   # list content of pkgzip created
#'   unzip(pkgzip_fpath, list = TRUE)
#' }
#'
#' @export
#'
pkgzip_build_prj_packages <- function(pkgs = NULL,
                                      prj = NULL,
                                      zip_ver = NULL,
                                      pkg_type = .Platform$pkgType,
                                      path = getwd(),
                                      with_deps = FALSE,
                                      filter_repo = NULL,
                                      skip_build_steps = NULL) {
  assert(dir.exists(path), "Existing folder expected for path")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")
  assert(is.logical(with_deps), "Logical value expected for with_deps")
  if (!any(with_deps) && !is.null(filter_repo)) {
    pkg_logwarn("filter_repo can be used only then building pkgzip with dependencies. Rejecting its value.")
    filter_repo <- NULL
  }
  if (!is.null(filter_repo)) {
    assert(is_nonempty_char1(filter_repo), "Non empty character(1) expected for filter_repo")
  }
  if (!is.null(skip_build_steps)) {
    assert(is.character(skip_build_steps)
           && all(skip_build_steps %in% c("spec", "docs", "imps", "tests", "rcpp_attribs", "vignettes")),
           paste("character(N) expected for skip_build_steps containing entities",
                 "spec, docs, imps, tests, rcpp_attribs or vignettes"))
  }

  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()
  ver_inf <- detect_zip_version(params, zip_ver) # from 15_zip_project.R

  prj_pkgs <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  assert(length(prj_pkgs) > 0, "The project does not have packages to include in PKGZIP")

  if (is.null(pkgs)) {
    pkgs <- prj_pkgs
  } else {
    assert(all(pkgs %in% prj_pkgs),
           "Packages requested to include into PKGZIP are not present in project: %s",
           paste(setdiff(pkgs, prj_pkgs), collapse = ", "))
    pkgs <- prj_pkgs[prj_pkgs %in% pkgs]
  }
  if (any(with_deps)) {
    raw_vers <- lapply(X = names(pkgs),
                       FUN = function(pkg_dir) {
                         collect_single_pkg_direct_deps(params, pkg_dir, pkgs[[pkg_dir]])
                       })
    dep_vers <- do.call("vers.union", raw_vers)
    inproj_deps <- intersect(prj_pkgs, vers.get_names(dep_vers))
    avail_vers <- collect_dependencies(vers.rm(dep_vers, inproj_deps), # from 18_repo_helpers.R
                                       pkg_type, params, filter_repo)


    zip_pref <- sprintf("%s_v%s", paste(pkgs, collapse = "_"), ver_inf$ver)
    pkgs <- c(pkgs, inproj_deps) # include also in-project dependencies into PKGZIP
    zip_file_name <- .pkgzip_file_name(c(vers.get_names(avail_vers), inproj_deps),
                                       pref = zip_pref)
  } else {
    avail_vers <- vers.build(avails = data.frame())
    zip_file_name <- .pkgzip_file_name(pkgs, suff = paste0("v", ver_inf$ver))
  }

  pkg_loginfo("Building project packages ...")
  build_install_tagged_prj_packages(params, # from 12_build_install_prj_pacakges.R
                                    ver_inf$rev,
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

  pkg_loginfo("... done. Creating PKGZIP file %s ...", zip_file_name)

  zip_file_path <- file.path(rsuite_fullUnifiedPath(path), zip_file_name)
  success <- zip_folder(tmp_path, zip_file_path)
  assert(success, "Failed to create zip file (zip returned non 0 return status).")

  pkg_loginfo("Zip file created: %s", zip_file_path)
  return(invisible(zip_file_path))
}

#'
#' Builds PKGZIP out of passed package files.
#'
#' @details
#' Logs all messages onto rsuite logger. Use  \code{logging::setLevel} to control logs
#' verbosity.
#'
#' @param files vector of files to upload. (type: character)
#' @param path folder path to put output zip into. The folder must exist.
#'    (type: character: default:  \code{getwd()})
#'
#' @return created pkgzip file path (invisible).
#'
#' @family in PKGZIP building
#'
#' @examples
#' \donttest{
#'   # download logging package
#'   pkg_fpath <- utils::download.packages("logging",
#'                                         repos = "https://cloud.r-project.org/",
#'                                         destdir = tempdir())[1,2]
#'
#'   # build PKGZIP
#'   pkgzip_fpath <- pkgzip_build_package_files(files = pkg_fpath, path = tempdir())
#'
#'   # list content of pkgzip created
#'   unzip(pkgzip_fpath, list = TRUE)
#' }
#'
#' @export
#'
pkgzip_build_package_files <- function(files, path = getwd()) {
  assert(dir.exists(path), "Existing folder expected for path")

  assert(!missing(files) && is.character(files) && length(files) > 0,
         "Non empty character(N) expected for files")
  nonexistent <- files[!file.exists(files)]
  assert(!length(nonexistent),
         "Some files requested to upload do not exist: %s",
         paste(nonexistent, collapse = ", "))

  pkg_infos <- get_package_files_info(files) # from 54_pkg_info.R
  pkg_infos[is.na(pkg_infos$RVersion)]$RVersion <- current_rver()
  pkg_infos$RVersion <- majmin_rver(pkg_infos$RVersion)

  all_names <- unique(pkg_infos$Package)
  all_names <- all_names[order(all_names)]
  zip_file_name <- sprintf("%s_pkgzip_%s.zip", Sys.Date(), paste(all_names, collapse = "_"))

  tmp_path <- tempfile("pkgzip_temp_repo")
  tryCatch({
    rvers <- unique(pkg_infos$RVersion)
    for (rver in rvers) {
      rver_infos <- pkg_infos[pkg_infos$RVersion == rver, ]
      rver_types <- unique(rver_infos$Type)

      pkg_loginfo("Preparing temp repository for %s types (R %s) ...",
                  paste(rver_types, collapse = ", "), rver)
      tmp_mgr <- repo_manager_dir_create(tmp_path, rver_types, rver)
      repo_manager_init(tmp_mgr)

      for (tp in rver_types) {
        dest_path <- rsuite_contrib_url(tmp_path, tp, rver)

        rver_tp_files <- pkg_infos[pkg_infos$Type == tp & pkg_infos$RVersion == rver, ]$File
        success <- file.copy(from = rver_tp_files, to = dest_path)
        assert(all(success),
               "Failed to copy to temporary repository: %s",
               paste(rver_tp_files[!success], collapse = ", "))

        rsuite_write_PACKAGES(dest_path, tp)
      }
    }
    pkg_loginfo("... done. Creating PKGZIP file %s ...", zip_file_name)

    zip_file_path <- file.path(rsuite_fullUnifiedPath(path), zip_file_name)
    success <- zip_folder(tmp_path, zip_file_path)
    assert(success, "Failed to create zip file (zip returned non 0 return status).")

    pkg_loginfo("Zip file created: %s", zip_file_path)
    return(invisible(zip_file_path))
  },
  finally = {
    unlink(tmp_path, recursive = TRUE, force = TRUE)
  })
}


#'
#' Builds PKGZIP out of passed external packages.
#'
#' @details
#' It uses the project to detect repositories to look for packages in.
#'
#' Logs all messages onto rsuite logger. Use  \code{logging::setLevel} to control logs
#' verbosity.
#'
#' @param pkgs vector of names of external packages which should be included in
#'   PKGZIP. (type: character)
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param pkg_type type of packages to build (type: character, default: platform default)
#' @param path folder path to put output zip into. The folder must exist.
#'    (type: character(1), default: getwd())
#' @param with_deps If TRUE will include dependencies pkgs dependencies into final zip.
#'    (type: logical, default: FALSE)
#' @param filter_repo repository address to not include dependencies available in.
#'     If NULL will not filter dependencies. Will be omitted if with_deps is FALSE.
#'     (type: character(1), default: NULL)
#'
#' @return created pkgzip file path (invisible).
#'
#' @family in PKGZIP building
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
#'   # build PKGZIP with logging package
#'   pkgzip_fpath <- pkgzip_build_ext_packages("logging", prj = prj, path = tempdir())
#'
#'   # list content of pkgzip created
#'   unzip(pkgzip_fpath, list = TRUE)
#' }
#'
#' @export
#'
pkgzip_build_ext_packages <- function(pkgs,
                                      prj = NULL,
                                      pkg_type = .Platform$pkgType,
                                      path = getwd(),
                                      with_deps = FALSE,
                                      filter_repo = NULL) {
  assert(dir.exists(path), "Existing folder expected for path")
  assert(!missing(pkgs) && is.character(pkgs) && length(pkgs) > 0,
         "Non empty character(N) expected for pkgs")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")
  assert(is.logical(with_deps), "Logical value expected for with_deps")
  if (!any(with_deps) && !is.null(filter_repo)) {
    pkg_logwarn("filter_repo can be used only then building pkgzip with dependencies. Rejecting its value.")
    filter_repo <- NULL
  }
  if (!is.null(filter_repo)) {
    assert(is_nonempty_char1(filter_repo), "Non empty character(1) expected for filter_repo")
  }

  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()

  if (any(with_deps)) {
    avail_vers <- collect_dependencies(vers.build(pkgs), pkg_type, params, filter_repo) # from 18_repo_helpers.R
  } else {
    avail_vers <- collect_packages(vers.build(pkgs), pkg_type, params) # from 18_repo_helpers.R
  }

  all_names <- vers.get_names(avail_vers)
  all_names <- all_names[order(all_names)]
  if (length(all_names) > 5) {
    zip_file_name <- sprintf("%s_pkgzip_%s_and_%sothers.zip",
                             Sys.Date(), paste(all_names[1:5], collapse = "_"), length(all_names) - 5)
  } else {
    zip_file_name <- sprintf("%s_pkgzip_%s.zip", Sys.Date(), paste(all_names, collapse = "_"))
  }

  tmp_path <- tempfile("pkgzip_temp_repo")
  on.exit({
    unlink(tmp_path, recursive = TRUE, force = TRUE)
  },
  add = TRUE)

  temp_repo_prepare(avail_vers, tmp_path, pkg_type, params) # from 18_repo_helpers.R
  temp_repo_write_PACKAGES(tmp_path, pkg_type, rver = params$r_ver) # from 18_repo_helpers.R

  pkg_loginfo("... done. Creating PKGZIP file %s ...", zip_file_name)

  zip_file_path <- file.path(rsuite_fullUnifiedPath(path), zip_file_name)
  success <- zip_folder(tmp_path, zip_file_path)
  assert(success, "Failed to create zip file (zip returned non 0 return status).")

  pkg_loginfo("Zip file created: %s", zip_file_path)
  return(invisible(zip_file_path))
}


#'
#' Builds PKGZIP out of a package on GitHub.
#'
#' Loads package from the GitHub repository, packages it into package file and builds
#' a PKGZIP out of it. It uses the project to detect repositories to look for dependencies
#' and to detect rversion if required.
#'
#' @details
#' Logs all messages onto rsuite logger. Use \code{logging::setLevel} to control logs
#' verbosity.
#'
#' @param repo repository address in format username/repo[/subdir][\@ref|#pull]. See
#'   devtools::install_github for more information.
#' @param ... GitHub specific parameters passed to  \code{devtools::install_github}.
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param pkg_type type of packages to build (type: character, default: platform default)
#' @param path folder path to put output zip into. The folder must exist.
#'    (type: character: default: \code{getwd()})
#' @param with_deps If TRUE will include dependencies pkgs dependencies into final zip.
#'    (type: logical, default: FALSE)
#' @param filter_repo repository address to not include dependencies available in.
#'     If NULL will not filter dependencies. Will be omitted if with_deps is FALSE.
#'     (type: character(1), default: NULL)
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
#' @return created pkgzip file path (invisible).
#'
#' @family in PKGZIP building
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
#'   # build PKGZIP with logging package from cran repository
#'   pkgzip_fpath <- pkgzip_build_github_package("cran/logging", prj = prj, path = tempdir())
#'
#'   # list content of pkgzip created
#'   unzip(pkgzip_fpath, list = TRUE)
#' }
#'
#' @export
#'
pkgzip_build_github_package <- function(repo, ...,
                                        prj = NULL,
                                        pkg_type = .Platform$pkgType,
                                        path = getwd(),
                                        with_deps = FALSE,
                                        filter_repo = NULL,
                                        skip_build_steps = NULL,
                                        keep_sources = FALSE) {
  assert(is_nonempty_char1(repo), "Non empty character(1) expected for repo")
  assert(dir.exists(path), "Existing folder expected for path")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")
  assert(is.logical(with_deps), "Logical value expected for with_deps")
  if (!any(with_deps) && !is.null(filter_repo)) {
    pkg_logwarn("filter_repo can be used only then building pkgzip with dependencies. Rejecting its value.")
    filter_repo <- NULL
  }
  if (!is.null(filter_repo)) {
    assert(is_nonempty_char1(filter_repo), "Non empty character(1) expected for filter_repo")
  }
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

  prj_config_set_rversion(rver = params$r_ver, prj = bld_prj)
  prj_config_set_repo_adapters(make_detached_repos(params), prj = bld_prj)

  pkg_info <- get_srcrepo_package(bld_prj, "github", repo, ...)

  bld_params <- bld_prj$load_params()

  # not to include default packages
  unlink(list.files(bld_params$script_path, pattern = ".+[.]R$", full.names = TRUE), force = TRUE)
  prj_install_deps(bld_prj, vanilla_sups = TRUE)

  pkg_ver <- read.dcf(file.path(bld_params$pkgs_path, pkg_info$dir, "DESCRIPTION"))[1, "Version"]

  pkgzip_build_prj_packages(pkgs = pkg_info$name, prj = bld_prj,
                            zip_ver = pkg_ver, pkg_type = pkg_type, path = path,
                            with_deps = with_deps, filter_repo = filter_repo,
                            skip_build_steps = skip_build_steps)
}
