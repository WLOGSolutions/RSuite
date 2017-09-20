#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Package API related to PKGZIP building.
#----------------------------------------------------------------------------

#'
#' Builds PKGZIP out of project packages. PKGZIP will be tagged with the same
#'   way az project zip.
#'
#' Logs all messages onto rsuite logger. Use logging::setLevel to control logs
#' verbosity.
#'
#' @param pkgs vector of project packages which should be included in PKGZIP
#'   or NULL to include all project packages (type: characted, default: NULL)
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param zip_ver if passed enforce version of PKGZIP package to passed value.
#'    Expected form of version is DD.DD. (type: character, default: NULL)
#' @param pkg_type type of packages to build (type: character, default: platform default)
#' @param path folder path to put output zip into. The folder must exist.
#'    (type: characted: default: getwd())
#' @param inc_deps flag desiding if dependencies should be included (type: logical: default: FALSE)
#'
#' @return created pkgzip file path (invisible).
#'
#' @export
#'
pkgzip_build_prj_packages <- function(pkgs = NULL,
                                      prj = NULL,
                                      zip_ver = NULL,
                                      pkg_type = .Platform$pkgType,
                                      path = getwd() #, inc_deps = FALSE
                                      ) {
  assert(dir.exists(path), "Existing folder expected for path")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")
#  assert(!missing(inc_deps) && length(inc_deps) == 1 && is.logical(inc_deps),
#         "logical(1) expected for inc_deps")
  
  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()

  prj_pkgs <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  if (!is.null(pkgs)) {
    all_names <- paste(pkgs, collapse = "_")
  } else {
    assert(length(prj_pkgs) > 0, "The project does not have packages to include in PKGZIP")
    all_names <- paste(prj_pkgs, collapse = "_")
  }

  pkg_loginfo("Building project packages ...")
  ver_inf <- detect_zip_version(params, zip_ver) # from 15_zip_project.R
  build_install_tagged_prj_packages(params, # from 12_build_install_prj_pacakges.R
                                    ver_inf$rev,
                                    build_type = pkg_type,
                                    pkgs = pkgs)

#  if (inc_deps) {
#    pkg_loginfo("... done. Downloading dependencies ...")
#    
#    repo_infos <- get_all_repo_infos(params) # from 53_repositories.R
#    log_repo_infos(repo_infos) # from 53_repositories.R
#    typedVers <- resolve_prj_deps(repo_infos, params,  # from 11_install_prj_deps.R
#                                  only_source = (pkg_type == "source"))
#  }
  
  zip_file_name <- sprintf("%s_pkgzip_%s_%s.zip", Sys.Date(), all_names, ver_inf$ver)
  pkg_loginfo("... done. Creating PKGZIP file %s ...", zip_file_name)

  zip_file_path <- file.path(rsuite_fullUnifiedPath(path), zip_file_name)
  success <- zip_folder(params$irepo_path, zip_file_path)
  assert(success, "Failed to create zip file (zip returned non 0 return status).")

  pkg_loginfo("Zip file created: %s", zip_file_path)
  return(invisible(zip_file_path))
}

#'
#' Builds PKGZIP out of passed package files.
#'
#' Logs all messages onto rsuite logger. Use logging::setLevel to control logs
#' verbosity.
#'
#' @param files vector of files to upload. (type: characted)
#' @param path folder path to put output zip into. The folder must exist.
#'    (type: characted: default: getwd())
#'
#' @return created pkgzip file path (invisible).
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
    for(rver in rvers) {
      rver_types <- unique(pkg_infos[pkg_infos$RVersion == rver, ]$Type)

      pkg_loginfo("Preparing temp repository for %s types (R %s) ...",
                  paste(rver_types, collapse = ", "), rver)
      tmp_mgr <- repo_manager_dir_create(tmp_path, rver_types, rver)
      repo_manager_init(tmp_mgr)

      for(tp in rver_types) {
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
  }, finally = {
    unlink(tmp_path, recursive = T, force = T)
  })
}


#'
#' Builds PKGZIP out of passed external packages. It uses project to detect
#' repositories to look for packages in.
#'
#' Logs all messages onto rsuite logger. Use logging::setLevel to control logs
#' verbosity.
#'
#' @param pkgs vector of names of external packages which should be included in
#'   PKGZIP. (type: characted)
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param pkg_type type of packages to build (type: character, default: platform default)
#' @param path folder path to put output zip into. The folder must exist.
#'    (type: characted: default: getwd())
#'
#' @return created pkgzip file path (invisible).
#'
#' @export
#'
pkgzip_build_ext_packages <- function(pkgs,
                                      prj = NULL,
                                      pkg_type = .Platform$pkgType,
                                      path = getwd()) {
  assert(dir.exists(path), "Existing folder expected for path")
  assert(!missing(pkgs) && is.character(pkgs) && length(pkgs) > 0,
         "Non empty character(N) expected for pkgs")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")
  
  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()

  pkg_loginfo("Detecting repositories (for R %s)...", params$r_ver)
  
  repo_infos <- get_all_repo_infos(params) # from 53_repositories.R
  log_repo_infos(repo_infos) # from 53_repositories.R

  contrib_urls <- retrieve_contrib_urls(repo_infos, pkg_type) # from 53_repositories.R

  # retrieves latest versions
  avail_pkgs <- versions.get_available_pkgs(pkgs, contrib_urls)

  unknown_pkgs <- setdiff(pkgs, avail_pkgs$Package)
  if (pkg_type != "source") { # try source packages to build
    src_contrib_urls <- retrieve_contrib_urls(repo_infos, "source") # from 53_repositories.R
    src_avail_pkgs <- versions.get_available_pkgs(unknown_pkgs, src_contrib_urls)
    unknown_pkgs <- setdiff(unknown_pkgs, src_avail_pkgs$Package)
  } else {
    src_avail_pkgs <- data.frame(Package = as.character())
  }
  assert(!length(unknown_pkgs),
         "Some packages are not available for %s type: %s",
         pkg_type, paste(unknown_pkgs, collapse = ", "))

  all_names <- c(avail_pkgs$Package, src_avail_pkgs$Package)
  all_names <- all_names[order(all_names)]
  if (length(all_names) > 5) {
    zip_file_name <- sprintf("%s_pkgzip_%s_and_%sothers.zip", 
                             Sys.Date(), paste(all_names[1:5], collapse = "_"), length(all_names) - 5)
  } else {
    zip_file_name <- sprintf("%s_pkgzip_%s.zip", Sys.Date(), paste(all_names, collapse = "_"))
  }

  tmp_path <- tempfile("pkgzip_temp_repo")
  on.exit({ unlink(tmp_path, recursive = T, force = T) }, add = TRUE)
  
  pkg_loginfo("Preparing temp repository for packages ...")

  tmp_mgr <- repo_manager_dir_create(tmp_path, pkg_type, params$r_ver)
  repo_manager_init(tmp_mgr)

  pkg_download(avail_pkgs,
               dest_dir = rsuite_contrib_url(tmp_path, pkg_type, params$r_ver))
  if (nrow(src_avail_pkgs) > 0) {
    build_source_packages(src_avail_pkgs, # from 17_build_src_packages.R
                          tmp_path, pkg_type, params, mgr_info$rver)
  }
  
  dest_path <- rsuite_contrib_url(tmp_path, pkg_type, params$r_ver)
  rsuite_write_PACKAGES(dest_path, pkg_type)

  pkg_loginfo("... done. Creating PKGZIP file %s ...", zip_file_name)

  zip_file_path <- file.path(rsuite_fullUnifiedPath(path), zip_file_name)
  success <- zip_folder(tmp_path, zip_file_path)
  assert(success, "Failed to create zip file (zip returned non 0 return status).")

  pkg_loginfo("Zip file created: %s", zip_file_path)
  return(invisible(zip_file_path))
}


#'
#' Loads package from github repository, packages it into package file and builds
#' PKGZIP out of it. It uses project to detect repositories to look for dependencies 
#' and to detect rversion if required.
#' 
#' Logs all messages onto rsuite logger. Use logging::setLevel to control logs
#' verbosity.
#'
#' @param repo repository address in format username/repo[/subdir][\@ref|#pull]. See
#'   devtools::install_github for more information.
#' @param ... github specific parametrs passed to devtools::install_github.
#' @param prj project object to use. If not passed will init project from
#'   working directory. (type: rsuite_project, default: NULL)
#' @param pkg_type type of packages to build (type: character, default: platform default)
#' @param path folder path to put output zip into. The folder must exist.
#'    (type: characted: default: getwd())
#'
#' @return created pkgzip file path (invisible).
#'
#' @export
#'
pkgzip_build_github_package <- function(repo, ..., 
                                        prj = NULL,
                                        pkg_type = .Platform$pkgType,
                                        path = getwd()) {
  assert(is_nonempty_char1(repo), "Non empty character(1) expected for repo")
  assert(dir.exists(path), "Existing folder expected for path")
  assert(is_nonempty_char1(pkg_type), "Non empty character(1) expected for pkg_type")
  
  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))
  
  params <- prj$load_params()

  bld_prj <- prj_start(name = basename(tempfile(pattern = "srcrepo_proj_")), 
                       path = tempdir(),
                       skip_rc = T)
  on.exit({ unlink(bld_prj$path, recursive = T, force = T) }, add = TRUE)
  
  prj_config_set_rversion(rver = params$r_ver, prj = bld_prj)
  prj_config_set_repo_adapters(make_detached_repos(params), prj = bld_prj)
  
  pkg_name <- get_srcrepo_package(bld_prj, "github", repo, ...)
  
  bld_params <- bld_prj$load_params()
  unlink(list.files(bld_params$script_path, pattern = ".+[.]R$", full.names = T),  force = T) # not to include default packages
  prj_install_deps(bld_prj)
  
  pkg_ver <- read.dcf(file.path(bld_params$pkgs_path, pkg_name, "DESCRIPTION"))[1, 'Version']
  
  pkgzip_build_prj_packages(pkgs = pkg_name, prj = bld_prj, 
                            zip_ver = pkg_ver, pkg_type = pkg_type, path = path)
}