#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for repository management during testing.
#----------------------------------------------------------------------------

init_test_manager <- function(prj, ra_name = "Dir") {
  params <- prj$load_params()
  repo_path <- tempfile(tmpdir = params$prj_path, pattern = "repo_")


  types <- c(params$pkgs_type, params$aux_pkgs_type)
  repo_mgr <- RSuite::repo_mng_start(ra_name = ra_name,
                                     path = repo_path, rver = params$r_ver,
                                     types = types)
  RSuite::repo_mng_init(repo_mgr)

  on_test_exit(function() {
    RSuite::repo_mng_stop(repo_mgr)
    unlink(repo_path, recursive = T, force = T)
  })

  return(list(repo_mgr = repo_mgr, path = repo_path, url = sprintf("file:///%s", repo_path)))
}

init_test_dir_adapter <- function(name) {
  repo_adapter <- RSuite:::repo_adapter_create_dir(name)
  RSuite::rsuite_register_repo_adapter(repo_adapter)

  on_test_exit(function() {
    RSuite::rsuite_unregister_repo_adapter(name)
  })

  return(repo_adapter)
}

create_package_deploy_to_repo <- function(name,
                                           prj,
                                           repo_manager = repo_manager,
                                           ver = "1.0",
                                           type = .Platform$pkgType,
                                           deps = "",
                                           imps = "logging") {
  pkg_path <- create_test_package(name, prj, ver, deps = deps, imps = imps)
  set_test_package_ns_imports(name, prj, unlist(strsplit(imps, ",")))

  params <- prj$load_params()
  on.exit({
    unlink(pkg_path, recursive = T, force = T)
    unlink(file.path(params$lib_path, "*"), recursive = T, force = T)
  }, add = T)
  loc_repo <- .get_local_repo_path(prj, type)

  prj_install_deps(prj, clean = T)
  prj_build(prj, type = type)

  repo_upload_prj_packages(repo_manager,
                           pkgs = name,
                           prj = prj,
                           skip_rc = TRUE)
}

get_intrepo_manager <- function(prj) {
  params <- prj$load_params()
  repo_path <- params$get_intern_repo_path()

  types <- c(params$pkgs_type, params$aux_pkgs_type)
  repo_mgr <- RSuite::repo_mng_start(ra_name = "Dir",
                                     path = repo_path, rver = params$r_ver,
                                     types = types)

  return(list(repo_mgr = repo_mgr, path = repo_path, url = sprintf("file:///%s", repo_path)))
}

expect_that_packages_available <- function(names, type, mgr, optional_names = c()) {
  repo_url <- sprintf("file:///%s", mgr$path)
  avails <- data.frame(available.packages(contriburl = RSuite:::rsuite_contrib_url(repo_url, type), filters = c()),
                       stringsAsFactors = F)$Package

  not_avail <- setdiff(names, avails)
  expect(length(not_avail) == 0,
         sprintf("Expected packages are not available in repo: %s",
                 paste(not_avail, collapse = ", ")))

  unexpect <- setdiff(avails, names)
  unexpect <- setdiff(unexpect, optional_names)
  expect(length(unexpect) == 0,
         sprintf("Unexpected packages found in rep: %s",
                 paste(unexpect, collapse = ", ")))
}
