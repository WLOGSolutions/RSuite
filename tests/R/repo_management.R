#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for repository management during testing.
#----------------------------------------------------------------------------

init_test_manager <- function(prj) {
  repo_path <- tempfile(tmpdir = get_wspace_dir(), pattern = "repo_")

  params <- prj$load_params()

  types <- c(params$pkgs_type, params$aux_pkgs_type)
  repo_mgr <- RSuite::repo_mng_start(ra_name = "Dir",
                                     path = repo_path, rver = params$r_ver,
                                     types = types)
  RSuite::repo_mng_init(repo_mgr)

  on_test_exit(function() {
    RSuite::repo_mng_stop(repo_mgr)
    unlink(repo_path, recursive = T, force = T)
  })

  return(list(repo_mgr = repo_mgr, path = repo_path, url = sprintf("file:///%s", repo_path)))
}

expect_that_packages_available <- function(names, type, mgr) {
  repo_url <- sprintf("file:///%s", mgr$path)
  avails <- data.frame(available.packages(contriburl = RSuite:::rsuite_contrib_url(repo_url, type), filters = c()),
                       stringsAsFactors = F)$Package

  not_avail <- setdiff(names, avails)
  expect(length(not_avail) == 0,
         sprintf("Expected packages are not available in repo: %s",
                 paste(not_avail, collapse = ", ")))

  unexpect <- setdiff(avails, names)
  expect(length(unexpect) == 0,
         sprintf("Unexpected packages found in rep: %s",
                 paste(unexpect, collapse = ", ")))
}
