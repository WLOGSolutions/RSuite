#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for pkgzip management during testing.
#----------------------------------------------------------------------------

init_test_pkgzip <- function() {
  path <- tempfile(tmpdir = get_wspace_dir(), pattern = "pkgzip_")
  dir.create(path, recursive = T)

  path <- normalizePath(path)
  on_test_exit(function() {
    unlink(path, recursive = T, force = T)
  })

  return(list(
    path = path,
    get_pkgzip_fpath = function() {
      files <- list.files(path, pattern = sprintf("%s_pkgzip_.*[.]zip", Sys.Date()), full.names = T)
      expect_length(files, 1)
      return(files)
    }
  ))
}

expect_that_pkgzip_contains <- function(names, type, pkgzip, optional_names = c()) {
  files <- pkgzip$get_pkgzip_fpath()
  cont_path <- file.path(pkgzip$path, "cont")
  expect_success({
    unzip(zipfile = files, exdir = file.path(pkgzip$path, "cont"))
    succeed()
  })

  c_url <- RSuite:::rsuite_contrib_url(repos = sprintf("file:///%s", cont_path), type = type)
  avail <- data.frame(available.packages(contriburl = c_url, filters = c()),
                      stringsAsFactors = F)$Package

  avail <- avail[order(avail)]
  names <- c(names, intersect(optional_names, avail))

  names <- names[order(names)]

  expect_equal(object = avail, expected = names)
}
