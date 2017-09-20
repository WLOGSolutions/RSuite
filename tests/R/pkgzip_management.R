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
    path = path
  ))
}

expect_that_pkgzip_contains <- function(names, type, pkgzip) {
  files <- list.files(pkgzip$path, pattern = sprintf("%s_pkgzip_.*[.]zip", Sys.Date()), full.names = T)
  expect_length(files, 1)

  cont_path <- file.path(pkgzip$path, "cont")
  expect_success({
    unzip(zipfile = files, exdir = file.path(pkgzip$path, "cont"))
    succeed()
  })

  c_url <- contrib.url(sprintf("file:///%s", cont_path), type)
  avail <- data.frame(available.packages(contriburl = c_url, filters = c()),
                      stringsAsFactors = F)$Package

  avail <- avail[ordered(avail)]
  names <- names[ordered(names)]

  expect_equal(object = avail, expected = names)
}
