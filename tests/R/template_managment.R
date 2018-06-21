#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for template managment during testing
#----------------------------------------------------------------------------

create_prj_test_template <- function(name = NULL, path = NA) {
  assert(!is.null(name), "Template name was not provided")
  
  RSuite::rsuite_start_prj_template(name = name, path = path)

  on_test_exit(function() {
    if (!is.na(path)) {
      unlink(path, recursive = T, force = T)
    }
  })
}

create_pkg_test_template <- function(name = NULL, path = NA) {
  assert(!is.null(name), "Template name was not provided")

  RSuite::rsuite_start_pkg_template(name = name, path = path)

  on_test_exit(function() {
    if (!is.na(path)) {
      unlink(path, recursive = T, force = T)
    }
  })
}