#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for template managment during testing
#----------------------------------------------------------------------------

create_prj_test_template <- function(name = NULL) {
  assert(!is.null(name), "Template name was not provided")

  tmpl_dir <- get_wspace_template_dir()
  RSuite::rsuite_start_prj_template(name = name, path = tmpl_dir)

  on_test_exit(function() {
    unlink(tmpl_dir, recursive = T, force = T)
  })
}

create_pkg_test_template <- function(name = NULL) {
  assert(!is.null(name), "Template name was not provided")

  tmpl_dir <- get_wspace_template_dir()
  RSuite::rsuite_start_pkg_template(name = name, path = tmpl_dir)

  on_test_exit(function() {
    unlink(tmpl_dir, recursive = T, force = T)
  })
}