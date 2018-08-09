#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for template managment during testing
#----------------------------------------------------------------------------

create_prj_test_template <- function(name = NULL, path = NULL) {
  stopifnot(!is.null(name))

  RSuite::tmpl_start_prj(name, path)

  on_test_exit(function() {
    if (!is.null(path)) {
      unlink(path, recursive = T, force = T)
    }
  })
}


create_pkg_test_template <- function(name = NULL, path = NULL) {
  stopifnot(!is.null(name))

  RSuite::tmpl_start_pkg(name, path)

  on_test_exit(function() {
    if (!is.null(path)) {
      unlink(path, recursive = T, force = T)
    }
  })
}


expect_templates <- function(expected_data) {
  template_data <- RSuite::tmpl_list_registered()
  template_data <- template_data[, c("Name", "HasProjectTemplate", "HasPackageTemplate")]
  result <- do.call(paste0, expected_data) %in% do.call(paste0, template_data)
  pass <- all(result)
  msg <- ifelse(pass, "", sprintf("%s templates were not found", expected_data[!result, ]))

  expect(pass, msg)
}