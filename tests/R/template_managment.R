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


expect_templates <- function(expected_data) {
  template_data <- RSuite::rsuite_get_templates()
  template_data <- template_data[, c("Name", "Type")]
  result <- do.call(paste0, expected_data) %in% do.call(paste0, template_data)
  pass <- all(result)
  msg <- ifelse(pass, "", sprintf("%s templates were not found", expected_data[!result, ]))
  
  expect(pass, msg)
}