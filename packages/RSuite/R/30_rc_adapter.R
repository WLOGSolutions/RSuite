#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# RC adapter class API.
#----------------------------------------------------------------------------

#'
#' Creates the base presentation for the RC adapter to use by concrete implementations.
#'
#' @param name name under which RC adapter will be registered in RSuite. It
#'   cannot contain whitespaces or comma. (type: character)
#'
#' @return object of type rsuite_rc_adapter
#'
#' @family in extending RSuite with RC adapter
#'
#' @examples
#' # create you own RC adapter
#' rc_adapter_create_own <- function() {
#'   result <- rc_adapter_create_base("Own")
#'   class(result) <- c("rc_adapter_own", class(result))
#'   return(result)
#' }
#'
#' @export
#'
rc_adapter_create_base <- function(name) {
  assert(is_nonempty_char1(name), "Non empty character(1) expected for name")
  assert(!grepl("[\\s,]", name, perl = TRUE),
         "Adapter name cannot contain whitespaces or comma.")

  result <- list(name = name)
  class(result) <- "rsuite_rc_adapter"
  return(result)
}

#'
#' Detects if dir is under adapter's managed version control.
#'
#' @param rc_adapter rc adapter object
#' @param dir path to the directory to check. The folder must exist (type: character)
#'
#' @return TRUE if dir is under version control.
#'
#' @family in extending RSuite with RC adapter
#'
#' @examples
#' # create you own RC adapter
#' rc_adapter_create_own <- function() {
#'   result <- rc_adapter_create_base("Own")
#'   class(result) <- c("rc_adapter_own", class(result))
#'   return(result)
#' }
#'
#' #' @export
#' rc_adapter_is_under_control.rc_adapter_own <- function(rc_adapter, dir) {
#'   # ... check ...
#'   return(TRUE)
#' }
#'
#' @export
#'
rc_adapter_is_under_control <- function(rc_adapter, dir) {
  assert(is_rc_adapter(rc_adapter), "rsuite_rc_adapter object expected for rc_adapter")
  assert(is_nonempty_char1(dir), "Non empty character(1) expected for dir")
  assert(dir.exists(dir), "Existing folder expected for dir")

  UseMethod("rc_adapter_is_under_control")
}

#'
#' Default implemenation of rc_adapter_is_under_control
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_is_under_control.default <- function(rc_adapter, dir) {
  assert(FALSE,
         "rc_adapter_is_under_control not implemented by %s",
         paste(class(rc_adapter), collapse = " "))
}

#'
#' Puts project structure under RC adapter's managed version control.
#'
#' @param rc_adapter rc adapter object
#' @param params rsuite_project_params object of the project.
#'
#' @family in extending RSuite with RC adapter
#'
#' @examples
#' # create you own RC adapter
#' rc_adapter_create_own <- function() {
#'   result <- rc_adapter_create_base("Own")
#'   class(result) <- c("rc_adapter_own", class(result))
#'   return(result)
#' }
#'
#' #' @export
#' rc_adapter_prj_struct_add.rc_adapter_own <- function(rc_adapter, params) {
#'   # ... add project specified by params under RC ...
#' }
#'
#' @export
#'
rc_adapter_prj_struct_add <- function(rc_adapter, params) {
  assert(is_rc_adapter(rc_adapter), "rsuite_rc_adapter object expected for rc_adapter")
  assert(class(params) == "rsuite_project_params", "rsuite_project_params object expected for params")

  UseMethod("rc_adapter_prj_struct_add")
}

#'
#' Default implemenation of rc_adapter_prj_struct_add
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_prj_struct_add.default <- function(rc_adapter, params) {
  assert(FALSE,
         "rc_adapter_prj_struct_add not implemented by %s",
         paste(class(rc_adapter), collapse = " "))
}


#'
#' Puts the package structure under RC adapter's managed version control.
#'
#' @param rc_adapter rc adapter object
#' @param params rsuite_project_params object of the project.
#' @param name name of the package to put under RC adapter's managed version control.
#'   Appropriate sub-folder must exist in project packages folder. (type: character)
#'
#' @family in extending RSuite with RC adapter
#'
#' @examples
#' # create you own RC adapter
#' rc_adapter_create_own <- function() {
#'   result <- rc_adapter_create_base("Own")
#'   class(result) <- c("rc_adapter_own", class(result))
#'   return(result)
#' }
#'
#' #' @export
#' rc_adapter_pkg_struct_add.rc_adapter_own <- function(rc_adapter, params, name) {
#'   # ... add package specified by name under RC in project specified by params ...
#' }
#'
#' @export
#'
rc_adapter_pkg_struct_add <- function(rc_adapter, params, name) {
  assert(is_rc_adapter(rc_adapter), "rsuite_rc_adapter object expected for rc_adapter")
  assert(class(params) == "rsuite_project_params", "rsuite_project_params object expected for params")
  assert(is_nonempty_char1(name), "Non empty character(1) expected for name")
  assert(file.exists(file.path(params$pkgs_path, name, "DESCRIPTION")),
         "Package name expected for name: it should be folder with DESCRIPTION file inside")

  UseMethod("rc_adapter_pkg_struct_add")
}

#'
#' Default implemenation of rc_adapter_pkg_struct_add
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_pkg_struct_add.default <- function(rc_adapter, params, name) {
  assert(FALSE,
         "rc_adapter_pkg_struct_add not implemented by %s",
         paste(class(rc_adapter), collapse = " "))
}

#'
#' Retrieves current RC version number for working copy at directory passed.
#'
#' @param rc_adapter rc adapter object
#' @param dir path to the directory to get the version for. The folder must exist (type: character)
#'
#' @return named list with following entries:
#' \describe{
#'   \item{has_changes}{TRUE if changes detected by RC in the directory. (type: logical)}
#'   \item{revision}{revision reported by RC. (type: character)}
#'   \item{latest}{the latest revision reported by RC at the repository. (type: character)}
#' }
#'
#' @family in extending RSuite with RC adapter
#'
#' @examples
#' # create you own RC adapter
#' rc_adapter_create_own <- function() {
#'   result <- rc_adapter_create_base("Own")
#'   class(result) <- c("rc_adapter_own", class(result))
#'   return(result)
#' }
#'
#' #' @export
#' rc_adapter_get_version.rc_adapter_own <- function(rc_adapter, dir) {
#'   # ... detect if working copy is consistent with repository state ...
#'   return(list(has_changes = TRUE,
#'               revision = "0.0",
#'               latest = FALSE))
#' }
#'
#' @export
#'
rc_adapter_get_version <- function(rc_adapter, dir) {
  assert(is_rc_adapter(rc_adapter), "rsuite_rc_adapter object expected for rc_adapter")
  assert(is_nonempty_char1(dir), "Non empty character(1) expected for dir")
  assert(dir.exists(dir), "Existing folder expected for dir")

  UseMethod("rc_adapter_get_version")
}

#'
#' Default implemenation of rc_adapter_get_version
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_get_version.default <- function(rc_adapter, dir) {
  assert(FALSE,
         "rc_adapter_get_version not implemented by %s",
         paste(class(rc_adapter), collapse = " "))
}



#'
#' Remove all RC related administrative entries from folder tree at dir.
#'
#' This is required for cleaning temporary folder during collecting entries to
#' put into project zip package.
#'
#' @param rc_adapter rc adapter object
#' @param dir path to the directory to remove administrators from. The folder must
#'   exist (type: character)
#'
#' @family in extending RSuite with RC adapter
#'
#' @examples
#' # create you own RC adapter
#' rc_adapter_create_own <- function() {
#'   result <- rc_adapter_create_base("Own")
#'   class(result) <- c("rc_adapter_own", class(result))
#'   return(result)
#' }
#'
#' #' @export
#' rc_adapter_remove_admins.rc_adapter_own <- function(rc_adapter, dir) {
#'   # ... unlink RC administrative folders from dir (like .svn or .git) ...
#' }
#'
#' @export
#'
rc_adapter_remove_admins <- function(rc_adapter, dir) {
  assert(is_rc_adapter(rc_adapter), "rsuite_rc_adapter object expected for rc_adapter")
  assert(is_nonempty_char1(dir), "Non empty character(1) expected for dir")
  assert(dir.exists(dir), "Existing folder expected for dir")

  UseMethod("rc_adapter_remove_admins")
}

#'
#' Default implemenation of rc_adapter_remove_admins
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_remove_admins.default <- function(rc_adapter, dir) {
  assert(FALSE,
         "rc_adapter_remove_admins not implemented by %s",
         paste(class(rc_adapter), collapse = " "))
}


#'
#' Checks if object is RC adapter.
#'
#' @param obj object to check.
#' @return TRUE if object is of class rsuite_rc_adapter
#'
#' @keywords internal
#' @noRd
#'
is_rc_adapter <- function(obj) {
  return("rsuite_rc_adapter" %in%  class(obj))
}
