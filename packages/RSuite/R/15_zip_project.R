#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities to support project zipping.
#----------------------------------------------------------------------------

#'
#' Detects zip_ver for the project based on enforced zip version,
#' ZipVersion from project PARAMETERS file or from versions of project packages.
#'
#' If zip version enforced with zip_ver parameter it is marked with 'x' at the end.
#'
#' If zip version is detected from ZipVersion or project packages RC version is
#' required to generate consitent zip package. Validation for changes in RC is
#' performed.
#'
#' @return named list with following structure
#' \describe{
#'   \item{ver}{Version detected (type: character)}
#'   \item{rev}{RC revision or NULL (type: character)}
#' }
#'
#' @keywords internal
#' @noRd
#'
detect_zip_version <- function(params, zip_ver, zip_rev = NULL) {
  if (!is.null(zip_ver)) {
    assert(is_nonempty_char1(zip_ver), "Non empty character(1) expected for zip_ver")
    assert(grepl("^\\d+(\\D\\d+)*$", zip_ver), "Please, provide zip_ver of form DD.DD")
  }

  if (!is.null(zip_rev)) {
    assert(is_nonempty_char1(zip_rev), "Non empty character(1) expected for zip_rev")
    assert(grepl("^\\d+$", zip_rev),
           paste0("Enforced revision(%s) is invalid:",
                  " it must contain digits only as it is appended to project packages version numbers."),
           zip_rev)
  }

  if (!is.null(zip_ver)) {
    if (!is.null(zip_rev)) {
      return(list(ver = paste0(zip_ver, "_", zip_rev, "x"), rev = zip_rev))
    }
    return(list(ver = paste0(zip_ver, "x"), rev = NULL))
  }

  prjinfo <- retrieve_consistent_prjinfo(params) # from 19_pack_helpers.R
  if (!is.null(prjinfo$ver)) {
    if (!is.null(zip_rev)) {
      prjinfo$rev <- zip_rev # overwrite with enforced revision
    }
    return(prjinfo)
  }

  zip_ver <- params$zip_version
  if (!is.null(zip_ver) && nchar(zip_ver) > 0) {
    assert(grepl("^\\d+(\\D\\d+)*$", zip_ver),
           paste0("ZipVersion in project PARAMETERS does not conform to DD.DD form.",
                  " Please, provide ZipVersion in acceptable form or enforce version with zip_ver"))
  } else {
    pkg_vers <- retrieve_project_pkgsvers(params$pkgs_path)
    assert(length(pkg_vers) > 0,
           paste0("Project does not contain packages, so zip version cannot be detected.",
                  " Please, enforce zip version with zip_ver parameter.",
                  " Use the --version option in RSuite CLI or pass the version",
                  " explicitly along with the zip_ver argument"))
    zip_ver <- denorm_version(max(norm_version(pkg_vers)))
  }

  stopifnot(!is.null(zip_ver))
  # we have zip_ver, now detect revision number

  if (!is.null(zip_rev)) {
    # zip_rev enforced; it has been checked for validity already
    return(list(ver = paste0(zip_ver, "_", zip_rev, "x"), rev = zip_rev))
  }

  revision <- NULL
  if (!is.null(prjinfo$rev)) {
    # revision stored in prjinfo
    revision <- prjinfo$rev
    stopifnot(grepl("^\\d+$", revision)) # validity insured while preparing prjinfo
  }

  if (is.null(revision)) {
    # CI build number
    revision <- detect_ci_build_number()
    assert(is.null(revision) || grepl("^\\d+$", revision),
           paste0("CI build number detected(%s) is invalid:",
                  " it must contain digits only as it is appended to project packages version numbers."),
           revision)
  }

  if (is.null(revision)) {
    # RC revision
    revision <- detect_consistent_revision(params)
    assert(is.null(revision) || grepl("^\\d+$", revision),
           paste0("RC revision detected(%s) is invalid:",
                  " it must contain digits only as it is appended to project packages version numbers."),
           revision)
  }

  if (is.null(revision)) {
    return(list(ver = paste0(zip_ver, "x"), rev = NULL))
  }

  return(list(ver = paste0(zip_ver, "_", revision), rev = revision))
}

#'
#' Detects CI project build number.
#'
#' @return ci build number detected (type: character)
#'
#' @keywords internal
#' @noRd
#'
detect_ci_build_number <- function() {
  ci_adapter <- detect_ci_adapter()
  if (is.null(ci_adapter)) {
    return(NULL)
  }

  ci_ver <- ci_adapter_get_version(ci_adapter)
  return(ci_ver)
}

#'
#' Detects revision of the project and checks if it is consistent:
#' project does not have changes and project revision is latest.
#'
#' @param params rsuite_project_params object.
#'
#' @return revision number detected (type: character)
#'
#' @keywords internal
#' @noRd
#'
detect_consistent_revision <- function(params) {
  rc_adapter <- detect_rc_adapter(params$prj_path)
  if (is.null(rc_adapter)) {
    return(NULL)
  }

  rc_ver <- rc_adapter_get_version(rc_adapter, params$prj_path)

  assert(!rc_ver$has_changes,
         paste0("Project has non commited changes.",
                " Please, commit all your changes to preserve project consistency."))
  assert(!rc_ver$needs_update,
         paste0("Project is not up to date with repository.",
                " Please, update to newest version to preserve project consistency."))

  return(rc_ver$revision)
}


#'
#' Builds project zip file.
#'
#' @param params rsuite_project_params object.
#' @param version package version to use.
#' @param odir otput dir path.
#'
#' @return created zip file path (type: character(1), invisible)
#'
#' @keywords internal
#' @noRd
#'
zip_project <- function(params, version, odir) {
  wdir <- tempfile()
  if (!dir.exists(wdir)) {
    success <- dir.create(wdir, recursive = TRUE)
    assert(success, "Failed to create temporary folder %s", wdir)
  }
  on.exit({
    unlink(wdir, recursive = TRUE, force = TRUE)
  },
  add = TRUE)

  prj_name <- params$get_safe_project_name()
  root_dir <- file.path(wdir, prj_name)

  pkg_loginfo("Preparing files for zipping...")
  if (dir.exists(root_dir)) {
    unlink(root_dir, recursive = TRUE, force = TRUE)
  }
  success <- dir.create(root_dir)
  assert(success, "Failed to create temporary folder")

  success <- dir.create(file.path(root_dir, "logs"))
  assert(success,
         "Failed to create logs folder in temporary folder")

  success <- file.copy(params$lib_path, root_dir, recursive = TRUE)
  assert(success,
         "Failed to copy project libraries to temporary folder")

  success <- file.copy(file.path(params$prj_path, ".Rprofile"), root_dir)

  if (dir.exists(params$script_path)) {
    success <- file.copy(params$script_path, root_dir, recursive = TRUE)
    assert(success,
           "Failed to copy scripts to temporary folder")
  }

  for (a in gsub("^\\s+|\\s+$", "", unlist(strsplit(params$artifacts, ",")))) {
    apath <- file.path(params$prj_path, a)
    success <- suppressWarnings(file.copy(apath, root_dir, recursive = TRUE))
    assert(success,
           "Failed to copy artifact %s to temporary folder", a)
  }

  # remove any .Rproj.user, .Rhistory, .RData, if exists in root_dir
  to_rem <- c(
    list.files(root_dir, pattern = "^[.]Rproj.user", recursive = TRUE, include.dirs = TRUE, all.files = TRUE),
    list.files(root_dir, pattern = "^[.]RData", recursive = TRUE, include.dirs = TRUE, all.files = TRUE),
    list.files(root_dir, pattern = "^[.]Rhistory", recursive = TRUE, include.dirs = TRUE, all.files = TRUE),
    list.files(root_dir, pattern = "^[.]svn", recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
  )
  unlink(file.path(root_dir, to_rem), recursive = TRUE, force = TRUE)

  # remove any RC administratives from in root_dir
  rc_adapter <- detect_rc_adapter(params$prj_path)
  if (!is.null(rc_adapter)) {
    rc_adapter_remove_admins(rc_adapter, root_dir)
  }

  writeLines(sprintf("%s v%s", params$project, version), file.path(root_dir, "readme.txt"))

  zip_file_name <- sprintf("%s_%s.zip", prj_name, version)
  pkg_loginfo("... done. Creating zip file %s ...", zip_file_name)

  zip_file_path <- file.path(rsuite_fullUnifiedPath(odir), zip_file_name)
  success <- zip_folder(wdir, zip_file_path)
  assert(success, "Failed to create zip file (zip returned non 0 return status).")

  pkg_loginfo("Zip file created: %s", file.path(odir, zip_file_name))

  return(invisible(zip_file_path))
}


#'
#' Creates zip archive out passed directory.
#'
#' @param wspace folder to create archive from
#' @param zip_file_path name of zip file to create in working directory
#'
#' @return TRUE if all ziped successfuly.
#'
#' @keywords internal
#' @noRd
#'
zip_folder <- function(wspace, zip_file_path) {
  wd <- setwd(wspace)
  tryCatch({
    zip_res <- run_rscript(c("retcode <- utils::zip(%s, file = '.', zip = 'zip')",
                             "stopifnot(retcode == 0)"),
                           rscript_arg("zipfile", zip_file_path),
                           log_debug = FALSE)
  },
  finally = {
    setwd(wd)
  })

  if (is.null(zip_res)) {
    return(TRUE)
  }

  if (zip_res == FALSE) {
    pkg_logwarn("Zip building aborted for %s", zip_file_path)
  } else {
    pkg_logwarn("Zip building failed: %s", zip_res)
  }
  return(FALSE)
}


#'
#' Extracts zip archive into passed directory.
#'
#' @param dest_dir folder to create extract to
#' @param zip_file_path name of zip file to extract.
#'
#' @return TRUE if all unziped successfuly.
#'
#' @keywords internal
#' @noRd
#'
unzip_folder <- function(dest_dir, zip_file_path) {
  zip_res <- run_rscript("utils::unzip(%s, %s)",
                         rscript_arg("zipfile", zip_file_path),
                         rscript_arg("exdir", dest_dir),
                         log_debug = FALSE)
  if (is.null(zip_res)) {
    return(TRUE)
  }

  if (zip_res == FALSE) {
    pkg_logwarn("Unzip aborted for %s", zip_file_path)
  } else {
    pkg_logwarn("Unzip failed: %s", zip_res)
  }
  return(FALSE)
}
