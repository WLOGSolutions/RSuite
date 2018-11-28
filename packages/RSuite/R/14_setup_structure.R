#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for managing project structure.
#----------------------------------------------------------------------------

#'
#' Structure compliance table
#'
#' @keywords internal
#' @noRd
#'
rsuite_strver_compliance <- list(
  "0.1" = c("0.1", "0.2"),
  "0.3" = c("0.3", "0.4", "0.5"),
  "0.6" = c("0.6", "0.7")
)

#'
#' Retrieves earliest RSuite compliant version for the version passed.
#' The result is normalized version.
#'
#' @keywords internal
#' @noRd
#'
get_earliest_strver_comliant <- function(ver) {
  for (v in names(rsuite_strver_compliance)) {
    if (ver %in% rsuite_strver_compliance[[v]]) {
      return(norm_version(v))
    }
  }
  return(norm_version(v))
}

#'
#' Checks project structure and parameters and adapts them to RSuite
#' requirements.
#'
#' @keywords internal
#' @noRd
#'
check_project_structure <- function(prj_dir) {
  stopifnot(dir.exists(prj_dir))

  rsuite_ver <- as.character(utils::packageVersion("RSuite"))
  prj_name <- basename(prj_dir)

  # Verify versions and parameters
  params_file <- file.path(prj_dir, "PARAMETERS")
  params <- force_load_PARAMETERS(params_file, prj_name, rsuite_ver)

  create_struct_dir(params$script_path, "master scripts")
  create_struct_dir(params$lib_path, "libraries")
  create_struct_dir(params$sbox_path, "sandbox")

  # Root project folder
  conf_templ <- file.path(prj_dir, "config_templ.txt")
  if (!file.exists(conf_templ)) {
    write.dcf(data.frame(LogLevel = "INFO"), file = conf_templ)
  }

  if (params$r_ver == current_rver()) {
    # add logger to libraries folder as it will be required for sure
    copy_folder(from = system.file(package = "logging"), # from 98_shell.R
                to = file.path(params$lib_path, "logging"))
  }

  # Update parameters at last
  if (is.na(params$rsuite_ver) || params$rsuite_ver != rsuite_ver) {
    update_PARAMETERS(params_file, rsuite_ver)
  }
}

#'
#' Loads parameters file and verifies if it registered RSuite version against
#' current.
#'
#' If PARAMETERS file does not exist creates it with default contents.
#'
#' @param params_file path to PARAMETERS file
#' @param prj_name name of project
#' @param rsuite_ver current version of RSuite
#'
#' @return project parameters (rsuite_project_params object)
#'
#' @keywords internal
#' @noRd
#'
force_load_PARAMETERS <- function(params_file, prj_name, rsuite_ver) {
  prj_dir <- dirname(params_file)

  if (!file.exists(params_file)) {
    params_dt <- data.frame(RSuiteVersion = rsuite_ver,
                            RVersion = current_rver(), # from 97_rversion.R
                            Project = prj_name,
                            Repositories = sprintf("MRAN[%s]", Sys.Date() - 7),
                            Artifacts = "config_templ.txt")
    write.dcf(params_dt, file = params_file)
    params <- load_prj_parameters(prj_dir)

    pkg_loginfo("Will create project %s structure for RSuite v%s.", prj_name, rsuite_ver)
  } else {
    params <- load_prj_parameters(prj_dir)

    prj_rsuite_ver <- ifelse(is.na(params$rsuite_ver), NA, get_earliest_strver_comliant(params$rsuite_ver))
    cur_rsuite_ver <- get_earliest_strver_comliant(rsuite_ver)
    assert(is.na(prj_rsuite_ver) || prj_rsuite_ver <= cur_rsuite_ver,
           paste0("Project %s has structure compliant with future version of RSuite.",
                  " Structure cannot be adapted to RStuite v%s."),
           prj_name, rsuite_ver)

    if (is.na(prj_rsuite_ver)) {
      pkg_loginfo(paste0("Project %s structure created without help of RSuite.",
                         " Will adapt structure to RSuite v%s."),
                  prj_name, rsuite_ver)
    } else if (prj_rsuite_ver < cur_rsuite_ver) {
      pkg_loginfo(paste0("Project %s has structure compliant with RSuite v%s.",
                         " Will adapt structure to RSuite v%s"),
                  prj_name, params$rsuite_ver, rsuite_ver)
    } else {
      pkg_loginfo("Project %s has structure compliant with current version of RSuite(v%s).",
                  prj_name, rsuite_ver)
    }
  }

  return(params)
}

#'
#' Updates project PARAMETERS file
#'
#' @param  params_file path to project PARAMETERS file to update
#' @param  rsuite_ver version of RSuite.
#'
#' @keywords internal
#' @noRd
#'
update_PARAMETERS <- function(params_file, rsuite_ver) {
  params_dt <- data.frame(read.dcf(params_file), stringsAsFactors = FALSE)

  if (!("RSuiteVersion" %in% colnames(params_dt))) {
    params_dt <- cbind(params_dt, data.frame(RSuiteVersion = rsuite_ver))
  } else {
    params_dt$RSuiteVersion <- rsuite_ver
  }

  if (!("RVersion" %in% colnames(params_dt))) {
    params_dt <- cbind(params_dt,
                       data.frame(RVersion = current_rver()))  # from 97_rversion.R
  }

  if (is.null(params_dt$Repositories)) {
    repos <- c()
    if (!is.null(params_dt$SnapshotDate) && nchar(params_dt$SnapshotDate) > 0) {
      repos <- c(repos, sprintf("MRAN[%s]", params_dt$SnapshotDate))
      params_dt$SnapshotDate <- NULL
    } else {
      repos <- c(repos, "CRAN")
    }

    loc_repo <- ifelse(!is.null(params_dt$LocalRepoPath), params_dt$LocalRepoPath, "repository")
    params_dt$LocalRepoPath <- NULL
    if (nchar(loc_repo) > 0 && dir.exists(file.path(dirname(params_file), loc_repo))) {
      repos <- c(repos, sprintf("Dir[%s]", loc_repo))
    }

    params_dt$Repositories <- paste(repos, collapse = ", ")
  }

  write.dcf(params_dt, file = params_file)
}


#'
#' Creates project structure out of project template.
#'
#' @keywords internal
#' @noRd
#'
create_package_structure <- function(pkg_dir, pkg_tmpl = "builtin") {
  stopifnot(!dir.exists(pkg_dir))

  tmpl_dir <- get_pkg_tmpl_dir(pkg_tmpl) # from 58_templates.R
  assert(!is.null(tmpl_dir) || pkg_tmpl == "builtin",
         "Requested to start package from unknown template '%s'.", pkg_tmpl)

  if (is.null(tmpl_dir)) {
    stopifnot(pkg_tmpl == "builtin")

    builtin_temp <- get_builtin_templs_temp_base() # from 58_templates.R
    on.exit({
      unlink(builtin_temp, recursive = TRUE, force = TRUE)
    },
    add = TRUE)
    tmpl_dir <- file.path(builtin_temp, "package")
  } else {
    validate_pkg_tmpl_struct(tmpl_dir) # from 58_templates.R
  }

  # copy template
  success <- copy_folder(tmpl_dir, pkg_dir) # from 98_shell.R
  assert(success, "Failed to copy template files.")

  files <- list.files(pkg_dir,
                      full.names = TRUE, include.dirs = FALSE, recursive = TRUE, all.files = TRUE)
  files <- files[!file.info(files)$isdir]

  # now replace markers in files
  keywords <- c(
    PackageName = basename(pkg_dir),
    RSuiteVersion = as.character(utils::packageVersion("RSuite")),
    RVersion = current_rver(), # from 97_rversion.R
    Date = as.character(Sys.Date()),
    User = iconv(Sys.info()[["user"]], from = "utf-8", to = "latin1")
  )

  for (f in files) {
    if (is_binary(f)) next
    lines <- readLines(con = f, warn = FALSE)
    lines <- replace_markers(keywords, lines)
    writeLines(lines, con = f)
  }

  rename_files_with_markers(keywords, files)
}


#'
#' Creates a project based on the given template.
#'
#' @param prj_dir project base directory
#'    (type: character).
#'
#' @param prj_tmpl name of (or path to) the project template
#'    (type: character).
#'
#' @keywords internal
#' @noRd
#'
create_project_structure <- function(prj_dir, prj_tmpl = "builtin") {
  tmpl_dir <- get_prj_tmpl_dir(prj_tmpl) # from 58_templates.R
  assert(!is.null(tmpl_dir) || prj_tmpl == "builtin",
         "Requested to start project from unknown template '%s'.", prj_tmpl)

  if (is.null(tmpl_dir)) {
    stopifnot(prj_tmpl == "builtin")

    builtin_temp <- get_builtin_templs_temp_base() # from 58_templates.R
    on.exit({
      unlink(builtin_temp, recursive = TRUE, force = TRUE)
    },
    add = TRUE)
    tmpl_dir <- file.path(builtin_temp, "project")
  } else {
    validate_prj_tmpl_struct(tmpl_dir) # from 58_templates.R
  }

  # copy template
  success <- copy_folder(tmpl_dir, prj_dir) # from 98_shell.R
  assert(success, "Failed to copy template files.")

  # now replace markers in files
  files <- list.files(prj_dir, full.names = TRUE, include.dirs = FALSE, recursive = TRUE)
  files <- files[!file.info(files)$isdir]

  if (!file.exists(file.path(tmpl_dir, "PARAMETERS"))
      || any(grepl("__LatestMRAN__", readLines(file.path(tmpl_dir, "PARAMETERS"))))) {
    mran_repo <- get_latest_mran_repo()
  } else {
    mran_repo <- sprintf("MRAN[%s]", Sys.Date())
  }

  keywords <- c(
    ProjectName = basename(prj_dir),
    RSuiteVersion = as.character(utils::packageVersion("RSuite")),
    RVersion = current_rver(), # from 97_rversion.R
    Date = as.character(Sys.Date()),
    LatestMRAN = mran_repo,
    User = iconv(Sys.info()[["user"]], from = "utf-8", to = "latin1")
  )

  # replace all markers in the file content
  for (f in files) {
    if (is_binary(f)) next
    lines <- readLines(con = f, warn = FALSE)
    lines <- replace_markers(keywords, lines) # from 58_templates.R
    writeLines(lines, con = f)
  }

  rename_files_with_markers(keywords, files)
}

#'
#' Cleans up __rc_ignores from project/package folder created.
#'
#' Use then package/project just created from template not under RC control.
#'
#' @keywords internal
#' @noRd
#'
clear_rc_adapter_infos <- function(dir_path) {
  to_remove <- list.files(dir_path, pattern = "__rc_ignore",
                          recursive = TRUE, full.names = TRUE)
  if (length(to_remove)) {
    unlink(to_remove, force = TRUE, recursive = TRUE)
  }
}


#'
#' Creates folder if does not exists.
#' Asserts that folder is created.
#'
#' @keywords internal
#' @noRd
#'
create_struct_dir <- function(dir, desc) {
  if (!dir.exists(dir)) {
    created <- dir.create(dir, recursive = TRUE)
    assert(created, "Failed to create %s directory: %s.", desc, dir)
  }
}

#'
#' Detects which RC system project is beeing managed with.
#'
#' @return rc_adapter or NULL
#'
#' @keywords internal
#' @noRd
#'
detect_rc_adapter <- function(prj_dir) {
  for (rc_name in reg_rc_adapter_names()) {
    rc_adapter <- find_rc_adapter(rc_name)
    stopifnot(!is.null(rc_adapter))

    if (rc_adapter_is_under_control(rc_adapter, dir = prj_dir)) {
      return(rc_adapter)
    }
  }
}

#'
#' Detects which CI system has triggered current build process.
#'
#' @return ci_adapter or NULL
#'
#' @keywords internal
#' @noRd
#'
detect_ci_adapter <- function() {
  for (ci_name in reg_ci_adapter_names()) {
    ci_adapter <- find_ci_adapter(ci_name)
    stopifnot(!is.null(ci_adapter))

    if (ci_adapter_is_building(ci_adapter)) {
      return(ci_adapter)
    }
  }
}
