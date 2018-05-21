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

  create_struct_dir(file.path(prj_dir, "logs"), "logs")
  create_struct_dir(params$pkgs_path, "packages")
  create_struct_dir(file.path(prj_dir, "tests"), "tests")
  create_struct_dir(params$script_path, "master scripts")

  copy_folder(from = system.file(file.path("extdata", "deployment"), package = "RSuite"),
              to = file.path(prj_dir, "deployment"))
  create_struct_dir(params$lib_path, "libraries")

  # Root project folder
  conf_templ <- file.path(prj_dir, "config_templ.txt")
  if (!file.exists(conf_templ)) {
    write.dcf(data.frame(LogLevel = "INFO"), file = conf_templ)
  }

  create_rproj(prj_dir, prj_name)
  create_rprofile(prj_dir, text = "source(file.path('R', 'set_env.R'), chdir = T)")

  # Scripts folder
  if (length(list.files(path = params$script_path, pattern = "*.R", recursive = F)) == 0) {
    msc_templ <- file.path(params$script_path, "master.R")
    success <- file.copy(
      from = system.file(file.path("extdata", "script_templ.R"), package = "RSuite"),
      to = msc_templ)
    if (!success) {
      pkg_logwarn("Failed to copy master script template: %s", msc_templ)
    }
  }

  create_rprofile(params$script_path, text = "source('set_env.R', chdir = T)")

  set_env_r <- file.path(params$script_path, "set_env.R")
  if (!file.exists(set_env_r)) {
    success <- file.copy(
      from = system.file(file.path("extdata", "set_env.R"), package = "RSuite"),
      to = set_env_r)
    assert(success,
           paste0("Failed to copy environment initialization script: %s.",
                  " Master scripts and deployment packages will probably not work."),
           set_env_r)
  }

  # initialize tests folder
  create_rproj(file.path(prj_dir, "tests"), paste0(prj_name, "_Tests"))
  create_rprofile(file.path(prj_dir, "tests"),
                  text = "source(file.path('..', 'R', 'set_env.R'), chdir = T)")

  if (params$r_ver == current_rver()) {
    # add logger to libraries folder as it will be required for sure
    copy_folder(from = system.file(package = "logging"),
                to = file.path(params$lib_path, "logging"))
  }

  # Update parameters at last
  if (is.na(params$rsuite_ver) || params$rsuite_ver != rsuite_ver) {
    update_PARAMETERS(params_file, rsuite_ver)
  }
}

#'
#' Loads parameters file and verified if its registered RSuite version against
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
  params_dt <- data.frame(read.dcf(params_file), stringsAsFactors = F)

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
create_package_structure <- function(pkg_dir) {
  stopifnot(!dir.exists(pkg_dir))

  files <- suppressWarnings(
    utils::unzip(system.file(file.path("extdata", "PackageTemplate.zip"), package = "RSuite"),
                 exdir = pkg_dir))
  assert(length(files) > 0, "Failed to create package structure at %s", pkg_dir)

  keywords <- list(
    pkg_name = basename(pkg_dir),
    today = as.character(Sys.Date()),
    user = iconv(Sys.info()[["user"]], from = "utf-8", to = "latin1")
  )

  # now replace markers in files
  for (f in files) {
    lines <- readLines(con = f, warn = F)
    lines <- gsub("<PackageName>", keywords$pkg_name, lines)
    lines <- gsub("<Date>", keywords$today, lines)
    lines <- gsub("<User>", keywords$user, lines)
    writeLines(lines, con = f)
  }

  create_rproj(pkg_dir, keywords$pkg_name)
  create_rprofile(pkg_dir, text = "source(file.path('..', '..', 'R', 'set_env.R'), chdir = TRUE)")
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
    created <- dir.create(dir, recursive = T)
    assert(created, "Failed to create %s directory: %s.", desc, dir)
  }
}

#'
#' Copy template Rproj into specified folder under specified name.
#' Does not copy if any Rproj exists already in the folder.
#' Logs warning if failed to copy.
#'
#' @keywords internal
#' @noRd
#'
create_rproj <- function(dir, name) {
  if (length(list.files(path = dir, pattern = "*.Rproj", recursive = FALSE)) > 0) {
    return()
  }

  tryCatch({
    con <- unz(system.file(file.path("extdata", "RStudio_proj_templ.zip"), package = "RSuite"),
               filename = "RStudio_proj_templ.Rproj")
    lines <- readLines(con)
  },
  error = function(e) {
    pkg_logwarn("Failed to copy RStudio project: %s", rproj_file)
    return(FALSE)
  },
  finally = {
    close(con)
  })

  rproj_file <- file.path(dir, paste0(name, ".Rproj"))
  writeLines(lines, con = rproj_file)
  return(TRUE)
}

#'
#' Creates .Rprofile at specified location with specified contents if not exists.
#'
#' @keywords internal
#' @noRd
#'
create_rprofile <- function(dir, text = "RSuite::prj_load()") {
  rprof_file <- file.path(dir, ".Rprofile")
  if (!file.exists(rprof_file)) {
    writeLines(text = text, con = rprof_file)
  }
}

#'
#' Copies folder from onto folder to if to not exists.
#'
#' @keywords internal
#' @noRd
#'
copy_folder <- function(from, to) {
  if (dir.exists(to)) {
    return(invisible(TRUE))
  }
  if (basename(from) == basename(to)) {
    success <- file.copy(from = from, to = dirname(to), recursive = TRUE, copy.mode = TRUE)
    return(invisible(success))
  }

  success <- dir.create(to, recursive = TRUE)
  for (ent in list.files(from, all.files = TRUE, recursive = FALSE, include.dirs = TRUE, no.. = TRUE)) {
    success <- (file.copy(from = file.path(from, ent), to = to, recursive = TRUE, copy.mode = TRUE)
                && success)
  }

  invisible(success)
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
