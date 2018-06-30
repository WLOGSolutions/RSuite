#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for management of project parameters.
#----------------------------------------------------------------------------

#'
#' Loads project parameters from specified path.
#'
#' @param prj_path path to base directory of project (the one containing
#'   PARAMETERS file). (type: character)
#' @return object of rsuite_project_parameters
#'
#' @keywords internal
#' @noRd
#'
load_prj_parameters <- function(prj_path) {
  assert(is.character(prj_path) && length(prj_path) == 1,
         "character(1) expected for prj_path")
  assert(file.exists(file.path(prj_path, "PARAMETERS")),
         "No project PARAMETERS file found at %s", prj_path)

  dcf <- read.dcf(file.path(prj_path, "PARAMETERS"))

  params <- list(
    prj_path = rsuite_fullUnifiedPath(prj_path),
    rsuite_ver = ifelse("RSuiteVersion" %in% colnames(dcf), dcf[1, "RSuiteVersion"], NA),
    r_ver = ifelse("RVersion" %in% colnames(dcf), dcf[1, "RVersion"], current_rver()), # from 97_rversion.R

    # Date of CRAN snapshot to look for dependencies in.
    #  if empty will use official CRAN and newest package versions available
    snapshot_date = ifelse("SnapshotDate" %in% colnames(dcf), dcf[1, "SnapshotDate"], ""),

    pkgs_path = rsuite_fullUnifiedPath(file.path(prj_path,
                                                 ifelse("PackagesPath" %in% colnames(dcf),
                                                        dcf[1, "PackagesPath"], "packages"))),
    script_path = rsuite_fullUnifiedPath(file.path(prj_path,
                                                   ifelse("ScriptPath" %in% colnames(dcf),
                                                          dcf[1, "ScriptPath"], "R"))),
    irepo_path = rsuite_fullUnifiedPath(file.path(prj_path, "deployment", "intrepo")),

    # Specifies where to put local project environment
    lib_path = rsuite_fullUnifiedPath(file.path(prj_path, "deployment", "libs")),
    # Specifies where to put user installed libraries
    sbox_path = rsuite_fullUnifiedPath(file.path(prj_path, "deployment", "sbox")),
    # Specifies where to put environment lock file
    lock_path = rsuite_fullUnifiedPath(file.path(prj_path, "deployment", "env.lock")),

    zip_version = ifelse("ZipVersion" %in% colnames(dcf), dcf[1, "ZipVersion"], ""),
    project = ifelse("Project" %in% colnames(dcf), dcf[1, "Project"], basename(prj_path)),
    artifacts = ifelse("Artifacts" %in% colnames(dcf), dcf[1, "Artifacts"], ""),
    excludes = ifelse("Excludes" %in% colnames(dcf), dcf[1, "Excludes"], ""),

    # repo_adapters to use for the project
    repo_adapters = ifelse("Repositories" %in% colnames(dcf), dcf[1, "Repositories"], "CRAN"),

    # This defines which type of packages are expected on the platform
    #   and how to build project packages.
    pkgs_type = switch(get_os_type(),
                       windows = "win.binary",
                       macos = .Platform$pkgType, # e.g. mac.binary.el-capitan
                       "source"),
    aux_pkgs_type = switch(get_os_type(),
                           windows = "source",
                           macos = "source",
                           "binary"),
    bin_pkgs_type = switch(get_os_type(),
                           windows = "win.binary",
                           macos = .Platform$pkgType, # e.g. mac.binary.el-capitan
                           "binary")
  )

  if (!dir.exists(params$lib_path)) {
    dir.create(params$lib_path, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(params$sbox_path)) {
    dir.create(params$sbox_path, recursive = TRUE, showWarnings = FALSE)
  }

  params$get_safe_project_name <- function() {
    gsub("[\\/\"\'<>]+", "_", params$project)
  }

  params$get_repo_adapter_names <- function() {
    specs <- unlist(strsplit(params$repo_adapters, ","))
    return(names(parse_repo_adapters_spec(specs)))
  }

  params$get_repo_adapter_arg <- function(repo_adapter_name, default, ix) {
    specs <- unlist(strsplit(params$repo_adapters, ","))
    parsed_specs <- parse_repo_adapters_spec(specs)
    if (!is.na(ix)) {
      parsed_specs <- parsed_specs[ix]
    }
    arg <- parsed_specs[names(parsed_specs) == repo_adapter_name]
    arg[is.null(arg) || nchar(arg) == 0] <- default
    names(arg) <- NULL
    return(arg)
  }

  params$get_intern_repo_path <- function() {
    intern_mgr <- repo_manager_dir_create(params$irepo_path, params$pkgs_type, params$r_ver)
    repo_manager_init(intern_mgr)
    return(rsuite_fullUnifiedPath(params$irepo_path))
  }

  class(params) <- "rsuite_project_params"
  return(params)
}
