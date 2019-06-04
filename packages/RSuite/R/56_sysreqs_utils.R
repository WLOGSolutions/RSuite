#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities related to sysreqs.
#----------------------------------------------------------------------------

#'
#' Retrieves full path to sysreqs database.
#'
#' @return path to sysreqs database.
#'
#' @keywords internal
#' @noRd
#'
get_sysreqsdb_path <- function() {
  dbpath <- system.file(file.path("extdata", "sysreqs", "db"), package = "RSuite")
  assert(dir.exists(dbpath), "No system requirements database found.")

  return(dbpath)
}

#'
#' Detects current platform description.
#'
#' @return named list with following content:
#' \describe{
#'   \item{name}{One of Windows, MacOS, RedHat, Debian (type: character)}
#'   \item{distrib}{Distribution e.g. for Debian: Debian, Ubuntu; for RedHat: CentOS, RedHat, Fedora (type: character(1))}
#'   \item{release}{Distribution release e.g. for Debian: squeeze, wheezy, jessie (type: character(1))}
#'   \item{sysreq_type}{One of Windows, Pkg, RPM, DEB (type: character(1))}
#'   \item{build}{True if build environment is required (type: logical(1))}
#' }
#'
#' @keywords internal
#' @noRd
#'
get_platform_desc <- function() {
  os_info <- get_os_info() # from 98_shell.R

  sysreq_type <- switch(os_info$platform,
                        Windows = "Windows",
                        MacOS = "OSX/brew",
                        RedHat = "RPM",
                        Debian = "DEB",
                        NA_character_)
  assert(!is.na(sysreq_type),
         "Platform %s is currently not supported by sysreqs.", os_info$platform)
  check_syslibs <- switch(
    os_info$platform,
    RedHat = list(cmd = paste("[shell]",
                              "all_pkgs=($(rpm -qa | sed -e \"s/^\\(.\\+\\)-[0-9]\\+[.-][0-9].*$/\\1/\"));",
                              "for lib in :params; do",
                              " if [[ ! \" ${all_pkgs[@]} \" =~ \" ${lib} \" ]]; then echo $lib; fi",
                              "done"),
                  under_root = NULL), # can be root or non root
    Debian = list(cmd = paste("[shell]",
                              "all_pkgs=($(dpkg -l | sed -e \"s/^..[ \\t]\\+\\([^ \\t:]\\+\\).\\+$/\\1/\"));",
                              "for lib in :params; do",
                              " if [[ ! \" ${all_pkgs[@]} \" =~ \" ${lib} \" ]]; then echo $lib; fi",
                              "done"),
                  under_root = NULL),
    MacOS = list(cmd = paste("[shell]",
                             "all_pkgs=($(brew list));",
                             "for lib in :params; do",
                             " if [[ ! \" ${all_pkgs[@]} \" =~ \" ${lib} \" ]]; then echo $lib; fi",
                             "done"),
                 under_root = FALSE)
  )
  instprep_syslibs <- switch(
    os_info$platform,
    Debian = list(cmd = "[shell] apt-get update",
                  under_root = TRUE)
  )
  install_syslibs <- switch(
    os_info$platform,
    RedHat = list(cmd = "[shell] yum install -y :params",
                  under_root = TRUE),
    Debian = list(cmd = "[shell] apt-get install -y :params",
                  under_root = TRUE),
    MacOS = list(cmd = "[shell] brew install :params",
                 under_root = FALSE)
  )

  return(list(name = os_info$platform,
              distrib = os_info$distrib,
              release = os_info$release,
              sysreq_type = sysreq_type,
              lib_tools = list(check = check_syslibs,
                               instprep = instprep_syslibs,
                               install = install_syslibs),
              build = TRUE))
}

#'
#' Verifies libtool specification and retrieves real command to run.
#'
#' @param libtool_spec libtool specification as retrieved from get_platform_desc()$lib_tools
#' @param libtool_desc description of libtool handler
#'
#' @return bare command to run or NULL if command cannot be executed.
#'
#' @noRd
#' @keywords internal
#'
get_libtool_cmd <- function(libtool_spec, libtool_desc) {
  if (is.null(libtool_spec) || is.null(libtool_spec$cmd)) {
    return(NULL)
  }

  cmd <- libtool_spec$cmd
  assert(grepl("^\\[shell\\] ", cmd),
         "System libraries %s handlers other than [shell] are not supported yet", libtool_desc)
  cmd <- gsub("^\\[shell\\]\\s*", "", cmd)
  return(cmd)
}

#'
#' Check if system user is appropriate to run libtool.
#'
#' @param libtool_spec libtool specification as retrieved from get_platform_desc()$lib_tools
#' @param warn If TRUE warning will be displayed descrybing why user is not appropriate
#'
#' @return TRUE if user is appropriate and can run the tool
#'
#' @noRd
#' @keywords internal
#'
is_libtool_user <- function(libtool_spec, warn = TRUE) {
  if (is.null(libtool_spec) || is.null(libtool_spec$under_root)) {
    # no preferences
    return(TRUE)
  }

  if (libtool_spec$under_root && Sys.info()["user"] != "root") {
    if (warn) {
      pkg_logwarn("Only 'root' can update system (install system libraries)")
    }
    return(FALSE)
  }

  if (!libtool_spec$under_root && Sys.info()["user"] == "root") {
    if (warn) {
      pkg_logwarn("Only non 'root' can update system (install system libraries)")
    }
    return(FALSE)
  }

  return(TRUE)
}

#'
#' Detects requirements specified in field in sysreqs database.
#'
#' @param pkg_name package name requirements are for. (type: character)
#' @param field SystemRequirements field from the package DESCRIPTION file. (type: character)
#'
#' @return named list with detected requirements name and list with following fields as value:
#' \describe{
#'   \item{plat_spec}{Platform specific specification}
#'   \item{handlers}{Handlers specification to check, install, built requirement.}
#'   \item{satisfies}{Character vector of other requirements the requirement satisfies.}
#'   \item{params}{Requirement parameters (like libraries to install) detected out of
#'      requirement specification.}
#' }
#'
#' @keywords internal
#' @noRd
#'
get_sysreqs_for <- function(pkg_name, field) {
  stopifnot(is.character(field) && length(field) == 1)

  dbpath <- get_sysreqsdb_path()
  plat_desc <- get_platform_desc()

  matched_reqs <- list()
  for (dbfile in list.files(dbpath, pattern = ".+[.]json")) {
    db_ent <- jsonlite::fromJSON(file.path(dbpath, dbfile))
    db_ent_name <- names(db_ent)[[1]]
    if (db_ent_name %in% names(matched_reqs)) {
      # do not process same tool again
      next
    }

    db_ent_val <- db_ent[[db_ent_name]]

    sysreq <- try_build_sysreq(pkg_name, field, db_ent_name, db_ent_val, plat_desc)
    if (!is.null(sysreq)) {
      matched_reqs[[db_ent_name]] <- sysreq
    }
  }
  matched_reqs
}

#'
#' Builds sysreq object based on sysreq db entry.
#'
#' If checks if field can be matched by the db entry. It positive builds
#' sysreq object describing the requirement for the platform.
#'
#' @param pkg_name name of package sysreqs are built for
#' @param field SystemRequirements field contents (type: character(1))
#' @param db_ent_name name of sysreq db entry (type: character(1))
#' @param db_ent_val value of the sysreq db entry to retrieve information from.
#' @param plat_desc description of platform retrieved with get_platform_desc.
#'
#' @return NULL if fails to match db entry with field or platform is not supported
#'   by db entry. Otherwise returns sysreq entry.
#'
#' @keywords internal
#' @noRd
#'
try_build_sysreq <- function(pkg_name, field, db_ent_name, db_ent_val, plat_desc) {
  match_pars <- character(0)

  # first try match regexp requirements
  re_reqs <- db_ent_val$sysreqs[grepl("^/.+/i?$", db_ent_val$sysreqs)]
  if (length(re_reqs) > 0) {
    re_match <- unlist(lapply(re_reqs,
                              function(re_req) {
                                re <- gsub("^/(.+)/i?$", "\\1", re_req)
                                case_ignore <- grepl("/i$", re_req)
                                if (grepl(re, field, ignore.case = case_ignore)) {
                                  found <- regmatches(field, regexpr(re, field, ignore.case = case_ignore))
                                  gsub(re, "\\1", found) # params or empty string
                                } else {
                                  as.character(NA)
                                }
                              }))
    match_pars <- c(match_pars, re_match)
  }

  # them match fixed requirements
  fx_reqs <- db_ent_val$sysreqs[!grepl("^/.+/i?$", db_ent_val$sysreqs)]
  if (length(fx_reqs) > 0) {
    fx_match <- unlist(lapply(fx_reqs,
                              function(fx_req) {
                                ifelse(grepl(fx_req, field, fixed = TRUE), "", as.character(NA))
                              }))
    match_pars <- c(match_pars, fx_match)
  }

  match_pars <- match_pars[!is.na(match_pars)]
  if (length(match_pars) == 0) {
    return() # not matched
  }

  params <- paste(match_pars[nchar(match_pars) > 0])
  pkg_logdebug("... %s requirements matched for %s ... (params: %s)", db_ent_name, pkg_name, params)
  plat_spec <- get_platform_spec(db_ent_val$platforms, db_ent_name, plat_desc)
  if (is.null(plat_spec)) {
    return() # not supported on platform for some reason
  }

  result <- list(plat_spec = plat_spec,
                 handlers = db_ent_val$handlers,
                 satisfies = trimws(unlist(strsplit(trimws(db_ent_val$satisfies), "\\s+"))),
                 params = params,
                 info = db_ent_val$info)
  class(result) <- "sysreq"
  return(result)
}

#'
#' Selects platform specification for the specified platform out of platforms part of
#'  sysreq db entry.
#'
#' @param dbent_platforms platforms part of sysreq db entry.
#' @param dbent_name name of db entry. (type: character(1))
#' @param plat_desc platform description retrieved by get_platform_desc.
#'
#' @return platform specification for the specified platform or NULL if no
#'   appropriate specification found which means that platform is not supported
#'   (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
get_platform_spec <- function(dbent_platforms, dbent_name, plat_desc) {
  if (!(plat_desc$sysreq_type %in% names(dbent_platforms))) {
    pkg_logdebug("... platform %s not supported by %s", plat_desc$name, dbent_name)
    return()
  }

  plat_specs <- dbent_platforms[[plat_desc$sysreq_type]]
  if (is.character(plat_specs) && length(plat_specs) == 1) {
    return(plat_specs)
  }

  req_type <- ifelse(plat_desc$build, "buildtime", "runtime")
  if (is.data.frame(plat_specs)
      && all(c("distribution", "releases", "runtime", "buildtime") %in% colnames(plat_specs))) {
    distrib <- plat_desc$sysreq_type
    release <- plat_desc$release
    dist_specs <- plat_specs[plat_specs$distribution == distrib
                             & grepl(release, plat_specs$releases, fixed = TRUE), ]
    if (nrow(dist_specs) == 0) {
      dist_specs <- plat_specs[plat_specs$distribution == distrib
                               & unlist(lapply(plat_specs$releases, is.null)), ]
    }
    if (nrow(dist_specs) == 0) {
      pkg_logdebug(".\t ... platform %s(%s %s) not supported by %s",
                   plat_desc$name, distrib, release, dbent_name)
      return()
    }
    return(dist_specs[1, req_type])
  }

  if (is.list(plat_specs)) {
    if (!(req_type %in% names(plat_specs))) {
      pkg_logdebug("... build type %s for platform %s not supported by %s",
                   req_type, plat_desc$name, dbent_name)
      return()
    }
    return(plat_specs[[req_type]])
  }

  pkg_logdebug("... unknown platform format for %s in %s", plat_desc$name, dbent_name)
  return()
}
