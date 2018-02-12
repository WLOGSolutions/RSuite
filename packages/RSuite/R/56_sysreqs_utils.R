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
#'   \item{name}{One of Windows, RPM, DEB (type: character)}
#'   \item{distrib}{Distribution e.g. for DEB: Debian, Ubuntu; for RPM: CentOS, RedHat, Fedora (type: character)}
#'   \item{release}{Distribution release e.g. for Debian: squeeze, wheezy, jessie (type: character)}
#'   \item{build}{True if build environment is required (type: logical)}
#' } one of
#'
#' @keywords internal
#'
get_platform_desc <- function() {
  get_platform <- switch(.Platform$OS.type,
                         windows = function() { "Windows" },
                         unix = function() {
                           if (file.exists("/etc/redhat-release") || file.exists("/etc/fedora-release")) {
                             "RPM"
                           } else if(file.exists("/etc/debian_version")) {
                             "DEB"
                           } else {
                             as.character(NA)
                           }
                         },
                         function() { as.character(NA) })
  platform <- get_platform()
  assert(!is.na(platform), "Could not detect current platform name.")

  get_distrib <- switch(platform,
                        DEB = function() {
                          if (file.exists("/etc/lsb-release") && any(grepl("DISTRIB[ _]ID=Ubuntu", readLines("/etc/lsb-release")))) {
                            "Ubuntu"
                          } else if(file.exists('/etc/os-release') && any(grepl("ID=debian", readLines('/etc/os-release')))) {
                            "Debian"
                          } else {
                            as.character(NA)
                          }
                        },
                        RPM = function() {
                          if (file.exists("/etc/fedora-release")) {
                            "Fedora"
                          } else if (file.exists("/etc/redhat-release")) {
                            if (file.exists("/etc/os-release") && any(grepl("ID=\"?centos\"?", readLines("/etc/os-release")))) {
                              "CentOS"
                            } else if (file.exists('/etc/redhat-release') && any(grepl("^Red Hat Linux", readLines("/etc/redhat-release")))) {
                              "RedHat"
                            } else {
                              as.character(NA)
                            }
                          } else {
                            as.character(NA)
                          }
                        },
                        function() { as.character(NA) })
  distrib <- get_distrib()

  get_release <- switch(distrib,
                        Ubuntu = function() {
                          if (!file.exists("/etc/lsb-release")) {
                            return(as.character(NA))
                          }
                          codename <- grep("^DISTRIB[ _]CODENAME=", readLines('/etc/lsb-release'), value = TRUE)
                          if (length(codename) != 1) {
                            return(as.character(NA))
                          }
                          gsub("^.+=([a-z]+).*$", "\\1", codename)
                        },
                        Debian = function() {
                          if (!file.exists('/etc/os-release')) {
                            return(as.character(NA))
                          }
                          ver <- grep("^VERSION=", readLines('/etc/os-release'), value = TRUE)
                          if (length(ver) != 1) {
                            return(as.character(NA))
                          }
                          gsub("^.+\\(([^)]+)\\).*$", "\\1", ver)
                        },
                        CentOS = function() {
                          if (!file.exists('/etc/redhat-release')) {
                            return(as.character(NA))
                          }

                          rel_ln <- readLines('/etc/redhat-release')[1]
                          if (!grepl("^.+release\\s+([0-9]+[.][0-9]+).+$", rel_ln)) {
                            return(as.character(NA))
                          }
                          gsub("^.+release\\s+([0-9]+[.][0-9]+).+$", "\\1", rel_ln)
                        },
                        RedHat = function() {
                          if (!file.exists('/etc/redhat-release')) {
                            return(as.character(NA))
                          }

                          rel_ln <- readLines('/etc/redhat-release')[1]
                          if (!grepl("^.+release\\s+([0-9]+([.][0-9]+)?)\\s+\\(([^)]+)\\).*$", rel_ln)) {
                            return(as.character(NA))
                          }
                          gsub("^.+release\\s+([0-9]+([.][0-9]+)?)\\s+\\(([^)]+)\\).*$", "\\3", rel_ln)
                        },
                        function() { as.character(NA) })

  check_syslibs <- switch(
    platform,
    RPM = '[shell] all_pkgs=$(rpm -qa | sed -e "s/^\\(.\\+\\)-[0-9]\\+[.-][0-9].*$/\\1/"); for lib in :params; do if [[ ! "${all_pkgs[@]}" =~ ${lib} ]]; then echo $lib; fi done',
    DEB = '[shell] all_pkgs=$(dpkg -l | sed -e "s/^..[ \\t]\\+\\([^ \\t:]\\+\\).\\+$/\\1/"); for lib in :params; do if [[ ! "${all_pkgs[@]}" =~ ${lib} ]]; then echo $lib; fi done'
  )
  install_syslibs <- switch(
    platform,
    RPM = '[shell] yum install -y :params',
    DEB = '[shell] apt-get update && apt-get install -y :params'
  )

  return(list(name = platform,
              distrib = distrib,
              release = get_release(),
              lib_tools = list(check = check_syslibs,
                               install = install_syslibs),
              build = TRUE))
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
#'   \item{satisfies}{Character vertor of other requirements the requirement satisfies.}
#'   \item{params}{Requirement parameters (like libraries to install) detected out of
#'      requirement specification.}
#' }
#'
#' @keywords internal
#'
get_sysreqs_for <- function(pkg_name, field) {
  stopifnot(is.character(field) && length(field) == 1)

  dbpath <- get_sysreqsdb_path()
  plat_desc <- get_platform_desc()

  matched_reqs <- list()
  for(dbfile in list.files(dbpath, pattern = ".+[.]json")) {
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

  params <- paste(match_pars[nchar(match_pars) >0])
  pkg_logdebug("... %s requirements matched for %s ... (params: %s)", db_ent_name, pkg_name, params)
  plat_spec <- get_platform_spec(db_ent_val$platforms, db_ent_name, plat_desc)
  if (is.null(plat_spec)) {
    return() # not supported on platform for some reason
  }

  result <- list(plat_spec = plat_spec,
                 handlers = db_ent_val$handlers,
                 satisfies = trimws(unlist(strsplit(trimws(db_ent_val$satisfies), "\\s+"))),
                 params = params)
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
#'
get_platform_spec <- function(dbent_platforms, dbent_name, plat_desc) {
  if (!(plat_desc$name %in% names(dbent_platforms))) {
    pkg_logdebug("... platform %s not suported by %s", plat_desc$name, dbent_name)
    return()
  }

  plat_specs <- dbent_platforms[[plat_desc$name]]
  if (is.character(plat_specs) && length(plat_specs) == 1) {
    return(plat_specs)
  }


  req_type <- ifelse(plat_desc$build, "buildtime", "runtime")
  if (is.data.frame(plat_specs) && all(c("distribution", "releases", "runtime", "buildtime") %in% colnames(plat_specs))) {
    distrib <- plat_desc$distrib
    release <- plat_desc$release
    dist_specs <- plat_specs[plat_specs$distribution == distrib & grepl(release, plat_specs$releases, fixed = TRUE), ]
    if (nrow(dist_specs) == 0) {
      dist_specs <- plat_specs[plat_specs$distribution == distrib & unlist(lapply(plat_specs$releases, is.null)), ]
    }
    if (nrow(dist_specs) == 0) {
      pkg_logdebug(".\t ... platform %s(%s %s) not supported by %s", plat_desc$name, distrib, release, dbent_name)
      return()
    }
    return(dist_specs[1, req_type])
  }

  if (is.list(plat_specs)){
    if (!(req_type %in% names(plat_specs))) {
      pkg_logdebug("... build type %s for platform %s not supported by %s", req_type, plat_desc$name, dbent_name)
      return()
    }
    return(plat_specs[[req_type]])
  }

  pkg_logdebug("... unknown platform format for %s in %s", platform, dbent_name)
  return()
}


