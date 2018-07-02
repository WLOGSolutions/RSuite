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
#'   \item{name}{One of Windows, RPM, DEB (type: character)}
#'   \item{distrib}{Distribution e.g. for DEB: Debian, Ubuntu; for RPM: CentOS, RedHat, Fedora (type: character(1))}
#'   \item{release}{Distribution release e.g. for Debian: squeeze, wheezy, jessie (type: character(1))}
#'   \item{build}{True if build environment is required (type: logical(1))}
#' } one of
#'
#' @keywords internal
#' @noRd
#'
get_platform_desc <- function() {
  platform <- .get_platform()
  assert(!is.na(platform), "Could not detect current platform name.")

  distrib <- .get_distrib(platform)

  check_syslibs <- switch(
    platform,
    RPM = paste("[shell]",
                "all_pkgs=$(rpm -qa | sed -e \"s/^\\(.\\+\\)-[0-9]\\+[.-][0-9].*$/\\1/\");",
                "for lib in :params; do if [[ ! \"${all_pkgs[@]}\" =~ ${lib} ]]; then echo $lib; fi done"),
    DEB = paste("[shell]",
                "all_pkgs=$(dpkg -l | sed -e \"s/^..[ \\t]\\+\\([^ \\t:]\\+\\).\\+$/\\1/\");",
                "for lib in :params; do if [[ ! \"${all_pkgs[@]}\" =~ ${lib} ]]; then echo $lib; fi done")
  )
  install_syslibs <- switch(
    platform,
    RPM = "[shell] yum install -y :params",
    DEB = "[shell] apt-get update && apt-get install -y :params"
  )

  return(list(name = platform,
              distrib = distrib,
              release = .get_release(distrib),
              lib_tools = list(check = check_syslibs,
                               install = install_syslibs),
              build = TRUE))
}

#'
#' Retrieves platform identifier: Windows, DEB or RPM
#'
#' @return platform identifier retrieved or NA if failed to retrieve.
#'   (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
.get_platform <- function() {
  os_type <- get_os_type()
  if (os_type == "windows") {
    return("Windows")
  }
  if (os_type == "macos") {
    return("Pkg")
  }
  if (os_type == "unix") {
    if (file.exists("/etc/redhat-release") || file.exists("/etc/fedora-release")) {
      return("RPM")
    }
    if (file.exists("/etc/debian_version")) {
      return("DEB")
    }
    return(NA_character_)
  }
  return(NA_character_)
}

#'
#' Retrieves distribution for the platform: Ubuntu, Debian, Fedora, CentOS or RedHat.
#'
#' @param platform current platform identifier (type: character(1))
#' @return platform distribution name or NA if failed to detect. (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
.get_distrib <- function(platform) {
  if (platform == "DEB") {
    if (file.exists("/etc/lsb-release") && any(grepl("DISTRIB[ _]ID=Ubuntu", readLines("/etc/lsb-release")))) {
      return("Ubuntu")
    }
    if (file.exists("/etc/os-release") && any(grepl("ID=debian", readLines("/etc/os-release")))) {
      return("Debian")
    }
    return(NA_character_)
  }

  if (platform == "RPM") {
    if (file.exists("/etc/fedora-release")) {
      return("Fedora")
    }

    if (!file.exists("/etc/redhat-release")) {
      # unexpected: RPM must have redhat-release file
      return(NA_character_)
    }
    if (file.exists("/etc/os-release") && any(grepl("ID=\"?centos\"?", readLines("/etc/os-release")))) {
      return("CentOS")
    }
    if (file.exists("/etc/redhat-release") && any(grepl("^Red Hat Linux", readLines("/etc/redhat-release")))) {
      return("RedHat")
    }
    return(NA_character_)
  }

  return(NA_character_)
}

#'
#' Retrieves release for passed distribution.
#'
#' @param distrib current distribution to detect release for. (type: character(1))
#' @return release identifier retrieved or NA if failed to detected. (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
.get_release <- function(distrib) {
  if (is.na(distrib)) {
    return(NA_character_)
  }

  if (distrib == "Ubuntu") {
    if (!file.exists("/etc/lsb-release")) {
      return(NA_character_)
    }
    codename <- grep("^DISTRIB[ _]CODENAME=", readLines("/etc/lsb-release"), value = TRUE)
    if (length(codename) != 1) {
      return(NA_character_)
    }
    return(gsub("^.+=([a-z]+).*$", "\\1", codename))
  }

  if (distrib == "Debian") {
    if (!file.exists("/etc/os-release")) {
      return(NA_character_)
    }
    ver <- grep("^VERSION=", readLines("/etc/os-release"), value = TRUE)
    if (length(ver) != 1) {
      return(NA_character_)
    }
    return(gsub("^.+\\(([^)]+)\\).*$", "\\1", ver))
  }

  if (distrib == "CentOS") {
    if (!file.exists("/etc/redhat-release")) {
      return(NA_character_)
    }
    rel_ln <- readLines("/etc/redhat-release")[1]
    if (!grepl("^.+release\\s+([0-9]+[.][0-9]+).+$", rel_ln)) {
      return(NA_character_)
    }
    return(gsub("^.+release\\s+([0-9]+[.][0-9]+).+$", "\\1", rel_ln))
  }

  if (distrib == "RedHat") {
    if (!file.exists("/etc/redhat-release")) {
      return(NA_character_)
    }
    rel_ln <- readLines("/etc/redhat-release")[1]
    if (!grepl("^.+release\\s+([0-9]+([.][0-9]+)?)\\s+\\(([^)]+)\\).*$", rel_ln)) {
      return(NA_character_)
    }
    return(gsub("^.+release\\s+([0-9]+([.][0-9]+)?)\\s+\\(([^)]+)\\).*$", "\\3", rel_ln))
  }

  return(NA_character_)
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
  if (!(plat_desc$name %in% names(dbent_platforms))) {
    pkg_logdebug("... platform %s not suported by %s", plat_desc$name, dbent_name)
    return()
  }

  plat_specs <- dbent_platforms[[plat_desc$name]]
  if (is.character(plat_specs) && length(plat_specs) == 1) {
    return(plat_specs)
  }


  req_type <- ifelse(plat_desc$build, "buildtime", "runtime")
  if (is.data.frame(plat_specs)
      && all(c("distribution", "releases", "runtime", "buildtime") %in% colnames(plat_specs))) {
    distrib <- plat_desc$distrib
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

  if (is.list(plat_specs)){
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
