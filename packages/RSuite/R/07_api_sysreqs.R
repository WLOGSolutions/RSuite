#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Package API related to system requirements.
#----------------------------------------------------------------------------

#'
#' Prints out all system requirements from dependencies and project packages.
#'
#' @param prj project object to collect sys requiremens for. If not passed will
#'    use loaded project or default whichever exists. Will init default project
#'    from working directory if no default project exists.
#'    (type: rsuite_project, default: NULL)
#' @return named list with named with package names and containing system
#'    requirements as value.
#'
#' @export
#'
sysreqs_collect <- function(prj = NULL) {
  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()
  # no need to check R version here. We will not build anything

  pkg_loginfo("Detecting repositories (for R %s)...", params$r_ver)

  repo_infos <- get_all_repo_infos(params) # from 53_repositories.R
  log_repo_infos(repo_infos) # from 53_repositories.R

  avail_vers <- resolve_prj_deps(repo_infos, params)
  pkg_loginfo("Detected %s dependencies. Processing...", length(vers.get_names(avail_vers)))

  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = T)
  on.exit({ unlink(tmp_dir, recursive = T, force = T) }, add = T)

  result <- list()

  dloaded <- pkg_download(avail_vers$avails, dest_dir = tmp_dir)
  for(r in 1:nrow(dloaded)) {
    dl <- dloaded[r, ]
    dcf <- get_pkg_desc(dl$Package, dl$Path) # from 51_pkg_info.R

    if (!is.null(dcf$SystemRequirements)) {
      result[[dl$Package]] <- gsub("\n", " ", dcf$SystemRequirements)
    }
  }

  prj_packages <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  if (length(prj_packages) > 0) {
    pkg_loginfo("Processing project packages...")
    for(pkg in names(prj_packages)) {
      dcf <- get_pkg_desc(pkg, file.path(params$pkgs_path, prj_packages[[pkg]])) # from 51_pkg_info.R

      if (!is.null(dcf$SystemRequirements)) {
        result[[pkg]] <- gsub("\n", " ", dcf$SystemRequirements)
      }
    }
  }

  pkg_loginfo("Done.")
  return(result)
}

#'
#' Collects system requirements with sysreqs_collect and performs checks for
#'   their existance.
#'
#' @param prj project object to check sys requiremens for. If not passed will
#'    use loaded project or default whichever exists. Will init default project
#'    from working directory if no default project exists.
#'    (type: rsuite_project, default: NULL)
#'
#' @export
#'
sysreqs_check <- function(prj = NULL) {
  sysreqs <- sysreqs_collect(prj)
  if (length(sysreqs) == 0) {
    pkg_loginfo("No system requirements detected to check.")
    return(invisible())
  }

  tools <- list()
  syslibs <- list()

  satisfied <- list()

  for(pkg_name in names(sysreqs)) {
    pkg_sysreqs <- sysreqs[[pkg_name]]
    r_sysreqs <- get_sysreqs_for(pkg_name, pkg_sysreqs)
    for(req_name in names(r_sysreqs)) {
      check_handler <- r_sysreqs[[req_name]]$handlers$check
      if (is.null(check_handler) || check_handler == "[syslib]") {
        syslibs[[req_name]] <- c(unlist(syslibs[req_name]), pkg_name)
      } else if (check_handler == "[tool]") {
        tools[[req_name]] <- c(unlist(tools[req_name]), pkg_name)
      } else {
        pkg_logwarn("[%s] Unknown check handler for %s: %s", pkg_name, req_name, check_handler)
        next
      }

      for(req_satisfied in r_sysreqs[[req_name]]$satisfies) {
        satisfied[[req_satisfied]] <- c(satisfied[[req_satisfied]], req_name)
      }
    }
  }

  for(req_satisfied in names(satisfied)) {
    pkg_loginfo("Requirement for %s will be satisfied by %s; not checking", req_satisfied, paste(satisfied[[req_satisfied]], collapse = ", "))
    if (req_satisfied %in% names(tools)) { tools[[req_satisfied]] <- NULL }
    if (req_satisfied %in% names(syslibs)) { syslibs[[req_satisfied]] <- NULL }
  }

  tools_exists <- file.exists(Sys.which(names(tools)))
  assert(all(tools_exists), "Lack of required tools detected in environment: %s", paste(names(tools)[!tools_exists], collapse = ", "))

  assert(length(syslibs) == 0,
         "Checking for existance of system libraries not implemented yet")

  pkg_loginfo("All system requirements satisfied.")
}


#'
#' Collects system requirements with sysreqs_collect and builds/installs them.
#'
#' @param prj project object to handle sys requiremens for. If not passed will
#'    use loaded project or default whichever exists. Will init default project
#'    from working directory if no default project exists.
#'    (type: rsuite_project, default: NULL)
#'
#' @export
#'
sysreqs_install <- function(prj = NULL) {
  sysreqs <- sysreqs_collect(prj)
  if (length(sysreqs) == 0) {
    pkg_loginfo("No system requirements detected to install.")
    return(invisible())
  }

  params <- safe_get_prj(prj)$load_params()

  build_specs <- list()
  satisfied <- list()

  for(pkg_name in names(sysreqs)) {
    pkg_sysreqs <- sysreqs[[pkg_name]]
    r_sysreqs <- get_sysreqs_for(pkg_name, pkg_sysreqs)
    for(req_name in names(r_sysreqs)) {
      install_handler <- r_sysreqs[[req_name]]$handlers$install
      assert(is.null(install_handler),
             "[%s](%s) Install handlers not supported yet", pkg_name, req_name)

      if (req_name %in% names(build_specs)) {
        # they use same sysreqsdb so commands to run are same. Just add more parameters.
        build_specs[[req_name]]$params <- paste(build_specs[[req_name]]$params, r_sysreqs[[req_name]]$params)
        build_specs[[req_name]]$packages <- c(build_specs[[req_name]]$packages, pkg_name)
        next
      }

      build_handler <- r_sysreqs[[req_name]]$handlers$build
      if (!is.null(build_handler)) {
        assert(grepl("^\\[shell\\]", build_handler),
               "[%s](%s) Build handler other than [shell] are not supported yet", pkg_name, req_name)
        cmd <- gsub("^\\[shell\\]\\s*(.+)$", "\\1", build_handler)

        tool_wspace <- gsub("\\", "/", file.path(params$prj_path, req_name), fixed = TRUE)
        cmd <- gsub(":path", tool_wspace, cmd, fixed = TRUE)
        cmd <- gsub(":tool", r_sysreqs[[req_name]]$plat_spec, fixed = TRUE)

        build_specs[[req_name]] <- list(cmd = cmd,
                                        params = r_sysreqs[[req_name]]$params,
                                        packages = c(pkg_name))
      }

      for(req_satisfied in r_sysreqs[[req_name]]$satisfies) {
        satisfied[[req_satisfied]] <- c(satisfied[[req_satisfied]], req_name)
      }
    }
  }

  for(req_satisfied in names(satisfied)) {
    pkg_loginfo("Requirement for %s will be satisfied by %s; not building", req_satisfied, paste(satisfied[[req_satisfied]], collapse = ", "))
    build_specs[[req_satisfied]] <- NULL
  }

  # TODO: run install specs

  for(req_name in names(build_specs)) {
    build_spec <- build_specs[[req_name]]
    pkg_loginfo("Building %s for %s ...", req_name, paste(build_spec$packages, collapse = ", "))

    cmd <- gsub(":params", r_sysreqs[[req_name]]$params, build_spec$cmd, fixed = TRUE)

    # TODO get retcode
    get_cmd_lines(sprintf("building %s for %s", req_name, paste(build_spec$packages, collapse = ", ")),
                  cmd = cmd,
                  log_debug = TRUE)

    pkg_loginfo("... done")
  }

  pkg_loginfo("All system requirements installed.")
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
#' @importFrom jsonlite fromJSON
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

    match_pars <- character(0)
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
      next
    }

    params <- paste(match_pars[nchar(match_pars) >0])
    pkg_logdebug("... %s requirements matched for %s ... (params: %s)", db_ent_name, pkg_name, params)
    plat_spec <- get_platform_spec(db_ent_val$platforms, db_ent_name, plat_desc)
    if (is.null(plat_spec)) {
      next
    }

    matched_reqs[[db_ent_name]] <- list(plat_spec = plat_spec,
                                        handlers = db_ent_val$handlers,
                                        satisfies = trimws(unlist(strsplit(trimws(db_ent_val$satisfies), "\\s+"))),
                                        params = params)
  }
  matched_reqs
}

get_platform_spec <- function(dbent_platforms, dbent_name, plat_desc) {
  if (!(plat_desc$name %in% names(dbent_platforms))) {
    pkg_logdebug("... platform %s not suported by %s", platform, dbent_name)
    return()
  }

  plat_specs <- dbent_platforms[[plat_desc$name]]
  if (is.character(plat_specs) && length(plat_specs) == 1) {
    return(plat_specs)
  }


  req_type <- ifelse(plat_desc$build, "buildtime", "runtime")
  if (is.data.frame(plat_specs) && all(c("distribution", "releases", "runtime", "buildtime") %in% colnames(plat_specs))) {
    # TODO:
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
  platform <- switch(.Platform$OS.type,
                     windows = "Windows",
                     linux = ifelse(file.exists("/etc/redhat-release"), "RPM",
                                    ifelse(file.exists("/etc/debian_version"), "DEB",
                                           as.character(NA))),
                     as.character(NA))
  assert(!is.na(platform), "Could not detect current platform name.")

  return(list(name = platform,
              distrib = "Ubuntu",
              release = "wily",
              build = TRUE))
}

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


