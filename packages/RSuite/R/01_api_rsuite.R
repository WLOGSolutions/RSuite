#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities to support RSuite as a whole: like adapters registration or RSuite
# version management.
#----------------------------------------------------------------------------

#'
#' Checks if newer version of RSuite is available.
#'
#' @return NULL if newer version is not available or newest available version number.
#'
#' @export
#'
rsuite_check_version <- function() {
  pkgs <- suppressWarnings({
    utils::available.packages(repos = 'https://wlog-rsuite.s3.amazonaws.com', filters = list())
  })
  pkgs <- data.frame(pkgs, stringsAsFactors = F, row.names = NULL)[, c('Package', 'Version')]
  pkgs <- pkgs[pkgs$Package == 'RSuite', ]
  if (!nrow(pkgs)) {
    return()
  }

  max_norm_ver <- gsub("-", ".", max(norm_version(pkgs$Version)))
  cur_norm_ver <- gsub("-", ".", norm_version(as.character(utils::packageVersion('RSuite'))))
  if (max_norm_ver <= cur_norm_ver) {
    return()
  }

  max_ver <- denorm_version(max(norm_version(pkgs$Version)))
  return(max_ver)
}

#'
#' Updates RSuite.
#'
#' @return TRUE if updated (invisible).
#'
#' @export
#'
rsuite_update <- function() {
  ver <- rsuite_check_version()
  if (is.null(ver)) {
    pkg_loginfo("Latest version of RSuite installed.")
    return(invisible(FALSE))
  }
  pkg_loginfo("Installing v%s of RSuite...", ver)

  rver <- current_rver() # from 97_rversion.R
  
  pkg_types <- unique(c(.Platform$pkgType, "source"))
  repo_infos <- build_repo_infos( # from 53_repositories.R
    spec = list(
      CRAN = "https://cloud.r-project.org/",
      S3 = "https://wlog-rsuite.s3.amazonaws.com"), 
    types = pkg_types,
    rver = rver)
  log_repo_infos(repo_infos)     # from 53_repositories.R

  rsuite_ver <- versions.build("RSuite", vmin = ver, vmax = ver)

  pkg_loginfo("Resolving dependencies (for R %s)...", rver)
  avail_vers <- resolve_missings(rsuite_ver, # from 11_install_prj_deps.R
                                 repo_infos = repo_infos, pkg_types = pkg_types)
  stopifnot(avail_vers$has_avails())
  
  prev_lib_path <- .libPaths()
  tryCatch({
    .libPaths(Sys.getenv("R_LIBS_USER")) # install it globally or in user env

    install_dependencies(avail_vers, 
                         lib_dir = .libPaths()[[1]], # install into default location
                         rver = rver)
  }, finally = {
    .libPaths(prev_lib_path)
  })

  pkg_loginfo("All done.")
  invisible(TRUE)
}

#'
#' Registers repository management adapter to use for projects.
#'
#' @param repo_adapter object complying rsuite_repo_adapter signature.
#'
#' @export
#'
rsuite_register_repo_adapter <- function(repo_adapter) {
  assert(!is.null(repo_adapter) && is_repo_adapter(repo_adapter),
         "Repo adapter object expected for repo_adapter")
  assert(is.null(find_repo_adapter(repo_adapter$name)),
         "Repo adapter '%s' is already registered", repo_adapter$name)

  reg_repo_adapter(repo_adapter$name, repo_adapter)
}

#'
#' Gets all names of registered repository management adapters.
#'
#' @return names of registered repository management adapters as chracter vector.
#'
#' @export
#'
rsuite_get_repo_adapter_names <- function() {
  reg_repo_adapter_names()
}


#'
#' Registers RC (revision control) adapter to use for projects.
#'
#' @param rc_adapter object complying rsuite_rc_adapter signature.
#'
#' @export
#'
rsuite_register_rc_adapter <- function(rc_adapter) {
  assert(!is.null(rc_adapter) && is_rc_adapter(rc_adapter),
         "RC adapter object expected for rc_adapter")
  assert(is.null(find_rc_adapter(rc_adapter$name)),
         "RC adapter '%s' is already registered", rc_adapter$name)

  reg_rc_adapter(rc_adapter$name, rc_adapter)
}

#'
#' Gets all names of registered RC (revision control) adapters.
#'
#' @return names of registered rc adapters as chracter vector.
#'
#' @export
#'
rsuite_get_rc_adapter_names <- function() {
  reg_rc_adapter_names()
}

