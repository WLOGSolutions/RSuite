#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities to support RSuite as a whole: like adapters registration or RSuite
# version management.
#----------------------------------------------------------------------------

#'
#' Checks if a newer version of RSuite is available.
#'
#' @return NULL if a newer version is not available or newest available version number.
#'
#' @family miscellaneous
#'
#' @examples
#' \donttest{
#'   # print latest version available or NULL if latest is currently installed
#'   rsuite_check_version()
#' }
#'
#' @export
#'
rsuite_check_version <- function() {
  pkgs <- suppressWarnings({
    utils::available.packages(repos = "https://wlog-rsuite.s3.amazonaws.com", filters = list())
  })
  pkgs <- data.frame(pkgs, stringsAsFactors = FALSE, row.names = NULL)[, c("Package", "Version")]
  pkgs <- pkgs[pkgs$Package == "RSuite", ]
  if (!nrow(pkgs)) {
    return()
  }

  max_norm_ver <- gsub("-", ".", max(norm_version(pkgs$Version)))
  cur_norm_ver <- gsub("-", ".", norm_version(as.character(utils::packageVersion("RSuite"))))
  if (max_norm_ver <= cur_norm_ver) {
    return()
  }

  max_ver <- denorm_version(max(norm_version(pkgs$Version)))
  return(max_ver)
}

#'
#' Updates RSuite to newest available version.
#'
#' @param lib.dir folder path to install RSuite into. Folder must exist.
#'   (type: character(1); default: \code{Sys.getevn("R_LIBS_USER")})
#'
#' @return TRUE if updated (invisible).
#'
#' @family miscellaneous
#'
#' @examples
#' \donttest{
#'   lib_dir <- tempfile("Rsuite_")
#'   dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
#'
#'   rsuite_update(lib_dir)
#' }
#'
#' @export
#'
rsuite_update <- function(lib.dir = Sys.getenv("R_LIBS_USER")) {
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

  rsuite_ver <- vers.build("RSuite", vmin = ver, vmax = ver)

  pkg_loginfo("Resolving dependencies (for R %s)...", rver)
  avail_vers <- resolve_dependencies(rsuite_ver, # from 11_install_prj_deps.R
                                     repo_infos = repo_infos, pkg_types = pkg_types)
  stopifnot(avail_vers$has_avails())

  prev_lib_path <- .libPaths()
  tryCatch({
    .libPaths(lib.dir) # install it globally or in user env

    install_dependencies(avail_vers,
                         lib_dir = .libPaths()[[1]], # install into default location
                         rver = rver,
                         check_repos_consistency = FALSE)
  },
  finally = {
    .libPaths(prev_lib_path)
  })

  pkg_loginfo("All done.")
  invisible(TRUE)
}

#'
#' Registers repository adapter to use for projects.
#'
#' @param repo_adapter object complying rsuite_repo_adapter signature.
#'
#' @family miscellaneous
#'
#' @examples
#' \donttest{
#'   repo_adapter <- repo_adapter_create_base("Own") # create your custom adapter
#'   class(repo_adapter) <- c("repo_adapter_own", class(repo_adapter))
#'   rsuite_register_repo_adapter(repo_adapter)
#' }
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
#' Gets all names of known repository adapters.
#'
#' @return names of registered repository management adapters as character vector.
#'
#' @family miscellaneous
#'
#' @examples
#' rsuite_get_repo_adapter_names()
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
#' @family miscellaneous
#'
#' @examples
#' rc_adapter <- rc_adapter_create_base("Own") # create your custom adapter
#' class(rc_adapter) <- c("rc_adapter_own", class(rc_adapter))
#'
#' # register it
#' rsuite_register_rc_adapter(rc_adapter)
#'
#' # unregister it
#' rsuite_unregister_rc_adapter("Own")
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
#' Unregisters RC (revision control) adapter.
#'
#' @param name RC adapter name to unregister.
#'
#' @family miscellaneous
#'
#' @examples
#' rc_adapter <- rc_adapter_create_base("Own") # create your custom adapter
#' class(rc_adapter) <- c("rc_adapter_own", class(rc_adapter))
#'
#' # register it
#' rsuite_register_rc_adapter(rc_adapter)
#'
#' # unregister it
#' rsuite_unregister_rc_adapter("Own")
#'
#' @export
#'
rsuite_unregister_rc_adapter <- function(name) {
  assert(!is.null(name) && is.character(name) && length(name) == 1 && nchar(name) > 0,
         "Non empty character(1) required for name")
  reg_rc_adapter(name, NULL)
}


#'
#' Gets all names of known RC (revision control) adapters.
#'
#' @return names of registered rc adapters as character vector.
#'
#' @family miscellaneous
#'
#' @examples
#' rsuite_get_rc_adapter_names()
#'
#' @export
#'
rsuite_get_rc_adapter_names <- function() {
  reg_rc_adapter_names()
}
