#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Package API related to configuration of projects.
#----------------------------------------------------------------------------

#'
#' Updates project configuration to use only specified repository adapters.
#'
#' @details
#' Project configuration (together with repositories to be used) is stored in
#' PARAMETERS file in project folder.
#'
#' After project configuration have been changed repository adapters are
#' initialized on the project.
#'
#' Repository adapters will be used for dependencies detection in the same
#' order as passed in names.
#'
#' @param repos vector of repository adapters configuration to use by the project.
#'    Each should be in form <repo_adapter_name>[<arg>]. They should be all
#'    registered. (type: character)
#' @param prj project object to update configuration for. If not passed will use loaded
#'    project or default whichever exists. Will init default project from working
#'    directory if no default project exists. (type: rsuite_project, default: NULL)
#'
#' @family in project configuration
#'
#' @examples
#' # create exemplary project base folder
#' prj_base <- tempfile("example_")
#' dir.create(prj_base, recursive = TRUE, showWarnings = FALSE)
#'
#' # start project
#' prj <- prj_start("my_project", skip_rc = TRUE, path = prj_base)
#'
#' # present initial project configuration
#' cat(readLines(file.path(prj$path, "PARAMETERS")), sep = "\n")
#'
#' # set repositories to use
#' prj_config_set_repo_adapters(c("CRAN", "MRAN[2018-01-01]"), prj = prj)
#'
#' # present final project configuration
#' cat(readLines(file.path(prj$path, "PARAMETERS")), sep = "\n")
#'
#' @export
#'
prj_config_set_repo_adapters <- function(repos, prj = NULL) {
  assert(!missing(repos) && is.character(repos),
         "character(N) vector expected for repos")

  known_ra_names <- reg_repo_adapter_names()
  ra_specs <- parse_repo_adapters_spec(repos)
  assert(all(names(ra_specs) %in% known_ra_names),
         "Unknown repo adapter names detected: %s",
         paste(setdiff(unique(names(ra_specs)), known_ra_names), collapse = ", "))

  prj <- safe_get_prj(prj)

  params_file <- file.path(prj$path, "PARAMETERS")
  params_dt <- read.dcf(params_file)
  repos_val <- paste(repos, collapse = ", ")
  if (!("Repositories" %in% colnames(params_dt))) {
    params_dt <- cbind(params_dt, data.frame(Repositories = repos_val))
  } else {
    params_dt[, "Repositories"] <- repos_val
  }
  write.dcf(params_dt, file = params_file)

  params <- prj$load_params()
  for (ra_ix in seq_along(ra_specs)) {
    ra_name <- names(ra_specs)[[ra_ix]]
    repo_adapter <- find_repo_adapter(ra_name)
    stopifnot(!is.null(repo_adapter))

    ra_info <- repo_adapter_get_info(repo_adapter, params)
    if (!ra_info$readonly) {
      mgr <- repo_adapter_create_manager(repo_adapter, params = params, ix = ra_ix)
      repo_manager_init(mgr)
      repo_manager_destroy(mgr)
    }
  }
}


#'
#' Updates project configuration to use specified R Version.
#'
#' Project configuration (together with R version to be used) is stored in
#' PARAMETERS file in project folder.
#'
#' @param rver R version to be used by the project. (type: character)
#' @param prj project object to update configuration for. If not passed will use loaded
#'    project or default whichever exists. Will init default project from working
#'    directory if no default project exists. (type: rsuite_project, default: NULL)
#' @param validate If TRUE will check if R version is valid for the platform.
#'    (type: logical, default: TRUE)
#'
#' @family in project configuration
#'
#' @examples
#' # create exemplary project base folder
#' prj_base <- tempfile("example_")
#' dir.create(prj_base, recursive = TRUE, showWarnings = FALSE)
#'
#' # start project
#' prj <- prj_start("my_project", skip_rc = TRUE, path = prj_base)
#'
#' # present initial project configuration
#' cat(readLines(file.path(prj$path, "PARAMETERS")), sep = "\n")
#'
#' # set repositories to use
#' prj_config_set_rversion("3.2", prj = prj, validate = FALSE)
#'
#' # present final project configuration
#' cat(readLines(file.path(prj$path, "PARAMETERS")), sep = "\n")
#'
#' @export
#'
prj_config_set_rversion <- function(rver, prj = NULL, validate = TRUE) {
  assert(is_nonempty_char1(rver), "Non empty character(1) expected for rver")

  if (any(validate)) {
    tryCatch({
      get_rscript_path(rver)
    },
    error = function(e) {
      assert(FALSE, "Could not find valid R %s in path", rver)
    })
  }

  prj <- safe_get_prj(prj)

  params_file <- file.path(prj$path, "PARAMETERS")
  params_dt <- read.dcf(params_file)
  if (!("RVersion" %in% colnames(params_dt))) {
    params_dt <- cbind(params_dt, data.frame(RVersion = majmin_rver(rver)))
  } else {
    params_dt[, "RVersion"] <- majmin_rver(rver)
  }
  write.dcf(params_dt, file = params_file)

  pkg_loginfo("Package R version set to %s", params_dt[1, "RVersion"])
}
