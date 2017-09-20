#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities related to managing information about repositories.
#----------------------------------------------------------------------------

#'
#' @keywords internal
#'
#' Creates repository info object.
#'
#' It contains information on about path, and provided parameters.
#' It can also retrieve supported contrib urls.
#'
#' @param path path to repository. url with schema expected (type: character).
#' @param expected_types project types expected to be found in repository
#'    (type: character).
#' @param rver R version to create repositories for
#'
#' @return object of type rsuite_repo_info.
#'
.create_repo_info <- function(path, expected_types, rver) {
  stopifnot(regexpr("^(https?|ftp|file)(://.*)", path) != -1) # url path expected

  types <- c()
  for(tp in expected_types) {
    pkgs_url <- paste0(rsuite_contrib_url(path, tp, rver = rver), "/PACKAGES")
    try({
      suppressWarnings({
        con <- url(pkgs_url, open = "rt")
      })
      ln <- readLines(con, n = 1)
      close(con)
      if (grepl("^Package:", ln)) {
        types <- c(types, tp)
      }
    }, silent = T)
  }

  res <- list(
    path = path,
    types = types,
    rver = rver,

    to_str = function() {
      sprintf("%s (%s)", path, paste(types, collapse = ", "))
    },
    get_contrib_url = function(type) {
      if (type %in% types) {
        return(rsuite_contrib_url(path, type, rver = rver))
      }
      NULL
    }
  )
  class(res) <- "rsuite_repo_info"
  return(res)
}

#'
#' @keywords internal
#'
#' Parses repository adapter configuration and extracts repository arguments.
#'
#' @param spec character vector specifying repositories in form <name>[<arg>]
#'   (type: character)
#'
#' @return character vector which values are repository arguments and names are
#'   repository names.
#'
parse_repo_adapters_spec <- function(specs) {
  specs <- trimws(specs)
  names <- trimws(gsub("\\[.*\\]$", "", specs))
  args <- trimws(gsub("^\\[|\\]$", "", gsub("^[^[]+", "", specs)))
  names(args) <- names
  return(args)
}

#'
#' @keywords internal
#'
#' Creates repos specification based on Url for repositories specified in params.
#'
#' @param params project parameters to extract repositories from. (type: rsuit_proj_params)
#'
#' @return character vector with values in form Url[<url>] for each repository path
#'   in params.
#'
make_detached_repos <- function(params) {
  urls <- c()
  
  ra_names <- params$get_repo_adapter_names()
  if (length(ra_names) < 1) {
    return(urls)
  }
  
  for(ix in 1:length(ra_names)) {
    ra_name <- ra_names[[ix]]
    repo_adapter <- find_repo_adapter(ra_name)
    if (is.null(repo_adapter)) {
      next
    }
    repo_path <- repo_adapter_get_path(repo_adapter, params, ix)
    urls <- c(urls, repo_path)
  }
  
  result <- sprintf("Url[%s]", urls)
  return(result)
}

#'
#' @keywords internal
#'
#' Retrieves all the repositories infos to use for project.
#'
#' @return non empty named list of repository descriptions as rsuite_repo_info object.
#'
get_all_repo_infos <- function(params, rver = NULL) {
  if (is.null(rver)) {
    rver <- params$r_ver
  }

  repos <- list()
  ra_names <- params$get_repo_adapter_names()
  if (length(ra_names) < 1) {
    return(repos)
  }
  
  non_reliable <- c()
  seen_adapters <- list()
  for(ix in 1:length(ra_names)) {
    ra_name <- ra_names[ix]
    if (!(ra_name %in% names(seen_adapters))) {
      repo_adapter <- find_repo_adapter(ra_name)
      if (is.null(repo_adapter)) {
        pkg_logwarn("Project is configured to use unknown repo adapter %s", ra_name)
        next
      }
      ra_info <- repo_adapter_get_info(repo_adapter, params)
  
      if (!ra_info$readonly) {
        mgr <- repo_adapter_create_manager(repo_adapter, params = params)
        repo_manager_init(mgr)
        repo_manager_destroy(mgr)
      }
      if (!ra_info$reliable) {
        non_reliable <- c(non_reliable, ra_name)
      }
      seen_adapters[[ra_name]] <- repo_adapter
    } else {
      repo_adapter <- seen_adapters[[ra_name]]
    }
    
    repo_path <- repo_adapter_get_path(repo_adapter, params, ix)
    if (is.null(names(repo_path))) {
      if (length(repo_path) == 1) { names(repo_path) <- sprintf("%s#%s", ra_name, ix) }
      else { names(repo_path) <- sprintf("%s#%s", paste0(ra_name, 1:length(repo_path)), ix) }
    }

    repos <- c(repos,
               build_repo_infos(repo_path,
                                types = c(params$pkgs_type, params$aux_pkgs_type),
                                rver = rver))
  }

  if (!is.null(non_reliable)) {
    pkg_logwarn(paste0("Project is configured to use non reliable repositories: %s.",
                       " You should use only reliable repositories to be sure of",
                       " project consistency over time."),
                paste(non_reliable, collapse = ", "))
  }
  assert(!is.null(repos), "No repositories to look for dependencies in!")

  return(repos)
}

#'
#' @keywords internal
#'
#' Based on names pathes to repositories build named list of repository infos.
#'
#' @param spec names list with pathes to repositories
#' @param types types to consider for each repository
#' @param rver R version to build repositories for
#'
#' @return named list with appropriate rsuite_repo_info objects.
#'
build_repo_infos <- function(spec, types, rver) {
  result <- lapply(X = spec, F = function(path) { .create_repo_info(path, types, rver) })
  names(result) <- names(spec)
  return(result)
}

#'
#' @keywords internal
#'
#' Logs information on repository infos,
#'
log_repo_infos <- function(repo_infos) {
  pkg_loginfo("Will look for dependencies in ...")
  for(n in names(repo_infos)) {
    pkg_loginfo(".      %10s = %s", n, repo_infos[[n]]$to_str())
  }
}

#'
#' @keywords internal
#'
#' Retrieves contrib_urls for provided type out of list of repository infos.
#'
#' @param repo_infos list of objects of rsuite_repo_info class.
#' @param type to retrieve url_contribs for
#'
#' @return character(N) containing retrieved contrib_urls
#'
retrieve_contrib_urls <- function(repo_infos, type) {
  result <- unlist(lapply(X = repo_infos,
                          F = function(ri) { ri$get_contrib_url(type) }))
  return(result)
}
