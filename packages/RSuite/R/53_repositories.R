#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities related to managing information about repositories.
#----------------------------------------------------------------------------

#'
#' Creates repository info object.
#'
#' It contains information on about path, and provided parameters.
#' It can also retrieve supported contrib urls.
#'
#' @param params project parameters. Used to access available packages cache.
#'   Can be NULL is packages cache will not be considered.
#' @param path path to repository. url with schema expected (type: character).
#' @param expected_types project types expected to be found in repository
#'    (type: character).
#' @param rver R version to create repositories info for
#'
#' @return object of type rsuite_repo_info.
#'
#' @keywords internal
#' @noRd
#'
.create_repo_info <- function(path, expected_types, rver) {
  stopifnot(regexpr("^(https?|ftp|file)(://.*)", path) != -1) # url path expected

  types <- unlist(lapply(X = expected_types,
                         FUN = function(tp) {
                            avails <- get_available_packages(path, tp, rver)
                            if (is.null(avails)) {
                              NULL
                            } else {
                              tp
                            }
                          }))

  get_archsrc_cache_fpath <- function(pkg_name) {
    if (!("source" %in% types)) {
      return()
    }

    repos_cache_dir <- get_cache_dir("repos_cache") # from 98_shell.R
    if (is.null(repos_cache_dir)) {
      return()
    }

    contrib_url <- rsuite_contrib_url(path, "source", rver = rver)
    if (grepl("^file://", contrib_url)) {
      return()
    }

    cache_file <- paste0(utils::URLencode(contrib_url, TRUE), "_srcarch_", pkg_name, ".rds")
    cache_fpath <- file.path(repos_cache_dir, cache_file)
    return(cache_fpath)
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
    },
    get_avail_pkgs = function(type) {
      if (type %in% types) {
        return(get_available_packages(path, type, rver = rver))
      }
      NULL
    },
    get_arch_src_url = function(pkg_name) {
      # is_local_url from 99_rpatches.R
      if (is_local_url(path)) {
        return(NULL)
      }
      return(sprintf("%s/00Archive/%s", rsuite_contrib_url(path, "source", rver = rver), pkg_name))
    },
    get_arch_src_cache = function(pkg_name) {
      cache_fpath <- get_archsrc_cache_fpath(pkg_name)
      if (is.null(cache_fpath)) {
        return(NULL)
      }
      if (!file.exists(cache_fpath) || .is_cache_too_old(cache_fpath)) {
        return(NULL)
      }

      cache <- tryCatch({
        readRDS(cache_fpath)
      },
      error = function(e) NULL)

      return(cache)
    },
    set_arch_src_cache = function(pkg_name, avails) {
      stopifnot(is.data.frame(avails)
                && all(c("Package", "Version", "File", "Repository") %in% colnames(avails)))

      cache_fpath <- get_archsrc_cache_fpath(pkg_name)
      if (!is.null(cache_fpath)) {
        tryCatch({
          saveRDS(avails, file = cache_fpath)
        },
        error = function(e) {
          print(e)
        })
      }
    }
  )
  class(res) <- "rsuite_repo_info"
  return(res)
}

#'
#' Parses repository adapter configuration and extracts repository arguments.
#'
#' @param spec character vector specifying repositories in form <name>[<arg>]
#'   (type: character)
#'
#' @return character vector which values are repository arguments and names are
#'   repository names.
#'
#' @keywords internal
#' @noRd
#'
parse_repo_adapters_spec <- function(specs) {
  specs <- trimws(specs)
  names <- trimws(gsub("\\[.*\\]$", "", specs))
  args <- trimws(gsub("^\\[|\\]$", "", gsub("^[^[]+", "", specs)))
  names(args) <- names
  return(args)
}

#'
#' Creates repos specification based on Url for repositories specified in params.
#'
#' @param params project parameters to extract repositories from. (type: rsuit_proj_params)
#'
#' @return character vector with values in form Url[<url>] for each repository path
#'   in params.
#'
#' @keywords internal
#' @noRd
#'
make_detached_repos <- function(params) {
  urls <- c()

  ra_names <- params$get_repo_adapter_names()
  if (length(ra_names) < 1) {
    return(urls)
  }

  for (ix in seq_along(ra_names)) {
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
#' Retrieves all the repositories infos to use for project.
#'
#' @return non empty named list of repository descriptions as rsuite_repo_info object.
#'
#' @keywords internal
#' @noRd
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
  for (ix in seq_along(ra_names)) {
    ra_name <- ra_names[ix]
    if (!(ra_name %in% names(seen_adapters))) {
      repo_adapter <- find_repo_adapter(ra_name)
      seen_adapters[[ra_name]] <- repo_adapter
      if (is.null(repo_adapter)) {
        pkg_logwarn("Project is configured to use unknown repo adapter %s", ra_name)
        next
      }
    }
    repo_adapter <- seen_adapters[[ra_name]]
    if (is.null(repo_adapter)) {
      next
    }

    ra_info <- repo_adapter_get_info(repo_adapter, params)
    if (!ra_info$readonly) {
      mgr <- repo_adapter_create_manager(repo_adapter, params = params, ix = ix)
      repo_manager_init(mgr)
      repo_manager_destroy(mgr)
    }
    if (!ra_info$reliable) {
      non_reliable <- c(non_reliable, ra_name)
    }

    repo_path <- repo_adapter_get_path(repo_adapter, params, ix)
    if (is.null(names(repo_path))) {
      if (length(repo_path) == 1) {
        names(repo_path) <- sprintf("%s#%s", ra_name, ix)
      } else {
        names(repo_path) <- sprintf("%s#%s", paste0(ra_name, seq_along(repo_path)), ix)
      }
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
#' Based on names pathes to repositories build named list of repository infos.
#'
#' @param spec names list with pathes to repositories.
#' @param types types to consider for each repository.
#' @param rver R version to build repositories for.
#'
#' @return named list with appropriate rsuite_repo_info objects.
#'
#' @keywords internal
#' @noRd
#'
build_repo_infos <- function(spec, types, rver) {
  result <- lapply(X = spec, FUN = function(path) .create_repo_info(path, types, rver))
  names(result) <- names(spec)
  return(result)
}

#'
#' Logs information on repository infos,
#'
#' @keywords internal
#' @noRd
#'
log_repo_infos <- function(repo_infos) {
  pkg_loginfo("Will look for dependencies in ...")
  for (n in names(repo_infos)) {
    pkg_loginfo(".      %10s = %s", n, repo_infos[[n]]$to_str())
  }
}

#'
#' Retrieves contrib_urls for provided type out of list of repository infos.
#'
#' @param repo_infos list of objects of rsuite_repo_info class.
#' @param type to retrieve url_contribs for
#'
#' @return character(N) containing retrieved contrib_urls
#'
#' @keywords internal
#' @noRd
#'
retrieve_contrib_urls <- function(repo_infos, type) {
  result <- unlist(lapply(X = repo_infos,
                          FUN = function(ri) ri$get_contrib_url(type)))
  return(result)
}

#'
#' Retrieves repository available packages.
#'
#' It's just wrapper over get_curl_available_packages.
#'
#' @param path path to repository. url with schema expected (type: character).
#' @param type to retrieve packages for
#' @param rver R version to retrieve packages for.
#'
#' @return data.frame as available.packages return or NULL if type is not
#'   supported by the repository.
#'
#' @keywords internal
#' @noRd
#'
get_available_packages <- function(path, type, rver) {
  stopifnot(all(regexpr("^(https?|ftp|file)(://.*)", path) != -1)) # url path expected

  contrib_url <- rsuite_contrib_url(path, type, rver)
  return(get_curl_available_packages(contrib_url))
}


#'
#' Retrieves available packages under contrib url.
#'
#' Packages with all versions are retrieved.
#'
#' Reads from cache if available. Retrieves available packages if not cached and
#'   stores result into cache.
#'
#' @param contrib_url url to retrive packages available at. (type: character)
#'
#' @return data.frame as available.packages return or NULL if type is not
#'   supported by the repository.
#'
#' @keywords internal
#' @noRd
#'
get_curl_available_packages <- function(contrib_url) {
  stopifnot(all(regexpr("^(https?|ftp|file)(://.*)", contrib_url) != -1)) # url expected

  repos_cache_dir <- get_cache_dir("repos_cache") # from 98_shell.R

  if (is.null(repos_cache_dir)) {
    curl2cfile <- as.list(rep("", length(contrib_url)))
    names(curl2cfile) <- contrib_url
  } else {
    # local repositories are not cached
    noncache_curl <- contrib_url[grepl("^file://", contrib_url)]
    curl2cfile <- as.list(rep("", length(noncache_curl)))

    cache_curl <- setdiff(contrib_url, noncache_curl)
    if (length(cache_curl)) {
      curl2cfile <- c(curl2cfile,
                      file.path(repos_cache_dir, paste0(utils::URLencode(cache_curl, TRUE), ".rds")))
    }
    names(curl2cfile) <- c(noncache_curl, cache_curl)
  }

  pkgs_raw <- lapply(X = names(curl2cfile),
                     FUN = function(curl) {
                       cfile <- curl2cfile[[curl]]
                       if (file.exists(cfile) && !.is_cache_too_old(cfile)) {
                         # try to read it from cache
                         pkgs <- tryCatch({
                           readRDS(cfile)
                         },
                         error = function(e) data.frame())

                         if (nrow(pkgs) > 0) {
                           return(pkgs)
                         }
                       }

                       # really retrieve it
                       res <- tryCatch({
                         if (nchar(cfile) > 0) {
                           pkg_logdebug("Trying to cache availables from %s ...", curl)

                           # remove R cache
                           r_cache_file <- file.path(tempdir(), paste0("repos_", utils::URLencode(curl, TRUE), ".rds"))
                           unlink(r_cache_file, force = TRUE)
                         }

                         pkgs <- suppressWarnings({
                           utils::available.packages(contriburl = curl, filters = list())
                         })
                         pkgs <- data.frame(pkgs, stringsAsFactors = FALSE, row.names = NULL)

                         if (nrow(pkgs) == 0) {
                           return(NULL)
                         }
                         if (nchar(cfile) > 0) {
                           # cache it for future
                           try({
                             saveRDS(pkgs, file = cfile)
                             pkg_logdebug("Availables from %s cached.", curl)
                           },
                           silent = TRUE)
                         }
                         pkgs
                       },
                       error = function(e) NULL)

                       return(res)
                     })
  pkgs_raw <- pkgs_raw[!is.null(pkgs_raw)]
  if (length(pkgs_raw) == 0) {
    return(NULL)
  }
  pkgs <- do.call("rbind", pkgs_raw)
  return(pkgs)
}

#'
#' Checks if cache file is too old.
#'
#' @param fpath cache file path to check.
#' @return FALSE if cache is still useable
#'
#' @keywords internal
#' @noRd
#'
.is_cache_too_old <- function(fpath) {
  if (!file.exists(fpath)) {
    return(TRUE)
  }
  mtime <- file.mtime(fpath)
  age_days <- as.double(difftime(Sys.time(), mtime, units = "days"))
  return(age_days < 0.0 || age_days >= 7.0)
}

#'
#' Removes available packages cache noth RSuite and R.
#'
#' @param path path to repository. url with schema expected (type: character).
#' @param type to retrieve packages for
#' @param rver R version to retrieve packages for.
#'
#' @keywords internal
#' @noRd
#'
clear_available_packages_cache <- function(path, type, rver) {
  stopifnot(regexpr("^(https?|ftp|file)(://.*)", path) != -1) # url path expected
  contrib_url <- rsuite_contrib_url(path, type, rver)

  # remove R cache
  r_cache_file <- file.path(tempdir(), paste0("repos_", utils::URLencode(contrib_url, TRUE), ".rds"))
  unlink(r_cache_file, force = TRUE)

  repos_cache_dir <- get_cache_dir("repos_cache") # from 98_shell.R
  if (!is.null(repos_cache_dir)
      && !grepl("^file://", # repository is not local
                path)) {
    cache_file <- file.path(repos_cache_dir, paste0(utils::URLencode(contrib_url, TRUE), ".rds"))
    unlink(cache_file, force = TRUE)
  }
}
