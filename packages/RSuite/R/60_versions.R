#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for managing versions objects which represent package with version
#   requirements.
#
#----------------------------------------------------------------------------

#'
#' Normalizes passed versions: 1.2.3 is converted to 001.002.003.
#'
#' Normalized versions can be compared in lexical order. The conversion is vectorized.
#'
#' @param ver version which can contain blocks of digits separeded with any non
#' digit character (type: character).
#'
#' @return vector of normalized versions. (type: character)
#'
#' @keywords internal
#' @noRd
#'
norm_version <- function(ver) {
  ver <- as.character(ver)

  ver <- gsub("(?<!\\d)(\\d)(?!\\d)", "0\\1", ver, perl = TRUE)
  ver <- gsub("(?<!\\d)(\\d\\d)(?!\\d)", "0\\1", ver, perl = TRUE)
  ver <- gsub("^(\\d+)$", "\\1.000.000", ver, perl = TRUE)
  ver <- gsub("^(\\d+\\D\\d+)$", "\\1.000", ver, perl = TRUE)
  ver <- gsub("[-_]", ".", ver, perl = TRUE)
  return(ver)
}

#'
#' Denormalizes passed versions: 001.002.000 is converted to 1.2.
#'
#' It's reverse operation to norm_version. The conversion is vectorized.
#'
#' @param ver version which can contain blocks of digits separeded with any non
#' digit character (type: character).
#'
#' @return vector of denormalized versions. (type: character)
#'
#' @keywords internal
#' @noRd
#'
denorm_version <- function(ver) {
  ver <- as.character(ver)
  ver <- gsub("(?<!\\d)0+([1-9]\\d*)(?!\\d)", "\\1", ver, perl = TRUE)
  ver <- gsub("(?<!\\d)0{3}(?!\\d)", "0", ver, perl = TRUE)
  ver <- gsub("\\D0$", "", ver, perl = TRUE)
  ver <- gsub("\\D0$", "", ver, perl = TRUE)
  return(ver)
}

#' Retrieves version numbers from the input version string e.g. 1.2.0
#' returns  c(1, 2, 0)
#'
#' @param vers list of versions which can contain blocks of digits separeded with a dot
#' or dash character (type: character).
#'
#' @return list of versions digit vectors (type: list)
#'
get_version_numbers <- function(vers) {
  return(strsplit(vers, split = "[.-]"))
}

get_closest_version <- function(ver, type) {
  ver_digits <- as.integer(get_version_numbers(ver)[[1]])
  ver_digits_len <- length(ver_digits)

  assert(ver_digits_len <= 4, "Incorrect version format: %s", ver)

  if (ver_digits_len == 3 && type == "next") {
    result <- paste(c(ver_digits, "1"), collapse = ".")
    return(result)
  }

  carry <- ifelse(type == "prev", -1, 1)
  lower_limit <- -1
  upper_limit <- 10000

  while (ver_digits_len != 0) {
    ver_digits[ver_digits_len] <- ver_digits[ver_digits_len] + carry

    if (ver_digits_len < 4) {
      upper_limit <- 10
    }

    if (ver_digits[ver_digits_len] == lower_limit || ver_digits[ver_digits_len] == upper_limit) {
      ver_digits[ver_digits_len] <- 0
    } else {
      break
    }

    ver_digits_len <- ver_digits_len - 1
  }

  result <- paste(ver_digits, collapse = ".")
  return(result)

}

#'
#' Returns standard columns expected in data.frame returned from available.packages.
#'
#' @return vector of character.
#'
#' @keywords internal
#' @noRd
#'
.standard_avail_columns <- function() {
  return(c("Package", "Version", "Depends", "Imports", "LinkingTo", "Repository", "File"))
}


#'
#' Builds versions object based on requirements specification and optional avails.
#'
#' @param df data.frame with at least pkg, vmin and vmax columns.
#' @param avails if not NULL should be data.frame of same structure as
#'    available.packages returns.
#'
#' @return created versions object.
#'
#' @keywords internal
#' @noRd
#'
.df2ver <- function(df, avails = NULL) {
  stopifnot(is.data.frame(df) && all(c("pkg", "vmin", "vmax") %in% colnames(df)))
  df <- df[, c("pkg", "vmin", "vmax")]


  if (!is.null(avails)) {
    stopifnot(is.data.frame(avails)
              && all(.standard_avail_columns() %in% colnames(avails)))
    avails <- avails[avails$Package %in% df$pkg, ]
    avails <- avails[!duplicated(avails[, c("Package", "Version")]), ]

    stopifnot(all( (df$pkg %in% avails$Package)                                # present in avails
                   | (!is.na(df$vmin) & !is.na(df$vmax) & df$vmin > df$vmax))) # or infeasible

    avails$NVersion <- norm_version(avails$Version)
    avails <- merge(x = avails, y = df, by.x = "Package", by.y = "pkg", all.x = FALSE, all.y = TRUE)

    avails <- avails[!is.na(avails$NVersion)
                     & (is.na(avails$vmin) | avails$vmin <= avails$NVersion)
                     & (is.na(avails$vmax) | avails$vmax >= avails$NVersion), ]
    avails <- avails[, c(.standard_avail_columns(), "NVersion")]
  }

  res <- list(pkgs = df, avails = avails)
  class(res) <- "versions"

  res$get_avails <- function() res$avails
  res$has_avails <- function() !is.null(res$avails)
  return(res)
}

#'
#' Custom print implementation for versions
#'
#' @param ver version object to print.
#'
#' @keywords internal
#' @noRd
#'
print.versions <- function(ver) {
  cat("--- reqs --\n")
  print(ver$pkgs)
  cat("--- avails --\n")
  print(ver$avails)
}


#'
#' Builds version object containing names with specified requirements.
#'
#' version object can have information on available packages or it can contain
#' just bare version requirements.
#'
#' @param pkg_names vector of package names (type: character)
#' @param vmin minimal version acceptable or NA if not limited (type: character)
#' @param vmax maximal version acceptable or NA if not limited (type: character)
#' @param avails data.frame in form available.packages return or NULL.
#'
#' @return version object which contains passed packages with requirements
#'
#' @keywords internal
#' @noRd
#'
vers.build <- function(pkg_names = c(), vmin = NA, vmax = NA, avails = NULL) {
  if (!is.null(avails)) {
    stopifnot(is.data.frame(avails))
    if (nrow(avails) == 0) {
      col_names <- .standard_avail_columns()
      avails <- data.frame(
        matrix(character(), nrow = 0, ncol = length(col_names), dimnames = list(c(), col_names)),
        stringsAsFactors = FALSE)
    } else {
      stopifnot(all(.standard_avail_columns() %in% colnames(avails)))
      avails <- avails[, .standard_avail_columns()]
    }
  }
  if (!length(pkg_names)) {
    return(.df2ver(data.frame(pkg = as.character(), vmin = as.character(), vmax = as.character(),
                              stringsAsFactors = FALSE),
                   avails = avails))
  }
  return(.df2ver(data.frame(pkg = pkg_names,
                            vmin = norm_version(vmin),
                            vmax = norm_version(vmax),
                            stringsAsFactors = FALSE),
                 avails = avails))
}

#'
#' Checks one version object requirements against the other.
#'
#' At least one of input objects should have avails to check if packages are
#'   available.
#' Result object states as found packages vers object satisfying common requirements
#'   of both input objects and common avails. Common requirements
#'   intersection is calculated for them. missings will contain packages from vers
#'   which common requirements are infeasible of cannot be satisfied by common avails.
#'
#' @param ver version object to check.
#' @param oth version object to check against.
#' @return check_res object.
#'
#' @keywords internal
#' @noRd
#'
vers.check_against <- function(ver, oth, extra_reqs = NULL) {
  stopifnot(is.versions(ver))
  stopifnot(is.versions(oth))
  stopifnot(ver$has_avails() || oth$has_avails())

  ver_diff <- merge(x = ver$pkgs, y = oth$pkgs, by.x = "pkg", by.y = "pkg", all.x = TRUE)
  ver_diff$vmin <- ifelse(!is.na(ver_diff$vmin.x) & (is.na(ver_diff$vmin.y) | ver_diff$vmin.x > ver_diff$vmin.y),
                          ver_diff$vmin.x,
                          ver_diff$vmin.y)
  ver_diff$vmax <- ifelse(!is.na(ver_diff$vmax.x) & (is.na(ver_diff$vmax.y) | ver_diff$vmax.x < ver_diff$vmax.y),
                          ver_diff$vmax.x,
                          ver_diff$vmax.y)

  avails <- rbind(ver$get_avails(), oth$get_avails())

  # test against non availability
  nonavail <- ver_diff[!(ver_diff$pkg %in% avails$Package), ]
  ver_diff <- ver_diff[!(ver_diff$pkg %in% nonavail$pkg), ]

  # test against version availability and version requirements
  test_ver <- .df2ver(ver_diff, avails = avails)
  test_ver <- vers.filter_sub_deps(test_ver, extra_reqs)

  nonavail <- rbind(nonavail, ver_diff[!(ver_diff$pkg %in% test_ver$get_avails()$Package), ])
  ver_diff <- ver_diff[!(ver_diff$pkg %in% nonavail$pkg), ]

  unfeasibles <- ver_diff[ver_diff$pkg %in% vers.get_unfeasibles(test_ver), ]
  ver_diff <- ver_diff[!(ver_diff$pkg %in% unfeasibles$pkg), ]

  found <- .df2ver(ver_diff, avails = test_ver$get_avails())
  missing <- .df2ver(rbind(nonavail, unfeasibles))

  return(check_res.build(found = found, missing = missing))
}

#'
#' Retrieves requirements for provided packages.
#'
#' @param ver version object to retrieve requirements for.
#' @param pkg_names character vector of names to retrieve requirements for.
#'    (type: character(N))
#'
#' @return data.frame with columns pkg, vmin, vmax
#'
#' @keywords internal
#' @noRd
#'
vers.get <- function(ver, pkg_names) {
  stopifnot(is.versions(ver))
  return(ver$pkgs[ver$pkgs$pkg %in% pkg_names, ])
}

#'
#' Retrieves packages which have unfeasible requiremets.
#'
#' If version object contains avails packages not presented in it will be
#'    reported as infeasibles also. such situation means that requirements cannot
#'    be satisfied by avails.
#'
#' @param ver version object to get unfeasibles from.
#'
#' @return data.frame with columns pkg, vmin and vmax.
#'
#' @keywords internal
#' @noRd
#'
vers.get_unfeasibles <- function(ver) {
  stopifnot(is.versions(ver))
  unfeasibles <- ver$pkgs[!is.na(ver$pkgs$vmin) & !is.na(ver$pkgs$vmax) & ver$pkgs$vmin > ver$pkgs$vmax, ]$pkg
  if (ver$has_avails()) {
    unfeasibles <- c(unfeasibles, ver$pkgs[!(ver$pkgs$pkg %in% ver$get_avails()$Package), ]$pkg)
    unfeasibles <- unique(unfeasibles)
  }
  return(unfeasibles)
}

#'
#' Creates version object containing no packages provided.
#'
#' If avail is present in vers they are also filtered not to contain provided
#'    packages.
#'
#' @param ver version object.
#' @param pkg_names character vector of packages to exclude from versions.
#'    (type: character(N))
#'
#' @return new version object.
#'
#' @keywords internal
#' @noRd
#'
vers.rm <- function(ver, pkg_names) {
  stopifnot(is.versions(ver))
  if (ver$has_avails()) {
    avails <- ver$get_avails()
    avails <- avails[!(avails$Package %in% pkg_names), ]
  } else {
    avails <- NULL
  }
  return(.df2ver(ver$pkgs[!(ver$pkgs$pkg %in% pkg_names), ], avails))
}

#'
#' Creates version object containing only provided packages.
#'
#' If avail is present in vers they are also filtered to contain provided
#'    packages.
#'
#' @param ver version object.
#' @param pkg_names character vector of packages to select from versions.
#'    (type: character(N))
#'
#' @return new version object.
#'
#' @keywords internal
#' @noRd
#'
vers.select <- function(ver, pkg_names) {
  stopifnot(is.versions(ver))
  if (ver$has_avails()) {
    avails <- ver$get_avails()
    avails <- avails[avails$Package %in% pkg_names, ]
  } else {
    avails <- NULL
  }
  return(.df2ver(ver$pkgs[ver$pkgs$pkg %in% pkg_names, ], avails))
}

#'
#' Removes from versions object packages from pkgs which satisfy requirements.
#'
#' @param ver version object to modify.
#' @param pkgs data.frame with at least Package and Version columns to check if
#'   they satisfy ver requirements.
#'
#' @return version object with packages from pkgs which satisfy requirements
#'   removed.
#'
#' @keywords internal
#' @noRd
#'
vers.rm_acceptable <- function(ver, pkgs) {
  stopifnot(is.versions(ver))
  stopifnot(is.data.frame(pkgs) && all(c("Package", "Version") %in% colnames(pkgs)))
  pkgs <- pkgs[, c("Package", "Version")]

  pkgs$Version <- norm_version(pkgs$Version)
  pkgs <- pkgs[order(pkgs$Package, pkgs$Version, decreasing = TRUE), ]
  pkgs <- pkgs[!duplicated(pkgs$Package), ]

  res_pkgs <- merge(x = ver$pkgs, y = pkgs, by.x = "pkg", by.y = "Package", all.x = TRUE)
  res_pkgs <- res_pkgs[
    is.na(res_pkgs$Version) # not present: unaceptable
    | (!is.na(res_pkgs$vmin) & res_pkgs$vmin > res_pkgs$Version)    # Version is less then required: unacceptable
    | (!is.na(res_pkgs$vmax) & res_pkgs$vmax < res_pkgs$Version), ] # Version is greater than required: unacceptable
  return(.df2ver(res_pkgs, ver$get_avails()))
}

#'
#' Removes from versions object packages which are from R base (including R itself).
#'
#' @param ver version object to modify.
#'
#' @return versions objects with base (and R) packages removed.
#'
#' @keywords internal
#' @noRd
#'
vers.rm_base <- function(ver) {
  stopifnot(is.versions(ver))

  base_pkgs <- utils::installed.packages(lib.loc = c(.Library.site, .Library), priority = "base")[, "Package"]
  vers.rm(ver, c(base_pkgs, "R"))
}


#'
#' Creates version object with same requirements but with avails.
#'
#' @param ver version object to modify.
#'
#' @return same version object with avails added.
#'
#' @keywords internal
#' @noRd
#'
vers.add_avails <- function(ver, avails) {
  stopifnot(is.versions(ver))

  return(.df2ver(ver$pkgs, avails))
}


#'
#' Creates version object with same requirements but without avails.
#'
#' @param ver version object to modify.
#'
#' @return same version object with avails removed.
#'
#' @keywords internal
#' @noRd
#'
vers.drop_avails <- function(ver) {
  stopifnot(is.versions(ver))

  return(.df2ver(ver$pkgs))
}

#'
#' From avails stored in vers picks packages with lastest version satisfying
#'   requirments.
#'
#' @param ver version object with avails.
#'
#' @return data.frame of structure same as available.packages returns.
#'
#' @keywords internal
#' @noRd
#'
vers.pick_available_pkgs <- function(ver) {
  stopifnot(is.versions(ver))
  stopifnot(ver$has_avails())

  avail_ver <- ver$get_avails()

  avail_ver <- avail_ver[order(avail_ver$Package, avail_ver$NVersion, decreasing = TRUE), ]
  avail_ver <- avail_ver[!duplicated(avail_ver$Package), ]

  return(avail_ver)
}


#'
#' Retrieve package names from versions object.
#'
#' @param ver version object to retrieve package names from.
#'
#' @return character vector with package names retrieved.
#'
#' @keywords internal
#' @noRd
#'
vers.get_names <- function(ver) {
  stopifnot(is.versions(ver))
  return(ver$pkgs$pkg)
}

#'
#' Checks if version object has any package requirements specified.
#'
#' @param ver version object to check.
#'
#' @return TRUE if no package specified in version object.
#'
#' @keywords internal
#' @noRd
#'
vers.is_empty <- function(ver) {
  stopifnot(is.versions(ver))
  return(nrow(ver$pkgs) == 0)
}

#'
#' Collects requirements out of data frame passed or available at contrib url packages.
#'
#' It retrieves available packages under passed contrib urls and detects lowest and
#' highes version available for each of them.
#'
#' @param pkgs data.frame containing at least columns Package, Version, Repository and File
#' @param contrib_url if pkgs is NULL will be used to retrieve available packages from the url.
#'
#' @return version object wich contains all packages with vmin and vmax set to
#'   same version value together with available packages data.frame.
#'
#' @keywords internal
#' @noRd
#'
vers.collect <- function(contrib_url, pkgs = NULL) {
  if (is.null(pkgs)) {
    stopifnot(!missing(contrib_url))

    all_pkgs <- get_curl_available_packages(contrib_url) # from 53_repositories.R
    if (!is.null(all_pkgs)) {
      pkgs <- all_pkgs[, .standard_avail_columns()]
    } else {
      pkgs <- data.frame()
    }
  } else {
    stopifnot(is.data.frame(pkgs) && all(.standard_avail_columns() %in% colnames(pkgs)))
  }

  if (!nrow(pkgs)) {
    return(vers.build(avails = data.frame()))
  }

  vers <- vers.build(unique(pkgs$Package), avails = pkgs)
  return(vers)
}

#'
#' Joins version objects the way to make version requirements more restrictive
#'
#' Result version object contains all packages with version information satisfying
#' requirements of all the input version objects. If some requiremets cannot be
#' satisfied appropriate packages will have vmin > vmax.
#'
#' @param ... version objects to union
#' @return version object containing all packages with maximal vmin
#'   and minimal vmax if they occure multiple times.
#'
#' @keywords internal
#' @noRd
#'
vers.union <- function(...) {
  vers <- list(...)
  if (!length(vers)) {
    return(vers.build())
  }

  ver1 <- vers[[1]]
  stopifnot(is.versions(ver1))
  for (i in seq_along(vers)) {
    ver2 <- vers[[i]]
    stopifnot(is.versions(ver2))
    stopifnot(ver1$has_avails() == ver2$has_avails())

    df <- merge(x = ver1$pkgs, y = ver2$pkgs, by.x = "pkg", by.y = "pkg", all.x = TRUE, all.y = TRUE)
    df$vmin <- ifelse(!is.na(df$vmin.x) & (is.na(df$vmin.y) | df$vmin.x > df$vmin.y), df$vmin.x, df$vmin.y)
    df$vmax <- ifelse(!is.na(df$vmax.x) & (is.na(df$vmax.y) | df$vmax.x < df$vmax.y), df$vmax.x, df$vmax.y)

    ver1 <- .df2ver(df, avails = rbind(ver1$get_avails(), ver2$get_avails()))
  }

  return(ver1)
}

#'
#' Picks latest available under passed contrib_urls packages from pkg_names.
#'
#' @param pkg_names names of packages to pick (type: character(N))
#' @param contrib_url urls to look for available packages in (type: character(1))
#'
#' @return data.frame of same structure available.packages returns which contains
#'   latest packages among pkg_names found.
#'
#' @keywords internal
#' @noRd
#'
vers.get_available_pkgs <- function(pkg_names, contrib_url) {
  stopifnot(is.character(contrib_url) && length(contrib_url) == 1)
  vers <- vers.collect(contrib_url)
  vers <- vers.rm(vers,
                  pkg_names = setdiff(vers.get_names(vers), pkg_names))
  return(vers.pick_available_pkgs(vers))
}

#'
#' Builds versions object from dependencies with optional version requirements.
#'
#' @param deps character with comma separated list of dependencies. It should be
#'    in form '<dep1>, <dep2> (>= <ver>), ...'. (type: character(1)).
#'
#' @param versions objects containing packages from dependencies with requirements
#'    specified.
#'
#' @keywords internal
#' @noRd
#'
vers.from_deps <- function(deps, pkg_name = NA) {
  stopifnot(is.character(deps) && length(deps) == 1)
  stopifnot(is.na(pkg_name) || is_nonempty_char1(pkg_name))

  parsed_deps <- trimws(unlist(strsplit(deps, split = ",")))
  parsed_deps <- parsed_deps[nchar(parsed_deps) > 0]
  pdfs <- lapply(X = parsed_deps,
                 FUN = function(pdesc) {
                   pdesc <- gsub("\\s+", "", pdesc)
                   ver_op <- sub(pattern = ".+\\(([><=]+).+\\)", replacement = "\\1", x = pdesc)
                   if (ver_op == pdesc) {
                     # no version info
                     return(vers.build(pdesc))
                   }
                   ver <- sub(pattern = ".+\\([><=]+(.+)\\)", replacement = "\\1", x = pdesc)
                   pdesc <- sub(pattern = "(.+)\\(.+\\)", replacement = "\\1", x = pdesc)
                   if (ver_op == "==") {
                     return(vers.build(pdesc, vmin = ver, vmax = ver))
                   }
                   if (ver_op == ">=") {
                     return(vers.build(pdesc, vmin = ver, vmax = NA))
                   }
                   if (ver_op == "<=") {
                     return(vers.build(pdesc, vmin = NA, vmax = ver))
                   }
                   if (ver_op == ">") {
                     ver <- get_closest_version(ver, type = "next")
                     return(vers.build(pdesc, vmin = ver, vmax = NA))
                   }
                   if (ver_op == "<") {
                     ver <- get_closest_version(ver, type = "prev")
                     return(vers.build(pdesc, vmin = NA, vmax = ver))
                   }

                   if (is.na(pkg_name)) {
                     assert(FALSE,
                            "Usupported version specification(%s %s) for %s",
                            ver_op, ver, pdesc)
                   } else {
                     assert(FALSE,
                            "Usupported version specification(%s %s) for %s in dependencies of %s",
                            ver_op, ver, pdesc, pkg_name)
                   }
                 })
  do.call("vers.union", pdfs)
}

#'
#' Builds versions object from dependencies found in avails.
#'
#' @param avails data.frame with at least columns Package, Depends, Imports and
#'   LinkingTo.
#'
#' @param versions objects containing packages from dependencies found in avails
#'    with requirements specified.
#'
#' @keywords internal
#' @noRd
#'
vers.from_deps_in_avails <- function(avails) {
  stopifnot(is.data.frame(avails)
            && all(c("Package", "Depends", "Imports", "LinkingTo") %in% colnames(avails)))
  stopifnot(nrow(avails) > 0)

  pdfs <- by(avails, seq_len(nrow(avails)), FUN = function(deps) {
    pkg_deps <- unname(unlist(deps[, c("Depends", "Imports", "LinkingTo")]))
    pkg_vers <- vers.from_deps(deps = paste(pkg_deps[!is.na(pkg_deps)], collapse = ", "),
                               pkg_name = deps$Package)
    pkg_vers
  })
  do.call("vers.union", pdfs)
}


#'
#' Removes packages from ver avails whose dependencies do not satisfy requirements
#' from ver and extra_reqs.
#'
#' @param ver version object to filter
#'
#' @param extra_reqs additional requirements
#'
#' @keywords internal
#' @noRd
#'
vers.filter_sub_deps <- function(ver, extra_reqs = NULL) {
  stopifnot(is.versions(ver))
  stopifnot(ver$has_avails())
  stopifnot(is.null(extra_reqs) || (is.versions(extra_reqs) && !extra_reqs$has_avails()))

  all_pkgs <- as.data.frame(ver$get_avails())

  if (is.null(extra_reqs)) {
    extra_reqs <- ver
  } else {
    extra_reqs <- vers.union(vers.drop_avails(extra_reqs), vers.drop_avails(ver))
  }


  # Check dependency requirements of available packages
  is_compatible <- by(all_pkgs, seq_len(nrow(all_pkgs)), FUN = function(deps) {
    pkg_deps <- unname(unlist(deps[, c("Depends", "Imports", "LinkingTo")]))
    pkg_vers <- vers.from_deps(deps = paste(pkg_deps[!is.na(pkg_deps)], collapse = ", "),
                               pkg_name = deps$Package)
    pkg_vers$pkgs <- pkg_vers$pkgs[pkg_vers$pkgs$pkg %in% extra_reqs$pkgs$pkg, ] # keep only packages from vers
    dep_req_vers <- vers.union(pkg_vers, vers.drop_avails(extra_reqs))

    length(vers.get_unfeasibles(dep_req_vers)) == 0
  })

  all_pkgs <- all_pkgs[is_compatible, ]
  ver$pkgs <- ver$pkgs[ver$pkgs$pkg %in% all_pkgs$Package, ]
  return(vers.add_avails(ver, all_pkgs))
}


#'
#' Check if object is version object.
#'
#' @param ver object to check
#' @return TRUE if version object.
#'
#' @keywords internal
#' @noRd
#'
is.versions <- function(ver) {
  return(class(ver) == "versions")
}
