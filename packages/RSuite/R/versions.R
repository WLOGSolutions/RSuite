#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for managing versions objects which represent package with version
#   requirements.
#
#----------------------------------------------------------------------------

#'
#' @keywords internal
#'
#' Normalizes passed versions: 1.2.3 is converted to 001.002.003. Normalized
#' versions can be compared in lexical order. Conversion is vectorized.
#'
#' @param ver version which can contain blocks of digits separeded with any non
#' digit character (type: character).
#'
#' @return vector of normalized versions. (type: character)
#'
norm_version <- function(ver) {
  ver <- as.character(ver)

  ver <- gsub('(?<!\\d)(\\d)(?!\\d)', '0\\1', ver, perl = T)
  ver <- gsub('(?<!\\d)(\\d\\d)(?!\\d)', '0\\1', ver, perl = T)
  ver <- gsub('^(\\d+)$', '\\1.000.000', ver, perl = T)
  ver <- gsub('^(\\d+\\D\\d+)$', '\\1.000', ver, perl = T)
  return(ver)
}

#'
#' @keywords internal
#'
#' Denormalizes passed versions: 001.002.000 is converted to 1.2.
#'
#' It's reverse operation to norm_version. Denormalization is vectorized.
#'
#' @param ver version which can contain blocks of digits separeded with any non
#' digit character (type: character).
#'
#' @return vector of denormalized versions. (type: character)
#'
denorm_version <- function(ver) {
  ver <- as.character(ver)
  ver <- gsub("(?<!\\d)0+([1-9]\\d*)(?!\\d)", "\\1", ver, perl = T)
  ver <- gsub("(?<!\\d)0{3}(?!\\d)", "0", ver, perl = T)
  ver <- gsub("\\D0$", "", ver, perl = T)
  ver <- gsub("\\D0$", "", ver, perl = T)
  return(ver)
}

.standard_avail_columns <- function() {
  return(c("Package", "Version", "Depends", "Imports", "LinkingTo", "Repository", "File"))
}

#'
#' @keywords internal
#'
#' Builds version object containing names with specified reqiurements
#'
#' @param pkg_names vector of package names (type: character)
#' @param vmin minimal version acceptable or NA if not limited (type: character)
#' @param vmax maximal version acceptable or NA if not limited (type: character)
#' @param avails data.frame in form available.packages return or NULL.
#' 
#' @return version object which contains passed packages with requirements
#'
versions.build <- function(pkg_names = c(), vmin = NA, vmax = NA, avails = NULL) {
  versions.diff <- function(ver1, ver2) {
    stopifnot(is.versions(ver1))
    stopifnot(is.versions(ver2))
    stopifnot(ver1$has_avails() || ver2$has_avails())

    ver_diff <- merge(x = ver1$pkgs, y = ver2$pkgs, by.x = "pkg", by.y = "pkg", all.x = T)

    missing <- ver_diff[(is.na(ver_diff$vmin.y) & is.na(ver_diff$vmax.y))
                        | (!is.na(ver_diff$vmin.y) & (!is.na(ver_diff$vmax.x) & ver_diff$vmax.x < ver_diff$vmin.y))
                        | (!is.na(ver_diff$vmax.y) & (!is.na(ver_diff$vmin.x) & ver_diff$vmin.x > ver_diff$vmax.y)), ]
    missing$vmin <- missing$vmin.x
    missing$vmax <- missing$vmax.x

    ver_diff <- ver_diff[!(ver_diff$pkg %in% missing$pkg), ]
    ver_diff$vmin <- ifelse(is.na(ver_diff$vmin.x) | (!is.na(ver_diff$vmin.y) & ver_diff$vmin.x < ver_diff$vmin.y),
                            ver_diff$vmin.y,
                            ver_diff$vmin.x)
    ver_diff$vmax <- ifelse(is.na(ver_diff$vmax.x) | (!is.na(ver_diff$vmax.y) & ver_diff$vmax.x > ver_diff$vmax.y),
                            ver_diff$vmax.y,
                            ver_diff$vmax.x)
    return(build.pkgSearchResults(
      found = .df2ver(ver_diff, avails = rbind(ver1$get_avails(), ver2$get_avails())),
      missing = .df2ver(missing)
    ))
  }

  versions.get_colliding <- function(ver) {
    stopifnot(is.versions(ver))
    colliding <- ver$pkgs$pkg[!is.na(ver$pkgs$vmin) & !is.na(ver$pkgs$vmax) & ver$pkgs$vmin > ver$pkgs$vmax]
    return(colliding)
  }

  versions.get <- function(ver, pkg_names) {
    stopifnot(is.versions(ver))
    return(ver$pkgs[ver$pkgs$pkg %in% pkg_names, ])
  }

  versions.rm <- function(ver, pkg_names) {
    stopifnot(is.versions(ver))
    if (ver$has_avails()) {
      avails <- ver$get_avails()
      avails <- avails[!(avails$Package %in% pkg_names), ]
    } else {
      avails <- NULL
    }
    return(.df2ver(ver$pkgs[!(ver$pkgs$pkg %in% pkg_names), ], avails))
  }

  versions.rm_acceptable <- function(ver, pkgs) {
    stopifnot(is.versions(ver))
    stopifnot(is.data.frame(pkgs) && all(c("Package", "Version") %in% colnames(pkgs)))
    pkgs <- pkgs[, c("Package", "Version")]

    pkgs$Version <- norm_version(pkgs$Version)
    pkgs <- pkgs[order(pkgs$Package, pkgs$Version, decreasing = T), ]
    pkgs <- pkgs[!duplicated(pkgs$Package),]

    res_pkgs <- merge(x = ver$pkgs, y = pkgs, by.x = "pkg", by.y = "Package", all.x = T)
    res_pkgs <- res_pkgs[
      is.na(res_pkgs$Version) # not present: unaceptable
      | (!is.na(res_pkgs$vmin) & res_pkgs$vmin > res_pkgs$Version)    # Version is less then required: unacceptable
      | (!is.na(res_pkgs$vmax) & res_pkgs$vmax < res_pkgs$Version), ] # Version is greater than required: unacceptable
    return(.df2ver(res_pkgs, ver$get_avails()))
  }

  versions.pick_available_pkgs <- function(ver) {
    stopifnot(is.versions(ver))
    stopifnot(ver$has_avails())

    avail_ver <- ver$get_avails()
    avail_ver <- avail_ver[order(avail_ver$Package, avail_ver$NVersion, decreasing = T), ]
    avail_ver <- avail_ver[!duplicated(avail_ver$Package),]

    return(avail_ver)
  }

  versions.get_names <- function(ver) {
    stopifnot(is.versions(ver))
    return(ver$pkgs$pkg)
  }

  versions.is_empty <- function(ver) {
    stopifnot(is.versions(ver))
    return(nrow(ver$pkgs) == 0)
  }

  .df2ver <- function(df, avails = NULL) {
    stopifnot(is.data.frame(df) && all(c('pkg', 'vmin', 'vmax') %in% colnames(df)))
    df <- df[, c('pkg', 'vmin', 'vmax')]
    
    if (!is.null(avails)) {
      stopifnot(is.data.frame(avails) 
                && all(.standard_avail_columns() %in% colnames(avails)))
      avails <- avails[avails$Package %in% df$pkg, ]
      avails <- avails[!duplicated(avails[, c('Package', 'Version')]), ]
      
      stopifnot(all(df$pkg %in% avails$Package                                   # present in avails
                    | (!is.na(df$vmin) & !is.na(df$vmax) && df$vmin > df$vmax))) # or infeasible
      
      avails$NVersion <- norm_version(avails$Version)
      avails <- merge(x = avails, y = df, by.x = "Package", by.y = "pkg", all.x = F, all.y = T)
      
      avails <- avails[!is.na(avails$NVersion)
                       & (is.na(avails$vmin) | avails$vmin <= avails$NVersion)
                       & (is.na(avails$vmax) | avails$vmax >= avails$NVersion), ]
      avails <- avails[, c(.standard_avail_columns(), 'NVersion')]
    }
    
    res <- list(pkgs = df,
                avails = avails,
                .df2ver = .df2ver)
    class(res) <- "versions"

    res$diff <- function(oth_ver) { versions.diff(res, oth_ver); }
    res$get_colliding <- function() { versions.get_colliding(res); }
    res$get <- function(pkg_names) { versions.get(res, pkg_names); }
    res$rm <- function(pkg_names) { versions.rm(res, pkg_names); }
    res$rm_acceptable <- function(pkgs) { versions.rm_acceptable(res, pkgs); }
    res$pick_available_pkgs <- function() { versions.pick_available_pkgs(res); }
    res$get_names <- function() { versions.get_names(res); }
    res$is_empty <- function() { versions.is_empty(res); }
    res$get_avails <- function() { avails; }
    res$has_avails <- function() { !is.null(avails); }
    res$rm_avails <- function() { .df2ver(df) }
    return(res)
  }

  if (!is.null(avails)) {
    stopifnot(is.data.frame(avails))
    if (nrow(avails) == 0) {
      col_names <- .standard_avail_columns()
      avails <- data.frame(
        matrix(character(), nrow = 0, ncol = length(col_names), dimnames = list(c(), col_names)),
        stringsAsFactors = F)
    } else {
      stopifnot(all(.standard_avail_columns() %in% colnames(avails)))
      avails <- avails[, .standard_avail_columns()]
    }
  }
  if (!length(pkg_names)) {
    return(.df2ver(data.frame(pkg = as.character(), vmin = as.character(), vmax = as.character(), stringsAsFactors = F),
                   avails = avails))
  }
  return(.df2ver(data.frame(pkg = pkg_names,
                            vmin = norm_version(vmin),
                            vmax = norm_version(vmax),
                            stringsAsFactors = F),
                 avails = avails))
}

#'
#' @keywords internal
#'
#' Collects requirements out of data frame passed or available at contrib url packages.
#'
#' @param pkgs data.frame containing at least columns Package, Version, Repository and File
#' @param contrib_urls if pkgs is NULL will be used to retrieve available packages from the urls.
#' 
#' @return version object wich contains all packages with vmin and vmax set to
#'   same version value together with available packages data.frame.
#'
versions.collect <- function(contrib_urls, pkgs = NULL) {
  if (is.null(pkgs)) {
    stopifnot(!missing(contrib_urls))

    # retrieves all duplicates because no filtering applied
    all_pkgs <- utils::available.packages(contrib_urls, filters = list())
    pkgs <- as.data.frame(all_pkgs, stringsAsFactors = F)[, .standard_avail_columns()]
  } else {
    stopifnot(is.data.frame(pkgs) && all(.standard_avail_columns() %in% colnames(pkgs)))
  }

  if (!nrow(pkgs)) {
    return(versions.build(avails = data.frame()))
  }

  n_vers <- norm_version(pkgs$Version)
  vmins <- aggregate(n_vers, by = list(pkg = pkgs$Package), FUN = function(...) min(as.character(...)))
  colnames(vmins) <- c('pkg', 'vmin')
  vmaxs <- aggregate(n_vers, by = list(pkg = pkgs$Package), FUN = function(...) max(as.character(...)))
  colnames(vmaxs) <- c('pkg', 'vmax')
  vers <- merge(x = vmins, y = vmaxs, by.x = "pkg", by.y = "pkg")

  return(versions.build()$.df2ver(vers, pkgs))
}

#'
#' @keywords internal
#'
#' Joins version objects the way to make version requirements more restrictive
#'
#' @param ... version objects to union
#' @return version object containing all packages with maximal vmin
#'   and minimal vmax if they occure multiple times.
#'
versions.union <- function(...) {
  vers <- list(...)
  if (!length(vers)) {
    return(versions.build())
  }

  ver1 <- vers[[1]]
  stopifnot(is.versions(ver1))
  for(i in 1:length(vers)) {
    ver2 <- vers[[i]]
    stopifnot(is.versions(ver2))
    stopifnot(ver1$has_avails() == ver2$has_avails())
    
    df <- merge(x = ver1$pkgs, y = ver2$pkgs, by.x = "pkg", by.y = "pkg", all.x = T, all.y = T)
    df$vmin <- ifelse(!is.na(df$vmin.x) & (is.na(df$vmin.y) | df$vmin.x > df$vmin.y), df$vmin.x, df$vmin.y)
    df$vmax <- ifelse(!is.na(df$vmax.x) & (is.na(df$vmax.y) | df$vmax.x < df$vmax.y), df$vmax.x, df$vmax.y)
    
    ver1 <- ver1$.df2ver(df, avails = rbind(ver1$get_avails(), ver2$get_avails()))
  }

  return(ver1)
}

versions.get_available_pkgs <- function(pkgs, contrib_urls) {
  vers <- versions.collect(contrib_urls)
  vers <- vers$rm(setdiff(vers$get_names(), pkgs))
  return(vers$pick_available_pkgs())
}

#'
#' @keywords internal
#'
#' Check if object is version object.
#'
#' @param ver object to check
#' @return TRUE if version object.
#'
is.versions <- function(ver) {
  return(class(ver) == "versions")
}
