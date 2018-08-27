#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities related to managing information about packages project, in file and
#  pkgzips.
#----------------------------------------------------------------------------

#'
#' Retrieves list of project packages from package folder in order to be
#' able to solve internal dependencies.
#'
#' @param pkgs_path path to folder containing project packages
#'
#' @return named vector of project package names. Names are package folders. (type: character).
#'
#' @keywords internal
#' @noRd
#'
build_project_pkgslist <- function(pkgs_path) {
  pkg_dirs <- Filter(x = list.dirs(pkgs_path, full.names = FALSE, recursive = FALSE),
                     f = function(d) file.exists(file.path(pkgs_path, d, "DESCRIPTION")))
  if (!length(pkg_dirs)) {
    return()
  }

  dir2deps <- list()
  pkg_names <- vapply(X = pkg_dirs,
                      FUN = function(pkg_dir) desc_retrieve_name(pkgs_path, pkg_dir),
                      FUN.VALUE = "")

  for (pkg_dir in pkg_dirs) {
    pkg_deps <- unlist(strsplit(desc_retrieve_dependencies(pkgs_path, pkg_dir), split = ","))
    pkg_deps <- trimws(gsub(x = pkg_deps, pattern = "(.*)(\\(.*\\))", replacement = "\\1"))
    pkg_deps <- pkg_deps[pkg_deps %in% pkg_names]
    dir2deps[[pkg_dir]] <- pkg_deps
  }
  stopifnot(length(pkg_dirs) == length(pkg_names))

  selection <- vapply(X = pkg_dirs,
                      FUN = function(pd) length(dir2deps[[pd]]) == 0,
                      FUN.VALUE = TRUE)
  ordered_dirs <- pkg_dirs[selection]
  ordered_names <- pkg_names[selection]

  pkg_dirs <- setdiff(pkg_dirs, ordered_dirs)
  pkg_names <- setdiff(pkg_names, ordered_names)
  while (length(pkg_dirs) > 0) {
    selection <- vapply(X = pkg_dirs,
                        FUN = function(pd) all(dir2deps[[pd]] %in% ordered_names),
                        FUN.VALUE = TRUE)
    ordered_dirs <- c(ordered_dirs, pkg_dirs[selection])
    ordered_names <- c(ordered_names, pkg_names[selection])

    pkg_dirs <- pkg_dirs[!selection]
    pkg_names <- pkg_names[!selection]
  }

  names(ordered_names) <- ordered_dirs
  return(ordered_names)
}

#'
#' Retrieves projects packages together with their versions.
#'
#' Packages are retrieved in order to be able to solve internal dependencies.
#'
#' @param pkgs_path path to folder containing project packages
#' @return named list with package names and package versions (type: list).
#'
#' @keywords internal
#' @noRd
#'
retrieve_project_pkgsvers <- function(pkgs_path) {
  project_packages <- Filter(x = list.dirs(pkgs_path,
                                           full.names = FALSE,
                                           recursive = FALSE),
                             f = function(n) file.exists(file.path(pkgs_path, n, "DESCRIPTION")))
  result <- list()
  for (pkg in project_packages) {
    descr <- read.dcf(file.path(pkgs_path, pkg, "DESCRIPTION"))
    result[[pkg]] <- as.character(descr[1, "Version"])
  }
  return(result)
}


#'
#' Updates versions of packages.
#'
#' @param pkgs_path path to folder containing project packages
#' @param pkgsvers named list with package names as names and version to set as
#'   values.
#'
#' @return named list with package names and package versions (type: list).
#'
#' @keywords internal
#' @noRd
#'
update_project_pkgsvers <- function(pkgs_path, pkgsvers) {
  project_packages <- Filter(x = list.dirs(pkgs_path,
                                           full.names = FALSE,
                                           recursive = FALSE),
                             f = function(n) file.exists(file.path(pkgs_path, n, "DESCRIPTION")))
  for (pkg in project_packages) {
    if (!(pkg %in% names(pkgsvers))) {
      next
    }

    desc_file <- file.path(pkgs_path, pkg, "DESCRIPTION")
    descr <- read.dcf(desc_file)
    descr[, "Version"] <- pkgsvers[[pkg]]
    write.dcf(descr, file = desc_file)
  }
}

#'
#' Retrieves dependencies from package description file.
#'
#' @param pkgs_path path to folder containing project packages.
#' @param pkg_dir package folder to retrieve description from.
#' @param fields fields to search dependencies in.
#'   (type: character, default: c("Imports", "Depends", "LinkingTo"))
#'
#' @return comma separated character will all unparsed dependency declarations.
#'
#' @keywords internal
#' @noRd
#'
desc_retrieve_dependencies <- function(pkgs_path, pkg_dir, fields = c("Imports", "Depends", "LinkingTo")) {
  stopifnot(length(fields) > 0)

  desc_file <- file.path(pkgs_path, pkg_dir, "DESCRIPTION")
  stopifnot(file.exists(desc_file))

  desc <- read.dcf(desc_file, fields = fields)
  return(dcf_retrieve_dependencies(desc, fields))
}

#'
#' Retrieves dependencies from package DCF read from description file.
#'
#' @param dcf DCF read from description file.
#' @param fields fields to search dependencies in.
#'   (type: character, default: c("Imports", "Depends", "LinkingTo"))
#'
#' @return comma separated character will all unparsed dependency declarations.
#'
#' @keywords internal
#' @noRd
#'
dcf_retrieve_dependencies <- function(dcf, fields = c("Imports", "Depends", "LinkingTo")) {
  pkgs <- unname(unlist(dcf[1, intersect(fields, colnames(dcf))]))
  pkgs <- trimws(gsub(pattern = "\\n", replacement = "", x = pkgs))
  pkgs <- pkgs[!is.na(pkgs) & nchar(pkgs) > 0]

  return(paste(pkgs, collapse = ", "))
}

#'
#' Retrieves name of package from package description file.
#'
#' @param pkgs_path path to folder containing project packages.
#' @param pkg_dir package folder to retrieve description from.
#'
#' @return name of package retrieved. (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
desc_retrieve_name <- function(pkgs_path, pkg_dir) {
  read.dcf(file.path(pkgs_path, pkg_dir, "DESCRIPTION"))[1, "Package"]
}

#'
#' Backups DESCRIPTION files or project packages.
#'
#' @param pkgs_path path to folder containing project packages.
#'
#' @return object sutable to restore DESCRIPTION files.
#'
#' @keywords internal
#' @noRd
#'
backup_pkgdesc_files <- function(pkgs_path) {
  bkp_dir <- tempfile(pattern = "pkgdescs_")
  success <- dir.create(bkp_dir)
  assert(success, "Failed to create folder to backup DESCRIPTION files of project packages")

  project_packages <- Filter(x = list.dirs(pkgs_path,
                                           full.names = FALSE,
                                           recursive = FALSE),
                             f = function(n) file.exists(file.path(pkgs_path, n, "DESCRIPTION")))
  for (pkg in project_packages) {
    success <- file.copy(from = file.path(pkgs_path, pkg, "DESCRIPTION"),
                         to = file.path(bkp_dir, pkg),
                         copy.date = TRUE)
    assert(success, "Failed to backup DESCRIPTION file of %s", pkg)
  }

  result <- list(bkp_dir = bkp_dir, pkgs_path = pkgs_path)
  class(result) <- "pkgdesc_backup"
  return(result)
}

#'
#' Restores backuped DESCRIPTION files of project packages.
#'
#' @param bkp object returned by backup_pkgdesc_files
#'
#' @keywords internal
#' @noRd
#'
restore_pkgdesc_files <- function(bkp) {
  stopifnot(class(bkp) == "pkgdesc_backup")

  project_packages <- Filter(x = list.dirs(bkp$pkgs_path,
                                           full.names = FALSE,
                                           recursive = FALSE),
                             f = function(n) file.exists(file.path(bkp$pkgs_path, n, "DESCRIPTION")))
  for (pkg in project_packages) {
    bkp_file <- file.path(bkp$bkp_dir, pkg)
    if (file.exists(bkp_file)) {
      success <- file.copy(from = bkp_file, to = file.path(bkp$pkgs_path, pkg, "DESCRIPTION"),
                           overwrite = TRUE, copy.date = TRUE)
      assert(success, "Failed to restore DESCRIPTION file of %s", pkg)
    }
  }

  unlink(bkp$bkp_dir, recursive = TRUE, force = TRUE)
}


#'
#' Retrieves information on file packages passed.
#'
#' @param files file packages to retrieve info for. (type: character)
#' @return data.table of following structure
#' \describe{
#'   \item{Package}{Name of package. (type: character)}
#'   \item{Version}{Package version. (type: character)}
#'   \item{Type}{Type of package: source, win.binary, mac.binary. (type: character)}
#'   \item{RVersion}{Version package was built for or NA if it is source package. (type: character)}
#'   \item{File}{File path. (type: character)}
#' }
#'
#' @keywords internal
#' @noRd
#'
get_package_files_info <- function(files) {
  assert(!missing(files) && is.character(files) && length(files) > 0,
         "Expected character(N) for files")
  assert(all(file.exists(files)),
         "Files do not exist: %s", paste(files[!file.exists(files)], collapse = ", "))

  get_pkg_file_info <- function(file) {
    abs_file <- normalizePath(file)
    pkg_name <- gsub("^([^_]+)_.+$", "\\1", basename(abs_file))
    dcf <- get_pkg_desc(pkg_name, abs_file)

    if (!("Built" %in% colnames(dcf))) {
      pkg_type <- "source"
      rver <- NA
    } else {
      pkg_type <- ifelse(grepl("; windows$", dcf$Built), "win.binary", "mac.binary")
      rver <- majmin_rver(gsub("^R ([^;]+);.+$", "\\1", dcf$Built))
    }

    dcf[, setdiff(c("Depends", "Imports", "LinkingTo"), colnames(dcf))] <- NA
    data.frame(Package = pkg_name, Version = dcf$Version,
               Depends = dcf$Depends, Imports = dcf$Imports, LinkingTo = dcf$LinkingTo,
               Type = pkg_type, RVersion = rver,
               Repository = path2local_url(dirname(abs_file)), # from 99_rpatches.R
               File = basename(file),
               Path = abs_file,
               stringsAsFactors = FALSE)
  }

  db <- do.call("rbind", lapply(X = files, FUN = get_pkg_file_info))
  return(db)
}

#'
#' Retrieves packages description.
#'
#' @param pkg_name name of the package. (type: character)
#' @param path package file path or folder of the package. (type: character)
#'
#' @return data.frame with single row as read.dcf exposes.
#'
#' @keywords internal
#' @noRd
#'
get_pkg_desc <- function(pkg_name, path) {
  if (dir.exists(path)) {
    dcf <- tryCatch({
      data.frame(read.dcf(file.path(path, "DESCRIPTION")), stringsAsFactors = FALSE)[1, ]
    },
    error = function(e) NULL)
  } else if (grepl("[.]zip$", path)) {
    con <- unz(path, file.path(pkg_name, "DESCRIPTION"))
    dcf <- tryCatch({
      data.frame(read.dcf(con), stringsAsFactors = FALSE)[1, ]
    },
    error = function(e) NULL,
    finally = {
      close(con)
    })
  } else {
    tmp_dir <- tempfile(pattern = "pkg_info")
    dir.create(tmp_dir)
    dcf <- tryCatch({
      utils::untar(path, files = file.path(pkg_name, "DESCRIPTION"), exdir = tmp_dir)
      data.frame(read.dcf(file.path(tmp_dir, pkg_name, "DESCRIPTION")), stringsAsFactors = FALSE)[1, ]
    },
    error = function(e) NULL,
    finally = {
      unlink(tmp_dir, recursive = TRUE, force = TRUE)
    })
  }

  assert(!is.null(dcf), "Failed to read DESCRIPTION from package: %s", path)
  return(dcf)
}

#'
#' Retrieves information on packages in PKGZIP passed.
#'
#' @param pkgzip pkgipz to retrieve information from. (type: character)
#'
#' @return data.table of following structure
#' \describe{
#'   \item{Package}{Name of package. (type: character)}
#'   \item{Version}{Package version. (type: character)}
#'   \item{Type}{Type of package: source, win.binary, mac.binary. (type: character)}
#'   \item{RVersion}{Version package was built for or NA if it is source package. (type: character)}
#'   \item{File}{File path inside archive. (type: character)}
#' }
#'
#' @keywords internal
#' @noRd
#'
get_pkgzip_info <- function(pkgzip) {
  assert(is_nonempty_char1(pkgzip), "Non empty character(1) expected for pkgzip")
  assert(file.exists(pkgzip), "File %s does not exist", pkgzip)

  files <- utils::unzip(pkgzip, list = TRUE)$Name
  files <- files[grepl("^.+/[^_]+_[^/]+[.](zip|t(ar[.])?gz)$", files)]
  dummy_urls <- sprintf("http://repo/%s", files)

  infos <- get_package_url_infos(dummy_urls)
  infos$File <- files
  return(infos[, c("Package", "Version", "Type", "RVersion", "File")])
}

#'
#' Fills up File column in available packages accoring to Package name, Version
#'   and Repository it found in.
#'
#' Does not change File attribute if it found already.
#'
#' @param avail_pkgs data.frame of the same structure as available.packages
#'   returns. (type: data.frame)
#'
#' @return data.frame with File column fullfilled.
#'
#' @keywords internal
#' @noRd
#'
deduce_package_files <- function(avail_pkgs) {
  stopifnot(is.data.frame(avail_pkgs) &&
              all(c("Package", "Version", "Repository", "File") %in% colnames(avail_pkgs)))

  no_file <- avail_pkgs[is.na(avail_pkgs$File), ]
  if (nrow(no_file) > 0) {
    avail_pkgs[is.na(avail_pkgs$File), ]$File <- paste0(
      no_file$Package, "_", no_file$Version,
      ifelse(grepl("[\\/]bin[\\/]windows[\\/]contrib[\\/]", no_file$Repository), ".zip",
             ifelse(grepl("[\\/]bin[\\/]macosx[\\/]([^\\/]+[\\/])?contrib[\\/]", no_file$Repository), ".tgz",
                    ".tar.gz")))
  }
  return(avail_pkgs)
}

#'
#' Detects information on packages based on their urls inside repository.
#'
#' @param urls package urls inside repository (type: character)
#'
#' @return data.table of following structure
#' \describe{
#'   \item{Package}{Name of package. (type: character)}
#'   \item{Version}{Package version. (type: character)}
#'   \item{Type}{Type of package: source, win.binary, mac.binary. (type: character)}
#'   \item{RVersion}{Version package was built for or NA if it is source package. (type: character)}
#'   \item{Url}{Package url inside repository. (type: character)}
#' }
#'
#' @keywords internal
#' @noRd
#'
get_package_url_infos <- function(urls) {
  stopifnot(is.character(urls) && length(urls) > 0)

  files <- sub("^.+/((src|bin)/.+)$", "\\1", urls)
  types <- ifelse(grepl("^src/contrib/[^/]+[.]tar[.]gz$", files), "source",
                  ifelse(grepl("^bin/windows/contrib/.+[.]zip$", files), "win.binary",
                         ifelse(grepl("^bin/macosx/([^/]+/)?contrib/.+$", files), "mac.binary",
                                "binary")))
  rvers <- ifelse(grepl("^src/.+$", files), NA,
                  gsub("bin/[^/]+/([^/]+/)?contrib/([^/]+)/.+$", "\\2", files))
  return(data.frame(
    Package = gsub("^.+/([^_]+)_[^/]+$", "\\1", files),
    Version = gsub("^.+/[^_]+_([^/]+)[.](zip|t(ar[.])?gz)$", "\\1", files),
    Type = types,
    RVersion = rvers,
    Url = urls,
    stringsAsFactors = FALSE
  ))
}

#'
#' Return list of dependency types respected for req_type
#'
#' @param req_type package type required (type: character)
#' @param bin_type platform binary package type (type: character)
#'
#' @return character vector with respected dependency types for req_type.
#'
#' @keywords internal
#' @noRd
#'
get_respected_types <- function(req_type, bin_type) {
  if (req_type == "source") {
    return("source")
  }
  return(c(bin_type, "source")) # source packages can always be build
}
