#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Installs oracle instant client for tests.
#----------------------------------------------------------------------------

installOracleInstantClient <- function() {
  if (.Platform$OS.type == "windows") {
    .win.installOracleInstantClient()
  } else {
    .unx.installOracleInstantClient()
  }
}

.win.installOracleInstantClient <- function() {
  if (all(c("OCI_INC", "OCI_LIB32", "OCI_LIB64") %in% names(Sys.getenv()))) {
    message("ROracle initialized already")
    return(invisible())
  }

  base_dir <- file.path(get_data_dir(), "instantclient")
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = T, showWarnings = F)
    base_dir <- normalizePath(base_dir)

    message(sprintf("Installing Oracle instant client from S3 into %s ...", base_dir))

    void <- lapply(
      X = c("instantclient-basic-nt-12.1.0.2.0.zip",
            "instantclient-basic-windows.x64-12.1.0.2.0.zip",
            "instantclient-sdk-windows.x64-12.1.0.2.0.zip"),
      FUN = function(fname) {
        loc_fpath <- file.path(base_dir, fname)
        if (!file.exists(loc_fpath)) {
          dload_res <- RSuite:::run_rscript("download.file('http://wlog-testdata.s3.amazonaws.com/RSuite/%s', %s)",
                                            fname, RSuite:::rscript_arg("destfile", loc_fpath))
          stopifnot(is.null(dload_res))
        }
        exdir <- file.path(base_dir, sub("^(.+)-[0-9\\.]+zip$", replacement = "\\1", x = fname))
        unzip_res <- RSuite:::run_rscript("unzip(%s, %s)",
                                          RSuite:::rscript_arg("zipfile", loc_fpath),
                                          RSuite:::rscript_arg("exdir", exdir))
        stopifnot(is.null(unzip_res))
      })

    message(sprintf("Installing Oracle instant client from S3 into %s ... done", base_dir))
  } else {
    base_dir <- normalizePath(base_dir)
    message(sprintf("Oracle instant client found at %s", base_dir))
  }

  message("Preparing ROracle environment ...")

  build_win_path <- function(...) { normalizePath(file.path(...)) }
  Sys.setenv(PATH = paste(build_win_path(base_dir, "instantclient-basic-nt", "instantclient_12_1"),
                          build_win_path(base_dir, "instantclient-basic-windows.x64", "instantclient_12_1"),
                          Sys.getenv("PATH"),
                          sep = .Platform$path.sep),
             OCI_LIB32 = build_win_path(base_dir, "instantclient-basic-nt", "instantclient_12_1"),
             OCI_LIB64 = build_win_path(base_dir, "instantclient-basic-windows.x64", "instantclient_12_1"),
             OCI_INC = build_win_path(base_dir, "instantclient-sdk-windows.x64", "instantclient_12_1", "sdk", "include"))

  message("Preparing ROracle environment ... done")
  invisible(base_dir)
}

.unx.installOracleInstantClient <- function() {
  if (all(c("OCI_INC", "OCI_LIB") %in% names(Sys.getenv()))) {
    message("ROracle initialized already")
    return(invisible())
  }

  base_dir <- file.path(get_data_dir(), "instantclient")
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = T, showWarnings = F)
    base_dir <- normalizePath(base_dir)

    message(sprintf("Installing Oracle instant client from S3 into %s ...", base_dir))

    void <- lapply(
      X = c("instantclient-basic-linux.x64-12.2.0.1.0.zip",
            "instantclient-sdk-linux.x64-12.2.0.1.0.zip"),
      FUN = function(fname) {
        loc_fpath <- file.path(base_dir, fname)
        if (!file.exists(loc_fpath)) {
          dload_res <- RSuite:::run_rscript("download.file('http://wlog-testdata.s3.amazonaws.com/RSuite/%s', %s)",
                                            fname, RSuite:::rscript_arg("destfile", loc_fpath))
          stopifnot(is.null(dload_res))
        }
        exdir <- file.path(base_dir, sub("^(.+)-[0-9\\.]+zip$", replacement = "\\1", x = fname))
        unzip_res <- RSuite:::run_rscript("unzip(%s, %s)",
                                          RSuite:::rscript_arg("zipfile", loc_fpath),
                                          RSuite:::rscript_arg("exdir", exdir))
        stopifnot(is.null(unzip_res))
      })

    # link libclntsh.so.12.1 to libclntsh.so
    libclntsh_dir <- file.path(base_dir, "instantclient-basic-linux.x64", "instantclient_12_2")
    file.link(from = file.path(libclntsh_dir, "libclntsh.so.12.1"),
              to = file.path(libclntsh_dir, "libclntsh.so"))

    message(sprintf("Installing Oracle instant client from S3 into %s ... done", base_dir))
  } else {
    base_dir <- normalizePath(base_dir)
    message(sprintf("Oracle instant client found at %s", base_dir))
  }

  message("Preparing ROracle environment ...")

  Sys.setenv(LD_LIBRARY_PATH = paste(file.path(base_dir, "instantclient-basic-linux.x64", "instantclient_12_2"),
                                     Sys.getenv("LD_LIBRARY_PATH"),
                                     sep = .Platform$path.sep),
             OCI_LIB = file.path(base_dir, "instantclient-basic-linux.x64", "instantclient_12_2"),
             OCI_INC = file.path(base_dir, "instantclient-sdk-linux.x64", "instantclient_12_2", "sdk", "include"))


  message("Preparing ROracle environment ... done")
  invisible(base_dir)
}
