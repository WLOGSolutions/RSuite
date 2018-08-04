#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities used by docker command.
#----------------------------------------------------------------------------


#'
#' Retrieves full path to docker command.
#'
#' Raises error if docker command is not available.
#'
get_docker_cmd <- function() {
  docker_cmd <- Sys.which("docker")
  if (docker_cmd == "") {
    stop("No docker command detected.")
  }
  return(docker_cmd)
}


#'
#' Retrieves platform id (rpm/deb) based of platform (ubuntu/centos).
#'
#' Raises error if platform is not supported.
#'
get_platform_id <- function(platform) {
  platform_id_dict <- list(ubuntu = "deb", debian = "deb", centos = "rpm")
  if (!(platform %in% names(platform_id_dict))) {
    stop(sprintf("Platform %s is not currently supported. Supported platforms are %s.",
                 platform, paste(names(platform_id_dict), collapse = " ")))
  }
  return(platform_id_dict[[platform]])
}

#'
#' Builds default R base image id to build project for the platform.
#'
#' Will raise error if platform is not supported.
#'
get_docker_base_image <- function(rver, platform) {
  get_platform_id(platform) # just to check if platform is valid
  sprintf("wlog/rsuite:%s_r%s", platform, RSuite:::majmin_rver(rver))
}

#'
#' Builds image id with latest RSuite & RSuite CLI to build project for the platform.
#'
get_docker_rsuite_image <- function(rver, platform) {
  stopifnot(!is.null(rver))
  sprintf("%s_v%s",
          get_docker_base_image(rver, platform),
          get_latest_release(get_platform_id(platform))$ver)
}


#'
#' Executes docker command passing provided arguments.
#'
#' Command output is logged on DEBUG level.
#' Raises error if command failes.
#'
exec_docker_cmd <- function(args, cmd_desc) {
  .log_single_out <- function(out_arr, desc) {
    lines <- c()
    for (single_out in out_arr) {
      for (line in single_out) {
        if (nchar(line) > 0) {
          logdebug("%s> %s", desc, line)
          lines <- c(lines, line)
        }
      }
    }
    return(lines)
  }

  docker_cmd <- get_docker_cmd()
  logdebug("running: %s", paste(c(docker_cmd, args), collapse = " "))
  con <- subprocess::spawn_process(command = docker_cmd, arguments = args)

  result <- list(
    out_lines = c(),
    err_lines = c(),
    ret_code = NA
  )
  tryCatch({
    while (subprocess::process_state(con) == "running") {
      out_arr <- subprocess::process_read(con, subprocess::PIPE_BOTH, timeout = 3000)

      out_lines <- .log_single_out(out_arr$stdout, "out")
      result$out_lines <- c(result$out_lines, out_lines)

      err_lines <- .log_single_out(out_arr$stderr, "err")
      result$err_lines <- c(result$err_lines, err_lines)
    }
  },
  finally = {
    result$ret_code <- subprocess::process_return_code(con)
    logdebug("return code: %s", result$ret_code)
  })

  return(invisible(result))
}

#'
#' Runs docker container from specified image.
#'
#' @return docker container name
#'
run_container <- function(docker_image, name) {
  if (is.null(name)) {
    cont_name <- basename(tempfile("rsbuild-"))
  } else {
    cont_name <- paste0("rsbuild-", name)
  }
  loginfo("Starting container %s based on %s image ...", cont_name, docker_image)
  output <- exec_docker_cmd(c("run", "--name", cont_name, "-d", docker_image),
                            sprintf("Starting container %s based on %s", cont_name, docker_image))
  if (output$ret_code != 0) {
    stop(sprintf("Failed to start container based on %s image", docker_image))
  }
  loginfo("... done.")
  return(cont_name)
}

#'
#' Runs shell command inside docker container passed..
#'
#' @return invisible command output with errcode.
#'
run_incont_cmd <- function(cont_name, cmd) {
  loginfo("Running command '%s' in %s container ...", cmd, cont_name)
  result <- exec_docker_cmd(c("exec", "-i", cont_name, "sh", "-c", shQuote(cmd)),
                            sprintf("Running command '%s' in %s container", cmd, cont_name))
  loginfo("... done.")
  return(invisible(result))
}

#'
#' Stops docker container passed and removes it completely.
#'
stop_container <- function(cont_name) {
  loginfo("Removing container %s ...", cont_name)
  exec_docker_cmd(c("rm", "-f", cont_name),
                  sprintf("Removing container %s", cont_name))
  loginfo("... done.")
}

#'
#' Gets names of RSuite build containers.
#'
#' @param cont_name container name prefix to find. If NULL will return all RSuite build containers.
#'   If passed should be exactly one container with such name (type: character(1), default: NULL).
#'
#' @return list of infos describing RSuite build containers detectected named by their names.
#' Each info is names list of following content
#' \describe{
#'   \item{base}{Base image of detected container (type: character(1))}
#' }
#'
find_rsbuild_infos <- function(cont_name = NULL) {
  output <- exec_docker_cmd(c("ps", "-f", "name=rsbuild-"), "Listing RSuite build containers") # from docker_utils.R
  infos <- lapply(output$out_lines[-1],
                   function(ln) {
                     toks <- unlist(strsplit(ln, "\\s+"))
                     list(base = toks[[2]])
                   })
  names(infos) <- unlist(lapply(output$out_lines[-1],
                                function(ln) {
                                  toks <- unlist(strsplit(ln, "\\s+"))
                                  toks[length(toks)]
                                }))

  if (is.null(cont_name)) {
    return(infos)
  }

  if (grepl("^rsbuild[-]", cont_name)) {
    infos <- infos[names(infos) == cont_name]
  } else {
    infos <- infos[grepl(paste0("^rsbuild[-]", cont_name), names(infos))]
  }

  if (length(infos) > 1) {
    stop(sprintf("Name '%s' is ambiguous", cont_name))
  }
  if (length(infos) != 1) {
    stop(sprintf("Container '%s' not found", cont_name))
  }
  return(infos)
}

#'
#' Builds Dockerfile out of template replacing RSuite tags
#'
build_dockerfile <- function(templ_fpath, docker_fpath, tags) {
  create_default_dockerfile <- function() {
    writeLines(c(paste0("FROM ", tags$From),
                 tags$DeployProject),
               con = docker_fpath)
  }
  if (is.null(templ_fpath)) {
    create_default_dockerfile()
    return()
  }

  templ_lines <- readLines(templ_fpath, warn = F)
  if (length(templ_lines) == 0) {
    create_default_dockerfile()
    return()
  }

  dfile_lines <- c()
  for(ix in 1:length(templ_lines)) {
    ln <- templ_lines[ix]

    extra_lines <- c()
    for(nm in names(tags)) {
      next_ln <- gsub(paste0("<RSuite:", nm, ">"), tags[[nm]][1], ln)
      if (next_ln != ln) {
        ln <- next_ln
        extra_lines <- c(extra_lines, tags[[nm]][-1])
      }
    }
    dfile_lines <- c(dfile_lines, ln, extra_lines)
  }

  writeLines(dfile_lines, con = docker_fpath)
}

#'
#' Retrieves docker cache file name: <prj>/deployment/docker_cache/<cont_img>/<platform_desc>.zip
#'
build_incont_cache_fpath <- function(cont_name, cont_img, prj) {
  cache_base_dir <- file.path(dirname(prj$load_params()$lib_path), "docker_cache")
  cont_img <- gsub("^([^:]+):.+$", "\\1", cont_img)
  cont_img <- gsub("/", "@", cont_img)

  plat_desc_rcode <- paste('os_info <- RSuite::rsuite_get_os_info();',
                           'cat(c(',
                           'sprintf(\"Distrib: %s\\n\", os_info$distrib),',
                           'sprintf(\"Version: %s\\n\", os_info$version),',
                           'sprintf(\"Arch:    %s\\n\", R.version$arch)))',
                           sep = "")
  plat_desc_res <- run_incont_cmd(cont_name, sprintf("Rscript -e '%s'", plat_desc_rcode))
  cache_fname <- sprintf("%s_%s_%s.zip",
                         gsub("^\\s*Distrib:\\s+(.+)$", "\\1", plat_desc_res$out_lines[[1]]),
                         gsub("^\\s*Version:\\s+(.+)$", "\\1", plat_desc_res$out_lines[[2]]),
                         gsub("^\\s*Arch:\\s+(.+)$", "\\1", plat_desc_res$out_lines[[3]]))
  return(file.path(cache_base_dir, cont_img, cache_fname))
}

#'
#' Retrieves project environment from container and stores it into cache file
#'
cache_incont_env <- function(cont_name, prj_name, cache_fpath) {
  loginfo("Caching project environment into %s ...", cache_fpath)

  if (!dir.exists(dirname(cache_fpath))) {
    dir.create(dirname(cache_fpath), recursive = TRUE, showWarnings = FALSE)
  }

  cache_fname <- basename(cache_fpath)
  run_incont_cmd(cont_name, sprintf("cd %s && zip -qr /tmp/%s ./deployment", prj_name, cache_fname)) # ...
  exec_docker_cmd(c("cp", sprintf("%s:/tmp/%s", cont_name, cache_fname), cache_fpath), # ...
                  "Copying project cache from container")

  loginfo("... done.")
}

#'
#' If cache file exists unzips it into project environment in container.
#'
decache_incont_env <- function(cont_name, prj_name, cache_fpath) {
  cache_fname <- basename(cache_fpath)
  if (!file.exists(cache_fpath)) {
    loginfo("No project cache found.")
    return(invisible(FALSE))
  }

  loginfo("Copying project cache into container (%s) ...", cache_fpath)

  exec_docker_cmd(c("cp", cache_fpath, sprintf("%s:/tmp/", cont_name)), "Copying project cache into container")
  run_incont_cmd(cont_name,
                 sprintf("cd %s && unzip -qu /tmp/%s && rm -f /tmp/%s", prj_name, cache_fname, cache_fname))

  loginfo("... done.")
  return(invisible(TRUE))
}
