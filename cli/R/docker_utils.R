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
  success_code <- basename(tempfile("success_code_"))
  cmd <- paste(c(get_docker_cmd(), args, "2>&1", "&&", "echo", success_code),
               collapse = " ")

  logdebug("running: %s", cmd)
  con <- pipe(cmd, open = "rt")

  tryCatch({
    lines <- c()
    while(TRUE) {
      ln <- readLines(con, n = 1, skipNul = T)
      if (!length(ln)) {
        stop(sprintf("%s failed.", cmd_desc))
      }
      if (ln == success_code) {
        break
      }
      lines <- c(lines, ln)
      logdebug("> %s", ln)
    }
    return(invisible(lines))
  },
  finally = close(con))
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
  exec_docker_cmd(c("run", "--name", cont_name, "-d", docker_image),
                  sprintf("Starting container %s based on %s", cont_name, docker_image))
  loginfo("... done.")
  return(cont_name)
}

#'
#' Runs shell command inside docker container passed..
#'
#' @return docker container name
#'
run_incont_cmd <- function(cont_name, cmd) {
  loginfo("Running command '%s' in %s container ...", cmd, cont_name)
  exec_docker_cmd(c("exec", "-i", cont_name, "sh", "-c", shQuote(cmd)),
                  sprintf("Running command '%s' in %s container", cmd, cont_name))
  loginfo("... done.")
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
#' @return names of RSuite build containers detectected. (type: character(N))
#'
get_rsbuild_names <- function(cont_name = NULL) {
  output <- exec_docker_cmd(c("ps", "-f", "name=rsbuild-"), "Listing RSuite build containers") # from docker_utils.R
  names <- unlist(lapply(strsplit(output[-1], "\\s+"), function(ln) { ln[length(ln)] }))

  if (is.null(cont_name)) {
    return(names)
  }

  if (grepl("^rsbuild[-]", cont_name)) {
    names <- names[names == cont_name]
  } else {
    names <- names[grepl(paste0("^rsbuild[-]", cont_name), names)]
  }

  if (length(names) > 1) {
    stop(sprintf("Name '%s' is ambiguous", cont_name))
  }
  if (length(names) != 1) {
    stop(sprintf("Container '%s' not found", cont_name))
  }
  return(names)
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
