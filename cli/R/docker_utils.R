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
  platform_id_dict <- list(ubuntu = "deb", centos = "rpm")
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
get_docker_base_image <- function(prj, platform) {
  get_platform_id(platform) # just to check if platform is valid

  params <- prj$load_params()

  sprintf("wlog/rsuite:%s_r%s", platform, RSuite:::majmin_rver(params$r_ver))
}

#'
#' Builds image id with latest RSuite & RSuite CLI to build project for the platform.
#'
get_docker_image <- function(prj, platform) {
  sprintf("%s_v%s",
          get_docker_base_image(prj, platform),
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

  result <- tryCatch({
    while(TRUE) {
      ln <- readLines(con, n = 1, skipNul = T)
      if (!length(ln)) {
        stop(sprintf("%s failed.", cmd_desc))
      }
      if (ln == success_code) {
        break
      }
      logdebug("> %s", ln)
    }
  },
  finally = close(con))
}

#'
#' Runs docker container from specified image.
#'
#' @return docker container name
#'
run_container <- function(docker_image) {
  cont_name <- basename(tempfile("cont_"))
  loginfo("Starting container %s based on %s image ...", cont_name, docker_image)
  exec_docker_cmd(c("run", "--name", cont_name, "-d", docker_image),
                  sprintf("Staring container %s based on %s", cont_name, docker_image))
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
