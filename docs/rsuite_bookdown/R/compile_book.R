# Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)
script_path <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- dirname(sub("--file=", "", args[grep("--file=", args)]))
  if (!length(script_path)) { return(".") }
  return(normalizePath(script_path))
})()

# Setting .libPaths() to point to libs folder
source(file.path(script_path, "set_env.R"), chdir = T)

config <- load_config()
args <- args_parser()

library(bookdown)

loginfo("--> Pandoc version: %s", rmarkdown::pandoc_version())

setwd(file.path(script_path, "../book_src"))
if (file.exists("_main.Rmd")) {
  file.remove("_main.Rmd")
}

bookdown::render_book(input = "index.Rmd",
                      output_format = "bookdown::gitbook",
                      output_dir = "../book")
