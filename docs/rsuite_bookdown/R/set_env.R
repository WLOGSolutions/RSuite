lib_path <- file.path("..", "libs")
sbox_path <- file.path("..", "sbox")
if (!file.exists(lib_path)) {
  lib_path <- file.path("..", "deployment", "libs")
  sbox_path <- file.path("..", "deployment", "sbox")
}

if (!dir.exists(sbox_path)) {
  dir.create(sbox_path, recursive = T)
}

.libPaths(c(normalizePath(sbox_path), normalizePath(lib_path), .libPaths()))

library(logging)
logging::logReset()
logging::setLevel(level = "FINEST")
logging::addHandler(logging::writeToConsole, level = "INFO")

script_path <- getwd()
