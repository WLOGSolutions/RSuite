#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for .libaths management.
#----------------------------------------------------------------------------

add_extra_to_libpath <- function() {
  extra_lib <- file.path(get_wspace_dir(), basename(tempfile()))
  original_libPaths <- .libPaths()
  
  dir.create(extra_lib, recursive = T)
  .libPaths(c(original_libPaths, extra_lib))
  on_test_exit(function() { 
    .libPaths(original_libPaths)
    unlink(extra_lib, recursive = T, force = T) 
  })
  
  new_libPaths <- .libPaths()
  stopifnot(extra_lib %in% new_libPaths)
  
  return(new_libPaths)  
}

