#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Package logger setup
#----------------------------------------------------------------------------

.logger_name <- "rsuite"
setLevel("FINEST", logging::getLogger(.logger_name))

pkg_loginfo <- function(msg, ...) { logging::loginfo(msg, ..., logger = getLogger(.logger_name)) }
pkg_logdebug <- function(msg, ...) { logging::logdebug(msg, ..., logger = getLogger(.logger_name)) }
pkg_logerror <- function(msg, ...) { logging::logerror(msg, ..., logger = getLogger(.logger_name)) }
pkg_logwarn <- function(msg, ...) { logging::logwarn(msg, ..., logger = getLogger(.logger_name)) }
pkg_logfinest <- function(msg, ...) { logging::logfinest(msg, ..., logger = getLogger(.logger_name)) }
