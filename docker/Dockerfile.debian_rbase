ARG rver=3.3

FROM rocker/r-ver:${rver}

LABEL name="R${rver} build under base Debian 9.2 (stretch)"
LABEL maintainer="WLOG Solutions <info@wlogsolutions.com>"

RUN apt-get update \
	&& apt-get install -y \
        libxml2-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        zlib1g-dev \
		supervisor \
        vim \
        sudo \
        net-tools \
        wget \
        curl \
        git \
        subversion \
    && echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" > /usr/local/lib/R/etc/Rprofile.site
            
WORKDIR /opt
CMD [ "supervisord", "-n" ]
