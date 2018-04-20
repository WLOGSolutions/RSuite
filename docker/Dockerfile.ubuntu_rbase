FROM ubuntu:14.04

ARG rver=3.3
ENV rver ${rver:-3.3}

LABEL name="R${rver} build under base Ubuntu 14.04 (trusty)"
LABEL maintainer="WLOG Solutions <info@wlogsolutions.com>"

RUN apt-get update \
	&& apt-get install -y \
		curl wget python-setuptools unzip zip git subversion \
		make gcc g++ gfortran \
		libcairo2-dev libcurl4-openssl-dev libtiff5-dev libicu-dev libzip2 openjdk-7-jdk \
		texinfo texlive-latex-base \
        libxml2-dev zlib1g-dev libbz2-dev \
		supervisor \
	&& export CPPFLAGS="$CPPFLAGS -fPIC" \
	&& cd /tmp \
	# installing R itself
	&& rsrc_pack=`curl -s https://cran.r-project.org/src/base/R-3/ | grep -e "href=\"R-$rver.[0-9].tar.gz\"" | sed -e "s/^.\+href=\"\(R-.\+[.]tar[.]gz\)\".\+$/\1/" | sort | tail -n 1` \
	&& echo "Installing R from https://cran.r-project.org/src/base/R-3/$rsrc_pack" \
	&& wget https://cran.r-project.org/src/base/R-3/$rsrc_pack \
    && tar xzf $rsrc_pack \
    && cd `echo $rsrc_pack | sed -e "s/[.]tar[.]gz$//"` \
    && mkdir -p builddir && cd builddir \
    && ../configure '--with-cairo' '--with-jpeglib' '--with-readline=no' '--with-tcltk' '--with-blas' '--with-lapack' '--enable-R-profiling' '--enable-R-shlib' '--enable-memory-profiling' '--with-x=no' \
    && make \
    && make install \
    # clean up
    && cd /tmp && rm -rf * 

WORKDIR /opt
CMD [ "supervisord", "-n" ]
