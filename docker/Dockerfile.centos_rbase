FROM centos

ARG rver=3.3
ENV rver ${rver:-3.3}


LABEL name="R${rver} build under base CentOS"
LABEL maintainer="WLOG Solutions <info@wlogsolutions.com>"

COPY centos/jpegsrc.v6b.tar.gz /tmp
RUN yum install -y \
        curl wget which python-setuptools unzip zip git subversion \
        make gcc gcc-c++ gcc-gfortran \
        cairo-devel openssl-devel libtiff-devel libicu-devel bzip2-devel java-1.8.0-openjdk-devel \
        texinfo texlive-latex-bin \
        libxml2-devel libcurl-devel zlib-devel \
    && export CPPFLAGS="$CPPFLAGS -fPIC" \
    && cd /tmp \
    # installing libjpeg v6b
    && tar xzf jpegsrc.v6b.tar.gz \
    && cd jpeg-6b && ./configure && make && mkdir -p /usr/local/man/man1 && make install install-lib && cd /tmp \
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
    && cd /tmp && rm -rf * \
    # installing supervisord 
    && easy_install supervisor \
    && mkdir -p /etc/supervisor/conf.d \
    && echo_supervisord_conf > /etc/supervisor/supervisord.conf \
    && echo "[include]" >> /etc/supervisor/supervisord.conf \
    && echo "files=/etc/supervisor/conf.d/*.conf" >> /etc/supervisor/supervisord.conf


WORKDIR /opt
CMD [ "supervisord", "-n", "-c", "/etc/supervisor/supervisord.conf" ]

