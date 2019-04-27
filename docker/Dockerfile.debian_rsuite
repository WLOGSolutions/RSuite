ARG rver=3.3

FROM wlog/rsuite:debian_r${rver}

ARG rsuite_ver
ENV rsuite_ver ${rsuite_ver:-0.23.232}

LABEL name="R${rver} + RSuite($rsuite_ver) + CLI under base Ubuntu"
LABEL maintainer="WLOG Solutions <info@wlogsolutions.com>"

RUN wget http://wlog-rsuite.s3.amazonaws.com/cli/debian/rsuitecli_${rsuite_ver}-1_all.deb \
    && dpkg -i rsuitecli_${rsuite_ver}-1_all.deb \
    && rm -rf rsuitecli_${rsuite_ver}-1_all.deb \
    # installing glue required for v3.5 before installing usethis required for devtools
    && Rscript -e "install.packages('glue')" \
    && rsuite install -v
