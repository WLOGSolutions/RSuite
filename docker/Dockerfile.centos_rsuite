ARG rver=3.3

FROM wlog/rsuite:centos_r${rver}

ARG rsuite_ver
ENV rsuite_ver ${rsuite_ver:-0.23.232}

LABEL name="R${rver} + RSuite($rsuite_ver) + CLI under base CentOS"
LABEL maintainer="WLOG Solutions <info@wlogsolutions.com>"

RUN wget http://wlog-rsuite.s3.amazonaws.com/cli/redhat/rsuitecli-${rsuite_ver}-1.noarch.rpm \
    && rpm -i rsuitecli-${rsuite_ver}-1.noarch.rpm \
    && rm -rf rsuitecli-${rsuite_ver}-1.noarch.rpm \
    && rsuite install -v \
    && yum install -y zip
