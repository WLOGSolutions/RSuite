FROM centos

RUN yum install -y git rpm-build python-setuptools \
    && easy_install supervisor \
    && echo_supervisord_conf > /etc/supervisord.conf
    
WORKDIR /opt

CMD [ "supervisord", "-n" ]