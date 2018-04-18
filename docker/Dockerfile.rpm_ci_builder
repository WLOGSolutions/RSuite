FROM wlog/rsuite:centos_r3.4_v0.23.232

LABEL name="Jenkins slave under CentOS with R3.3 and RSuite"
LABEL maintainer="WLOG Solutions <info@wlogsolutions.com>"

# installing sshd
RUN yum install -y openssh-server openssh-clients \
    && /usr/bin/ssh-keygen -A \
    && echo -e "[program:sshd]\ncommand=/usr/sbin/sshd -D -e\nautorestart=true\nstartretries=3" > /etc/supervisor/conf.d/sshd.conf

EXPOSE 22

# installing jenkins requirements
RUN yum install -y java \
    && useradd jenkins \
    && echo -e "WLOGsc2017\nWLOGsc2017" | passwd jenkins \
    && mkdir -p /opt/jenkins \
    && chown jenkins:jenkins /opt/jenkins

# installing test requirements
RUN yum install -y libaio zip \
    && echo "options(repos = c(WLOG = 'http://wlog-cran.s3.amazonaws.com', CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" > /usr/local/lib64/R/etc/Rprofile.site

WORKDIR /opt/jenkins
CMD [ "supervisord", "-n" ]
