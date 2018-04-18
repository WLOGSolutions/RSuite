FROM wlog/rsuite:ubuntu_r3.3_v0.23.232

LABEL name="Jenkins slave under Debian with R3.3 and RSuite"
LABEL maintainer="WLOG Solutions <info@wlogsolutions.com>"

RUN apt-get update \
    && echo "deb http://http.debian.net/debian jessie-backports main" | sudo tee --append /etc/apt/sources.list.d/jessie-backports.list > /dev/null \
    && apt-get update
    
# installing sshd
RUN apt-get install -y openssh-server openssh-client \
    && mkdir -p /var/run/sshd \
    && echo "[program:sshd]" > /etc/supervisor/conf.d/sshd.conf \
    && echo "command=/usr/sbin/sshd -D -e" >> /etc/supervisor/conf.d/sshd.conf \ 
    && echo "autorestart=true" >> /etc/supervisor/conf.d/sshd.conf \
    && echo "startretries=3" >> /etc/supervisor/conf.d/sshd.conf

EXPOSE 22

# installing jenkins requirements
RUN apt-get install -y -t jessie-backports openjdk-8-jdk \
    && useradd -m jenkins \
    && echo "jenkins:WLOGsc2017" | chpasswd \
    && mkdir -p /opt/jenkins \
    && chown jenkins:jenkins /opt/jenkins

# installing test requirements
RUN apt-get install -y libaio1 libaio-dev \
    && echo "options(repos = c(WLOG = 'http://wlog-cran.s3.amazonaws.com', CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" > /usr/local/lib/R/etc/Rprofile.site
    
WORKDIR /opt/jenkins
CMD [ "supervisord", "-n" ]
