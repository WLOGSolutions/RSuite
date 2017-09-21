FROM ubuntu

RUN apt-get update \
    && apt-get install -y git supervisor devscripts debhelper
    
WORKDIR /opt

CMD [ "supervisord", "-n" ]