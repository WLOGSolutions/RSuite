FROM ubuntu

RUN apt-get update \
    && apt-get install -y \
		git supervisor \
		cpio make g++ \
		wget libxml2-dev libssl1.0-dev
    
# install bomutils
RUN git clone https://github.com/hogliux/bomutils /tmp/bomutils \
	&& cd /tmp/bomutils && make && make install \
	&& rm -rf /tmp/bomutils
	
#install xar
RUN cd /tmp \
	&& wget https://github.com/downloads/mackyle/xar/xar-1.6.1.tar.gz \
	&& tar xzf xar-1.6.1.tar.gz && cd /tmp/xar-1.6.1 \
	&& ./configure && make && make install \
	&& rm -rf /tmp/xar-1.6.1*

WORKDIR /opt

CMD [ "supervisord", "-n" ]