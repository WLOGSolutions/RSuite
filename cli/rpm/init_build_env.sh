#!/bin/bash

echo "Installing required packages ..."
yum install -y subversion rpm-build

src_dir=/opt/RSuite
if [ -d ${src_dir} ]; then
	svn update ${src_dir}
else
	echo "Checking out sources into ${src_dir}..."
	svn co http://192.168.0.101:8080/svn/Projekty/Infrastructure/trunk/R ${src_dir}
fi
