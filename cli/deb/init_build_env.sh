#!/bin/bash

echo "Updating apt-get ..."
apt-get update
echo "Installing required packages ..."
apt-get install -y subversion devscripts

src_dir=/opt/RSuite
if [ -d ${src_dir} ]; then
	svn update ${src_dir}
else
	echo "Checking out sources into ${src_dir}..."
	svn co http://192.168.1.110:8080/svn/Projekty/Infrastructure/trunk/R ${src_dir}
fi
