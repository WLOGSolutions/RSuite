Windows
=======

MSI is separate for x64 and x86. To build MSI just run WiX/build.cmd

Debian/Ubuntu
=============

Change to deb folder and run build_deb.sh. You will need debscripts package to build deb.
On Windows docker is used to build rpm: change to deb and call create_deb.cmd.
RPM package will be created in debs subfolder.

RedHat/CentOS
=============

Change to rpm folder and run build_rpm.sh.
On Windows docker is used to build rpm: change to rpm and call create_rpm.cmd. 
RPM package will be created in rpms subfolder.
