%define _buildrootdir %{_builddir}

Name:           rsuitecli
Version:        %{ver}
Release:        1
Summary:        RSuite CLI is command line API to RSuite

Vendor:         WLOG Solution <info@wlogsolutions.com>
License:        MIT
URL:            http://rsuite.io
Group:          Development/Tools
Source:		%{name}-%{version}.tar.gz
BuildArch: 	noarch

Requires(post): info
Requires(preun): info

%description
RSuite CLI wraps functionalities of RSuite so you can create
and manage your R projects, build them or manage your own
package repositories.

%prep
%setup

%build
# Empty section.

%install
mkdir -p %{buildroot}/usr/share/rsuitecli
install -m 755 rsuite.sh %{buildroot}/usr/share/rsuitecli/rsuite.sh
install -m 644 version.txt %{buildroot}/usr/share/rsuitecli/version.txt
install -m 755 rsuite-bash-prompt %{buildroot}/etc/bash_completion.d/rsuite-bash-prompt
cp -R R %{buildroot}/usr/share/rsuitecli/

mkdir -p %{buildroot}/usr/bin
install -m 755 rsuite %{buildroot}/usr/bin/rsuite

%clean

%files
/usr/bin/rsuite
/usr/share/rsuitecli/rsuite.sh
/usr/share/rsuitecli/version.txt
/usr/share/rsuitecli/R/command_mgr.R
/usr/share/rsuitecli/R/command_utils.R
/usr/share/rsuitecli/R/docker_utils.R
/usr/share/rsuitecli/R/cmd_repo.R
/usr/share/rsuitecli/R/cmd_proj.R
/usr/share/rsuitecli/R/cmd_pkgzip.R
/usr/share/rsuitecli/R/cmd_install.R
/usr/share/rsuitecli/R/cmd_update.R
/usr/share/rsuitecli/R/cmd_docker.R
/usr/share/rsuitecli/R/packages/src/contrib/getopt_1.20.0.tar.gz
/usr/share/rsuitecli/R/packages/src/contrib/logging_0.7-103.tar.gz
/usr/share/rsuitecli/R/packages/src/contrib/optparse_1.4.4.tar.gz
/usr/share/rsuitecli/R/packages/src/contrib/PACKAGES
/usr/share/rsuitecli/R/packages/src/contrib/PACKAGES.gz
/etc/bash_completion.d/rsuite-bash-prompt
