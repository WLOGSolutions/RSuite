#!/bin/bash

base_dir=$(dirname $0)
base_dir=`realpath ${base_dir}`

basever=`cat ${base_dir}/../version.txt | sed -e "s/[\r\n]//"`
gittag=`git tag | tail -n 1`
if [ -z "$gittag" ]; then
	echo "ERROR: Failed to detect git tag"
	exit 1
fi
ver=${basever}.${gittag}

build_dir=${base_dir}/build
rm -rf ${build_dir} ${base_dir}/pkgs
mkdir ${build_dir}
mkdir -p ${build_dir}/rsuitecli/base.pkg ${build_dir}/rsuitecli/Resources/en.lproj ${build_dir}/scripts

app_dir="${build_dir}/root/Applications/RSuiteCLI.app"
mkdir -p "${app_dir}"
cp -R ${base_dir}/../R "${app_dir}"
cp ${base_dir}/../rsuite "${app_dir}/rsuite"
cp ${base_dir}/../../LICENSE "${app_dir}/LICENSE"
cp ${base_dir}/../rsuite-bash-prompt "${app_dir}/rsuite-bash-prompt"
echo -n $ver > "${app_dir}/version.txt"
chmod +x "${app_dir}"/{rsuite,rsuite-bash-prompt}
chmod -x "${app_dir}"/R/*.R

## create administrative content of the package

pushd ${build_dir} > /dev/null
(cd root && find . | cpio -o --format odc --owner 0:80 | gzip -c ) > rsuitecli/base.pkg/Payload

cat > scripts/postinstall <<EOL
#!/bin/sh

# postinstall script for RSuite CLI
cd /usr/bin

if uname -r | grep '^1[5-9]' >/dev/null; then
  ## 15.0 and higher don't allow messing wiht /usr/bin
  ## so use /usr/local/bin instead
  if [ ! -e /usr/local/bin ]; then
	mkdir -p /usr/local/bin
  fi
  cd /usr/local/bin
fi

# create convenience script to RSuite CLI
rm -rf rsuite
rsuite_sh=/Applications/RSuiteCLI.app/rsuite
cat > rsuite <<EOLsh
#!/bin/bash
if [ ! -e "\$rsuite_sh" ]; then
	echo "RSuite CLI is not available any more."
	exit 1
fi
"\$rsuite_sh" \\\$*
EOLsh
chmod +x rsuite
EOL
chmod +x scripts/postinstall
( cd scripts && find . | cpio -o --format odc --owner 0:80 | gzip -c ) > rsuitecli/base.pkg/Scripts

sizeKBytes=$(du --apparent-size --block-size=1K -s ./root | sed -e "s/^\([0-9]*\).*$/\1/")
filesNo=$(find ./root | wc -l)
cat > rsuitecli/base.pkg/PackageInfo <<EOL
<pkg-info format-version="2" identifier="com.WLOGSolutions.RSuiteCLI.base.pkg" version="${ver}" install-location="/Applications" auth="root">
  <payload installKBytes="${sizeKBytes}" numberOfFiles="${filesNo}"/>
  <scripts>
	<postinstall file="./postinstall"/>
  </scripts>
  <bundle-version>
    <bundle id="com.WLOGSolutions.RSuiteCLI" CFBundleIdentifier="com.WLOGSolutions.RSuiteCLI" path="./RSuiteCLI.app" CFBundleVersion="${ver}"/>
  </bundle-version>
</pkg-info>
EOL

mkbom -u 0 -g 80 root rsuitecli/base.pkg/Bom

cat > rsuitecli/Distribution <<EOL
<?xml version="1.0" encoding="utf-8"?>
<installer-script minSpecVersion="1.000000" authoringTool="com.apple.PackageMaker" authoringToolVersion="3.0.3" authoringToolBuild="174">
    <title>RSuiteCLI ${ver} for Mac OS X 10.11 or higher (El Capitan build)</title>
    <options customize="never" allow-external-scripts="no"/>
    <domains enable_anywhere="true" enable_localSystem="true"/>
    <installation-check script="pm_install_check();"/>
    <script>function pm_install_check() {
  if(!(system.compareVersions(system.version.ProductVersion,'10.5') >= 0)) {
    my.result.title = 'Failure';
    my.result.message = 'You need at least Mac OS X 10.5 to install RSuiteCLI.';
    my.result.type = 'Fatal';
    return false;
  }
  return true;
}
</script>
	<welcome language="en" mime-type="text/html"><![CDATA[
<html lang="en">
<head>
    <meta http-equiv="content-type" content="text/html; charset=iso-8859-1">
    <title>Install my_archive </title>
</head>
<body>
<font face="Helvetica">
<p>
This installer will guide you throu the steps necessary to setup <i>RSuite CLI ${ver}</i> on your machine.
</font>
</ul>
</body>
</html>
]]></welcome>
	<license language="en" mime-type="text/html"><![CDATA[
<html lang="en">                                                                                                                                                                         
<head>                                                                                                                                                                                   
    <meta http-equiv="content-type" content="text/html; charset=iso-8859-1">                                                                                                             
    <title>License agreement</title>                                                                                                                                                   
</head>                                                                                                                                                                                  
<body>
	<font face="Helvetica">
        <p>This software is distributed under terms of Apache License Version 2.0. The terms of this license are in a file LICENSE which you should have received with this software.    
        <p>``Share and Enjoy.''
	</font>
</body>                                                                                                                                                                                  
</html>                                                                                                                                                                                  
]]></license>
    <choices-outline>
        <line choice="rsuiteclibase"/>
    </choices-outline>
    <choice id="rsuiteclibase" title="RSuiteCLI ${ver} Utility" tooltip="RSuiteCLI ${ver} Utility" description="RSuite CLI utility version ${ver} (Platform independent).">
        <pkg-ref id="com.WLOGSolutions.RSuiteCLI.base.pkg"/>
    </choice>
    <pkg-ref id="com.WLOGSolutions.RSuiteCLI.base.pkg" installKBytes="${sizeKBytes}" version="${ver}" auth="Root">#base.pkg</pkg-ref>
</installer-script>
EOL

( cd rsuitecli && xar --compression none -cf "../RSuiteCLI ${ver} Installer.pkg" * )

popd > /dev/null

mkdir -p ${base_dir}/pkgs
mv ${build_dir}/RSuiteCLI\ ${ver}*.pkg ${base_dir}/pkgs/
