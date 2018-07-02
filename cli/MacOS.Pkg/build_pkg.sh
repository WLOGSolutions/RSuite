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
mkdir -p ${build_dir}/rsuitecli/base.pkg ${build_dir}/rsuitecli/Resources/en.lproj

app_dir="${build_dir}/root/Applications/RSuiteCLI ${ver}.app"
mkdir -p "${app_dir}"
cp -R ${base_dir}/../R "${app_dir}"
cp ${base_dir}/../rsuite "${app_dir}/rsuite.sh"
echo -e "#!/bin/bash\n/usr/share/rsuitecli/rsuite.sh \$*\n" > "${app_dir}/rsuite"
cp ${base_dir}/../rsuite-bash-prompt "${app_dir}/rsuite-bash-prompt"
echo -n $ver > "${app_dir}/version.txt"
chmod +x "${app_dir}"/{rsuite,rsuite.sh,rsuite-bash-prompt}
chmod -x "${app_dir}"/R/*.R

pushd ${build_dir} > /dev/null
(cd root && find . | cpio -o --format odc --owner 0:80 | gzip -c ) > rsuitecli/base.pkg/Payload

sizeKBytes=$(du --apparent-size --block-size=1K -s ./root | sed -e "s/^\([0-9]*\).*$/\1/")
filesNo=$(find ./root | wc -l)
cat > rsuitecli/base.pkg/PackageInfo <<EOL
<pkg-info format-version="2" identifier="com.WLOGSolutions.RSuiteCLI.base.pkg" version="${ver}" install-location="/" auth="root">
  <payload installKBytes="${sizeKBytes}" numberOfFiles="${filesNo}"/>
  <bundle-version>
    <bundle id="com.WLOGSolutions.RSuiteCLI" CFBundleIdentifier="com.WLOGSolutions.RSuiteCLI" path="./Applications/RSuiteCLI ${ver}.app" CFBundleVersion="${ver}"/>
  </bundle-version>
</pkg-info>
EOL

mkbom -u 0 -g 80 root rsuitecli/base.pkg/Bom

cat > rsuitecli/Distribution <<EOL
<?xml version="1.0" encoding="utf-8"?>
<installer-script minSpecVersion="1.000000" authoringTool="com.apple.PackageMaker" authoringToolVersion="3.0.3" authoringToolBuild="174">
    <title>RSuiteCLI ${ver}</title>
    <options customize="never" allow-external-scripts="no"/>
    <domains enable_anywhere="true"/>
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
    <choices-outline>
        <line choice="choice1"/>
    </choices-outline>
    <choice id="choice1" title="base">
        <pkg-ref id="com.WLOGSolutions.RSuiteCLI.base.pkg"/>
    </choice>
    <pkg-ref id="com.WLOGSolutions.RSuiteCLI.base.pkg" installKBytes="${sizeKBytes}" version="${ver}" auth="Root">#base.pkg</pkg-ref>
</installer-script>
EOL

( cd rsuitecli && xar --compression none -cf "../RSuiteCLI ${ver} Installer.pkg" * )

popd > /dev/null

mkdir -p ${base_dir}/pkgs
mv ${build_dir}/RSuiteCLI\ ${ver}*.pkg ${base_dir}/pkgs/