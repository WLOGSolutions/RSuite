#!/bin/bash

base_dir=$(dirname $0)
base_dir=`realpath ${base_dir}`

basever=`cat ${base_dir}/../version.txt`
gittag=`git tag | tail -n 1`
if [ -z "$gittag" ]; then
	echo "ERROR: Failed to detect git tag"
	exit 1
fi
ver=${basever}.${gittag}

build_dir=${base_dir}/rsuitecli
rm -rf ${build_dir}
mkdir -p ${build_dir}

cp -R ${base_dir}/../R ${build_dir}
cp ${base_dir}/../rsuite ${build_dir}/rsuite
echo -n $ver > ${build_dir}/version.txt
chmod +x ${build_dir}/rsuite
chmod -x ${build_dir}/R/*.R

mkdir -p ${base_dir}/zips

pushd ${base_dir} > /dev/null
zip -r zips/rsuitecli-${ver}.zip ${build_dir}
popd > /dev/null
