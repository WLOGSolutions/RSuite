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

build_dir=${base_dir}/build
rm -rf ${build_dir}
orig_dir=${build_dir}/rsuitecli-${ver}
orig_file=rsuitecli_${ver}.orig.tar.gz
mkdir -p ${orig_dir}

cp -R ${base_dir}/../R ${orig_dir}
cp ${base_dir}/../rsuite ${orig_dir}/rsuite.sh
echo -e "#!/bin/bash\n/usr/share/rsuitecli/rsuite.sh \$*\n" > ${orig_dir}/rsuite
echo -n $ver > ${orig_dir}/version.txt
chmod +x ${orig_dir}/{rsuite,rsuite.sh}
chmod -x ${orig_dir}/R/*.R

pushd ${build_dir} > /dev/null
tar czf ${orig_file} $(basename ${orig_dir})
popd > /dev/null

cp -R ${base_dir}/debian ${orig_dir}
head -n 1 ${base_dir}/debian/changelog | sed -e "s/([^-]\+\(-[^)]\+\))/(${ver}\\1)/" > ${orig_dir}/debian/changelog
tail -n +2 ${base_dir}/debian/changelog >> ${orig_dir}/debian/changelog

chmod -x ${orig_dir}/debian/{changelog,compat,control,copyright,rules}
chmod +x ${orig_dir}/debian/rules

pushd ${orig_dir} > /dev/null
debuild -us -uc 
popd > /dev/null

mkdir -p ${base_dir}/debs
mv ${build_dir}/rsuitecli_${ver}*.deb ${base_dir}/debs/

