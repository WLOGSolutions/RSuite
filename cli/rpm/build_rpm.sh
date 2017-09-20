#!/bin/bash

base_dir=$(dirname $0)
base_dir=`realpath ${base_dir}`

basever=`cat ${base_dir}/../version.txt`
gittag=`git tag | tail -n 1`
if [ -z "$gittag" ]; then
	echo "Failed to detect git tag"
	exit 1
fi

ver=${basever}.${gittag}

build_dir=${base_dir}/build
rm -rf ${build_dir}

orig_dir=${build_dir}/rsuitecli-${ver}
orig_file=rsuitecli-${ver}.tar.gz

mkdir -p ${orig_dir}
cp -R ${base_dir}/../R ${orig_dir}
cp ${base_dir}/../rsuite ${orig_dir}/rsuite.sh
echo -e "#!/bin/bash\n/usr/share/rsuitecli/rsuite.sh \$*\n" > ${orig_dir}/rsuite
echo -n $ver > ${orig_dir}/version.txt
chmod +x ${orig_dir}/{rsuite.sh,rsuite}
chmod -x ${orig_dir}/R/*.R

mkdir -p ${build_dir}/rpmbuild/{SOURCES,SPECS}
cp rsuitecli.spec ${build_dir}/rpmbuild/SPECS/

pushd ${build_dir} > /dev/null
tar czf ${build_dir}/rpmbuild/SOURCES/${orig_file} $(basename ${orig_dir})
popd > /dev/null

pushd ${build_dir}/rpmbuild/SPECS > /dev/null
rpmbuild -ba "-D ver ${ver}" "-D _topdir ${build_dir}/rpmbuild" rsuitecli.spec
popd > /dev/null

mv ${build_dir}/rpmbuild/RPMS/noarch/rsuitecli-${ver}*.noarch.rpm ${base_dir}
rm -rf ${build_dir}

