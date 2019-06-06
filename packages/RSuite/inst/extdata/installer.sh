#!/bin/bash

if ! echo "$0" | grep '\.sh$' > /dev/null; then
	echo "Please, run using 'bash', but not '.' or 'source'"
	exit 1
fi

if [ "$SHELL" != "/bin/bash" -a "$SHELL" != "/usr/bin/bash" ]; then
	echo "Please, run under '/bin/bash' shell."
	exit 1
fi

if ! unzip --help > /dev/null 2>&1; then
	echo "'unzip' is required to run installation. Please, install 'unzip' and rerun the script"
	exit 1
fi

THIS_DIR=$(DIRNAME=$(dirname "$0"); cd "$DIRNAME"; pwd)
THIS_FILE=$(basename "$0")
THIS_PATH="$THIS_DIR/$THIS_FILE"

if md5sum --help > /dev/null 2>&1; then
        MD5CMD=md5sum
elif cat "$THIS_PATH" | md5 > /dev/null 2>&1; then
        MD5CMD=md5
else
        echo "Neither 'md5sum' nor 'md5' detected. Please, install one of them and rerun the script"
        exit 1
fi

# some colors
c_inf=$(tput setaf 2)
c_deb=$(tput setaf 6)
c_war=$(tput setaf 3)
c_err=$(tput setaf 1)
c_def=$(tput setaf default)

# cursor moving
cmv_beg=$(tput cub 80) # move to first columnt
cmv_c40=${cmv_beg}$(tput cuf 40) # move 40 columns forward

echo "==> Extracting deployment zip ..."
# create zip to extract
script_end_line=$[ $(cat $THIS_PATH | grep -na "@@END_HEADER@@" | tail -n 1 | sed -e "s/:.*$//") + 1 ]
temp_dir=$(mktemp -d)
cleanup_and_exit() {
	rm -rf $temp_dir
	exit $1
}

zipfile=$temp_dir/${THIS_FILE/.sh/.zip}
cat $THIS_PATH | tail -n +${script_end_line} > $zipfile

# analyse file contents
proj_and_libs=($(unzip -l $zipfile | grep -e "\s[^/]\+/\(libs/[^/]\+/\)\?$" | sed -e 's/^[ \t]*0[ \t]*[: 0-9-]*//'))
proj=${proj_and_libs[0]}
libs=(${proj_and_libs[@]:1})

# first installation
if [ ! -f $proj/readme.txt ]; then
	echo "==> ... installing ${c_inf}${proj}${c_def} ..."
	echo -n "${c_deb}"
	unzip $zipfile -x "${proj}libs/**"; errcode=$?
	echo -n "${c_def}"
	if [ $errcode -gt 0 ]; then
		echo "${c_err}... failed to install base${c_def} (errcode: $errcode)"
		cleanup_and_exit $errcode
	fi
	echo "==> ... installing packages ..."
	echo -n "${c_deb}"
	unzip -oq $zipfile "${proj}libs/**"; errcode=$?
	if [ $errcode -gt 0 ]; then
		echo "${c_err}... failed to install packages${c_def} (errcode: $errcode)"
		cleanup_and_exit $errcode
	fi
	echo -n "${c_def}"
	echo "==> ... All done."
	cleanup_and_exit 0
fi

# upgrade
old_ver=$(cat ${proj}readme.txt | head -n 1 | sed -e "s/^.* \(v.*\)$/\1/")
echo "==> ... updating ${c_inf}$proj${c_def} from ${old_ver} ..."

echo -n "${c_deb}"
unzip -o $zipfile -x "${proj}libs/**"; errcode=$?
echo -n "${c_def}"
if [ $errcode -gt 0 ]; then
	echo "${c_err}... failed while updating base${c_def} (errocode: $errcode)"
	cleanup_and_exit $errcode
fi

echo "==> ... processing packages ..."

safe_echo() {
  echo $1
}
check_update_lib() {
	desc=$(unzip -p $zipfile ${lib}DESCRIPTION | tr -d "\r")
	zip_hash=$(echo "$desc" | $MD5CMD | sed -e 's/[ \t].*$//')
	name=$(echo "$desc" | grep Package: | sed -e 's/^.*:[ \t]*//')
	if [ -f ${proj}libs/${name}/DESCRIPTION ]; then
		old_hash=$(cat ${proj}libs/${name}/DESCRIPTION | tr -d "\r" | $MD5CMD | sed -e 's/[ \t].*$//')
		if [ "${old_hash}" == "${zip_hash}" ]; then
			safe_echo "${c_def}... ${lib} ... ${c_inf}same${c_def}"
			return 0
		fi
		info="${c_war}different${c_def}; updating"
	else
		info="${c_war}none${c_def}; extracting"
	fi

	unzip -oq $zipfile "${lib}**"; errcode=$?
	if [ $errcode -gt 0 ]; then
		safe_echo "${c_def}... ${lib} ... ${info} ... ${c_err}failed${c_def} (errcode: $errcode)"
		return 1
	fi
	safe_echo "${c_def}... ${lib} ... ${info} ... done"
	return 0
}

pids=()
for lib in ${libs[@]}; do
  check_update_lib $lib &
  pids[${#pids[@]}]=$!
done

errors=0
for pid in ${pids[@]}; do
  wait $pid
done

echo "==> ... All done."
cleanup_and_exit 0

@@END_HEADER@@
