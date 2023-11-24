#! /bin/bash
tag="X"
tagnum="0"

tags=( $(herbstclient tag_status) )

checkuse() {
    if [[ "${tags[$1]}" == [-]* ]] ; then
        herbstclient use "${tags[$1]:1}" # cutting off first char (.#:!)
        exit 0
    fi
}

# get names of active tags
for ((i=0; i<="${#tags[@]}"; i++)); do
    [[ "${tags[i]}" == "#"* ]] && activetag="$i"
done

# if $tag not focused, focus it
if [[ ${activetag} != ${tagnum} ]]; then
    herbstclient use $tag
    exit 0
else
    # otherwise, cycle through tags to find last used tag and exit
    for ((i="$((activetag-1))"; i>=0; i--)) do
        checkuse "$i"
    done
    for ((i=${#tags[@]}-1; i>$((activetag-1)); i--)) do
        checkuse "$i"
    done
    for ((i="$((activetag+1))"; i<"${#tags[@]}"; i++)); do
        checkuse "$i"
    done
    for ((i=0; i<"$activetag"; i++)); do
        checkuse "$i"
    done
fi
