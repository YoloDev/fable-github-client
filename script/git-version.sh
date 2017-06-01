#!/bin/bash

########################################
# Helpers
########################################

REGEX_SEMVER="^v([0-9]+)\.([0-9]+)\.([0-9]+)$"
REGEX_BRANCH="^[a-z/]+/(.*)$"
DIR_ROOT="$(git rev-parse --show-toplevel 2> /dev/null)"
CHANGELOG="CHANGELOG.md"

# Use in the the functions: eval $invocation
invocation='say_verbose "Calling: ${yellow:-}${FUNCNAME[0]} ${green:-}$*${normal:-}"'

# standard output may be used as a return value in the functions
# we need a way to write text on the screen in the functions so that
# it won't interfere with the return value.
# Exposing stream 3 as a pipe to standard output of the script itself
exec 3>&1

# Setup some colors to use. These need to work in fairly limited shells, like the Ubuntu Docker container where there are only 8 colors.
# See if stdout is a terminal
if [ -t 1 ]; then
    # see if it supports colors
  ncolors=$(tput colors)
  if [ -n "$ncolors" ] && [ $ncolors -ge 8 ]; then
    bold="$(tput bold       || echo)"
    normal="$(tput sgr0     || echo)"
    black="$(tput setaf 0   || echo)"
    red="$(tput setaf 1     || echo)"
    green="$(tput setaf 2   || echo)"
    yellow="$(tput setaf 3  || echo)"
    blue="$(tput setaf 4    || echo)"
    magenta="$(tput setaf 5 || echo)"
    cyan="$(tput setaf 6    || echo)"
    white="$(tput setaf 7   || echo)"
  fi
fi

function say_err() {
  printf "%b\n" "${red:-}git-version: Error: $1${normal:-}" >&2
}

function say() {
  # using stream 3 (defined in the beginning) to not interfere with stdout of functions
  # which may be used as return value
  printf "%b\n" "${cyan:-}git-version:${normal:-} $1" >&3
}

function say_verbose() {
  if [ "$verbose" = true ]; then
    say "$1"
  fi
}

function say_set() {
  local varname="$1"
  local value="$2"

  say_verbose "${green:-}$varname${normal:-}=${yellow}$value${normal:-}"
}

# Joins elements in an array with a separator
# Takes a separator and array of elements to join
#
# Adapted from code by gniourf_gniourf (http://stackoverflow.com/a/23673883/1819350)
#
# Example
#   $ arr=("red car" "blue bike")
#   $ join " and " "${arr[@]}"
#   red car and blue bike
#   $ join $'\n' "${arr[@]}"
#   red car
#   blue bike
#
function join() {
  local separator=$1
  local elements=$2
  shift 2 || shift $(($#))
  printf "%s" "$elements${@/#/$separator}"
}

# Resolves a path to a real path
# Takes a string path
#
# Example
#   $ echo $(resolve-path "/var/./www/../log/messages.log")
#   /var/log/messages.log
#
function resolve-path() {
  local path="$1"
  if pushd "$path" > /dev/null 2>&1
  then
    path=$(pwd -P)
    popd > /dev/null
  elif [ -L "$path" ]
  then
    path="$(ls -l "$path" | sed 's#.* /#/#g')"
    path="$(resolve-path $(dirname "$path"))/$(basename "$path")"
  fi
  echo "$path"
}

function basename-git() {
  echo $(basename "$1" | tr '-' ' ' | sed 's/.sh$//g')
}

########################################
# Git version functions
########################################
function get-prev-version-tag() {
  eval $invocation

  local tag=$(git describe --tags --abbrev=0 --match="v[0-9]*" 2> /dev/null)
  if ! [[ $? -eq 0 ]]; then
    say_verbose "${magenta:-}No version tags found${normal:-}"
    echo ""
    return
  fi

  until [[ "$tag" =~ $REGEX_SEMVER ]]; do
    tag=$(git describe --tags --abbrev=0 --match="v[0-9]*" "$tag^" 2> /dev/null)
    if ! [[ $? -eq 0 ]]; then
      say_verbose "${magenta:-}No version tags found${normal:-}"
      echo ""
      return
    fi
  done

  echo "$tag"
}

function get-version-from-tag() {
  eval $invocation

  local tag="$1"
  if [[ "$tag" =~ $REGEX_SEMVER ]]; then
    local major=${BASH_REMATCH[1]}
    local minor=${BASH_REMATCH[2]}
    local patch=${BASH_REMATCH[3]}

    echo "$major.$minor.$patch"
    return
  fi

  say_err "$tag is not a valid version"
  exit 1
}

function get-exact-version-tag() {
  eval $invocation

  local tag=$(git describe --exact-match --tags --abbrev=0 --match="v[0-9]*" HEAD 2> /dev/null)
  if [[ $? -eq 0 ]]; then
    if [[ "$tag" =~ $REGEX_SEMVER ]]; then
      local major=${BASH_REMATCH[1]}
      local minor=${BASH_REMATCH[2]}
      local patch=${BASH_REMATCH[3]}

      echo "$major.$minor.$patch"
      return
    fi
  fi

  echo ""
}

function get-next-full-version() {
  eval $invocation

  local tag="$1"
  if [[ "$tag" =~ $REGEX_SEMVER ]]; then
    local major=${BASH_REMATCH[1]}
    local minor=${BASH_REMATCH[2]}
    local patch=${BASH_REMATCH[3]}

    local incrMajor=$(git rev-list --count --grep="Semver: major" $tag..HEAD)
    local incrMinor=$(git rev-list --count --grep="Semver: minor" $tag..HEAD)

    if [[ $incrMajor > 0 ]]; then
      say_verbose "incrementing major"
      major=$(($major + 1))
      minor=0
      patch=0
    elif [[ $incrMinor > 0 ]]; then
      say_verbose "incrementing minor"
      minor=$(($minor + 1))
      patch=0
    else
      patch=$(($patch + 1))
    fi

    echo "$major.$minor.$patch"
    return
  fi

  echo "0.1.0"
}

function get-branch-name() {
  eval $invocation
  
  git rev-parse --abbrev-ref HEAD
}

function get-branch-short-name() {
  eval $invocation

  local branch=$1

  if [[ "$branch" =~ $REGEX_BRANCH ]]; then
    branch=${BASH_REMATCH[1]}
  else
    say_verbose "did not match"
  fi

  echo "$branch"
}

function get-branch-point() {
  eval $invocation

  local branch=$1
  local base=$2
  diff -u <(git rev-list --first-parent "$branch") <(git rev-list --first-parent "$base") | sed -ne 's/^ //p' | head -1
}

function get-commit-count() {
  eval $invocation

  local since="$1"
  if [[ "$since" == "" ]]; then
    git rev-list --count HEAD
  else
    git rev-list --count "$since"..HEAD
  fi
}

function get-git-short-hash() {
  git rev-parse --short HEAD
}

function get-branch-version-meta() {
  eval $invocation

  local lastVersionTag="$1"

  local branchName=$(get-branch-name)
  say_set "branch-name" "$branchName"

  if [[ "$branchName" == "master" ]]; then
    local commitCount=$(get-commit-count "$lastVersionTag")
    say_set "commit-count" "$commitCount"
    echo "-ci.$commitCount"
    return
  fi

  local shortName=$(get-branch-short-name "$branchName")
  say_set "short-name" "$shortName"

  local branchPoint=$(get-branch-point "$branchName" "master")
  say_set "branch-point" "$branchPoint"

  local commitCount=$(get-commit-count "$branchPoint")
  say_set "commit-count" "$commitCount"

  local shortHash=$(get-git-short-hash)
  say_set "short-hash" "$shortHash"

  echo "-$shortName.$commitCount+$shortHash"
}

function get-version() {
  eval $invocation

  local exact=$(get-exact-version-tag)
  if ! [[ "$exact" == "" ]]; then
    say_verbose "Exact version match!"
    echo "$exact"
    return
  fi

  local lastVersionTag=$(get-prev-version-tag)
  say_set "last-version-tag" "$lastVersionTag"
  local nextVersion=$(get-next-full-version "$lastVersionTag")
  say_set "next-full-version" "$nextVersion"
  local branchMeta=$(get-branch-version-meta "$lastVersionTag")
  say_set "branch-meta" "$branchMeta"
  echo "$nextVersion$branchMeta"
}

function release() {
  local version_current_tag=$(get-prev-version-tag)
  local version_current=""
  if ! [[ "$version_current_tag" == "" ]]; then
    version_current=$(get-version-from-tag "$version_current_tag")
  fi

  say_set "prev-version-tag" "$version_current"
  local version_new=$(get-next-full-version "$version_current_tag")
  say_set "new-version" "$version_new"
  local git_root="$DIR_ROOT"
  say_set "git-root" "$git_root"

  local status=0
  local git_origin=$(git config --get remote.origin.url | sed 's#^\([^@]\+@\|https\?://\)\([^:/]\+\)[:/]\([^\.]\+\)\..*$#\2/\3#g')
  local git_compare=https://${git_origin}/compare

  local update_links_desc=()
  local update_links_line=()
  local update_links=0

  local require_link=1
  local version_new_grep="\[${version_new}\]"
  local version_new_actual="[${version_new}]"
  local version_previous=

  # If there is no existing version relax rules slightly
  if [[ "" == "${version_current}" ]]; then
    require_link=0
    version_new_grep="${version_new}"
    version_new_actual="${version_new}"
  fi
  version_new_grep="$(echo "${version_new_grep}" | sed 's#\.#\\.#g')"

  local format="## ${version_new_actual} - $(date +%Y-%m-%d)\n### Added\n- ...\n\nSee http://keepachangelog.com/ for full format."

  if ! git show HEAD:${CHANGELOG} > /dev/null 2>&1; then
    echo "Error: No ${CHANGELOG} file committed at \"${git_root}/${CHANGELOG}\""
    exit 1
  fi

  local file_content=$(git show HEAD:${CHANGELOG})
  local file_versions=$(echo "${file_content}" | grep '^## \[\?[0-9]\+\.[0-9]\+\.[0-9]\]\? - ')

  if ! echo "${file_content}" | grep -q "^## ${version_new_grep} - [0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}"; then
    echo -e "Error: no changelog details found for ${version_new}. Changelog details should be recorded in ${CHANGELOG} with the format:\n"
    echo -e "${format}"
    status=1
    version_previous=$(echo "${file_versions}" | head -n 1 | sed 's/^## \[\?\([0-9]\+\.[0-9]\+\.[0-9]\)\]\?.*$/\1/g')
  else
    version_previous=$(echo "${file_versions}" | head -n 2 | tail -n 1 | sed 's/^## \[\?\([0-9]\+\.[0-9]\+\.[0-9]\)\]\?.*$/\1/g')
  fi

  if ! echo "${file_content}" | grep -q "^\[unreleased\]: ${git_compare}/${version_new}\.\.\.HEAD$"; then
    update_links_desc+=("unreleased")
    update_links_line+=("[unreleased]: ${git_compare}/${version_new}...HEAD")
    update_links=1
  fi

  if [ ${require_link} -eq 1 ] && ! echo "${file_content}" | grep -q "^\[${version_new}\]: ${git_compare}/${version_previous}\.\.\.${version_new}$"; then
    update_links_desc+=("version ${version_new}")
    update_links_line+=("[${version_new}]: ${git_compare}/${version_previous}...${version_new}")
    update_links=1
  fi

  if [ ${update_links} -eq 1 ]; then
    local update_links_descs=$(join " and " "${update_links_desc[@]}")
    local update_links_lines=$(join $'\n' "${update_links_line[@]}")
    local links="link"
    if [[ ${#update_links_desc[@]} -gt 1 ]]; then
      links+="s"
    fi

    echo -e "\nError: no ${links} for ${update_links_descs}. Please add the ${update_links_descs} ${links} at the bottom of the changelog:\n"
    echo -e "${update_links_lines}"
    status=1
  fi

  if [ ${status} -eq 1 ] && git rev-list @{u}... > /dev/null 2>&1 && [ $(git rev-list --left-right @{u}... | grep "^>" | wc -l | sed 's/ //g') -gt 0 ]; then
    echo -e "\nAfter making these changes, you can add your change log to your latest unpublished commit by running:\n\ngit add ${CHANGELOG}\ngit commit --amend -m '$(git log -1 --pretty=%B)'"
  fi

  exit $status
}

verbose=false

if [[ $# -eq 0 ]]; then
  echo "Use sub-command get or release"
  exit 1
fi

while [ $# -ne 0 ]; do
  name=$1
  case $name in
    --verbose)
      verbose=true
      ;;
    
    get)
      get-version
      ;;
    
    release)
      github_changelog_generator
      ;;
    
    release)
      release "$@"
      ;;

    *)
      say_err "Unknown argument \"${red:-}$name${normal:-}\""
  esac
  shift
done