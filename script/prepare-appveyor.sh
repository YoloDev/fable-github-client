if ! [[ "$APPVEYOR_PULL_REQUEST_NUMBER" == "" ]]; then
  git checkout -B "pr-$APPVEYOR_PULL_REQUEST_NUMBER"
else
  git checkout -B "$APPVEYOR_REPO_BRANCH"
fi