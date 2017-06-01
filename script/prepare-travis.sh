if ! [[ "$TRAVIS_PULL_REQUEST" == "false" ]]; then
  git checkout -B "pr-$TRAVIS_PULL_REQUEST"
elif [[ "$(git rev-parse --abbrev-ref HEAD)" == "HEAD" ]]; then
  git checkout -B "$TRAVIS_BRANCH"
fi