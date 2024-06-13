#! /usr/bin/env -S nix shell 'nixpkgs#rsync' 'nixpkgs#gnutar' --command bash
# shellcheck shell=bash

set -euox pipefail

RELEASE_CANDIDATE_BRANCH=$(buildkite-agent meta-data get "release-candidate-branch")
echo "RELEASE_CANDIDATE_BRANCH=$RELEASE_CANDIDATE_BRANCH"

VERSION=$(buildkite-agent meta-data get "release-version")
echo "VERSION=$VERSION"


buildkite-agent artifact \
    download "result/linux/cardano-wallet-$VERSION-linux64.tar.gz" "."


tar xvzf "result/linux/cardano-wallet-$VERSION-linux64.tar.gz"

cd test/e2e

TESTS_NODE_DB="$(pwd)/state/node_db"
export TESTS_NODE_DB

mkdir -p "$TESTS_NODE_DB"/preprod
rsync -a "$NODE_STATE_DIR"/ "$TESTS_NODE_DB"/preprod

git fetch --all



git checkout "$RELEASE_CANDIDATE_BRANCH"

CARDANO_NODE_CONFIGS="$(pwd)/../../configs/cardano"
export CARDANO_NODE_CONFIGS

tmpfile=$(mktemp /tmp/node-preprod.XXXXXX)

CARDANO_NODE_SOCKET_PATH="$tmpfile"
export CARDANO_NODE_SOCKET_PATH


nix-shell --run "./run_all_tests.sh"

rm "$tmpfile"