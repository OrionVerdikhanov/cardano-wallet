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

TESTS_E2E_BINDIR="$(pwd)/cardano-wallet-$VERSION-linux64"
export TESTS_E2E_BINDIR

cd test/e2e

TESTS_NODE_DB="$(pwd)/state/node_db"
export TESTS_NODE_DB

mkdir -p "$TESTS_NODE_DB"/preprod
rsync -a "$NODE_STATE_DIR/db-new" "$TESTS_NODE_DB/preprod"

git fetch --all



git checkout "$RELEASE_CANDIDATE_BRANCH"

CARDANO_NODE_CONFIGS="$(pwd)/../../configs/cardano"
export CARDANO_NODE_CONFIGS

tmpfile=$(mktemp /tmp/node-preprod.XXXXXX)

CARDANO_NODE_SOCKET_PATH="$tmpfile"
export CARDANO_NODE_SOCKET_PATH


TESTS_E2E_STATEDIR=./state
export TESTS_E2E_STATEDIR

TESTS_E2E_TOKEN_METADATA=https://metadata.world.dev.cardano.org/
export TESTS_E2E_TOKEN_METADATA

nix-shell --run "rake run_on[preprod,sync,true]"

rm "$tmpfile"