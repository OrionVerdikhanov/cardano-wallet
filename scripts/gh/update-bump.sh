#!/usr/bin/env bash
# Needs yq , jq and bump-cli

set -euo pipefail

if [ -z "${BUMP_SH_DOC_ID:-}" ] || [ -z "${BUMP_SH_TOKEN:-}" ]; then
  echo "BUMP_SH_DOC_ID or BUMP_SH_TOKEN variables not set" > /dev/stderr
  exit 1
fi

JSON_SWAGGER=$(mktemp)

cat specifications/api/swagger.yaml | yq . -o=json | jq -r tostring > $JSON_SWAGGER

bump deploy --doc "$BUMP_SH_DOC_ID" --token "$BUMP_SH_TOKEN" $JSON_SWAGGER
