default:
  @just --list

# build wallet-e2e suite with cabal
build:
  cabal build all

local-cluster $LOCAL_CLUSTER_CONFIGS="lib/local-cluster/test/data/cluster-configs":
  nix shell '.#local-cluster' '.#cardano-node' '.#cardano-wallet' -c "local-cluster"

unit $LOCAL_CLUSTER_CONFIGS="lib/local-cluster/test/data/cluster-configs":
  cabal test cardano-wallet:unit

# run wallet-e2e suite against the preprod network
e2e-preprod:
  nix run '.#cardano-wallet-e2e' -- preprod \
    -s lib/wallet-e2e/test-state \
    -c lib/wallet-e2e/config/cardano-node/preprod

# run wallet-e2e suite against the local test cluster
e2e-local:
  nix shell \
    '.#local-cluster' '.#cardano-node' '.#cardano-wallet' '.#cardano-wallet-e2e' '.#local-cluster' \
    -c wallet-e2e local -s lib/wallet-e2e/test-state -c lib/local-cluster/test/data/cluster-configs

# run wallet-e2e suite against the manually started node/wallet
e2e-manual:
  nix run '.#cardano-wallet-e2e' -- manual
