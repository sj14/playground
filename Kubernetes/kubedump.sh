#!/usr/bin/env bash

# https://gist.github.com/negz/c3ee465b48306593f16c523a22015bec

#set -o xtrace   # output every line which gets executed
#set -o errexit  # fail if a command exits with non-zero
set -o nounset  # fail if an unset variable is used
set -o pipefail # fail if a command in a pipe fails

CONTEXT="$1"

if [[ -z ${CONTEXT} ]]; then
  echo "Usage: $0 KUBE-CONTEXT"
  exit 1
fi

# Namespaced resources
NAMESPACES=$(kubectl get ns -o jsonpath="{.items[*].metadata.name}")
NAMESPACE_RESOURCES=$(kubectl api-resources --namespaced=true -o name | tr "\n" " ")

for ns in ${NAMESPACES};do
  for resource in ${NAMESPACE_RESOURCES};do
    rsrcs=$(kubectl --context ${CONTEXT} -n ${ns} get -o json ${resource} | jq -r '.items[].metadata.name')
    for r in ${rsrcs};do
      dir="${CONTEXT}/${ns}/${resource}"
      echo "${dir}/${r}"
      mkdir -p "${dir}"
      kubectl --context ${CONTEXT} -n ${ns} get -o yaml ${resource} ${r} > "${dir}/${r}.yaml"
    done
  done
done

# Cluster-Scoped resources
CLUSTER_RESOURCES=$(kubectl api-resources --namespaced=false -o name | tr "\n" " ")
for resource in ${CLUSTER_RESOURCES};do
    rsrcs=$(kubectl --context ${CONTEXT} get -o json ${resource} | jq -r '.items[].metadata.name')
    for r in ${rsrcs};do
        dir="${CONTEXT}/_cluster-scoped/${resource}"
        echo "${dir}/${r}"
        mkdir -p "${dir}"
        kubectl --context ${CONTEXT} get -o yaml ${resource} ${r} > "${dir}/${r}.yaml"
    done
done
