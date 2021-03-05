#!/usr/bin/env bash

# Description
# ===========
# Get the SonarQube Quality Gate status of a specific project for the current git commit.
# When the Quality Gate analysis is "OK", return 0, otherwise, return 1.
# Well suitable for usage in a build pipeline.
#
# Requirements
# ============
# git, curl, jq
#
# Usage
# =====
# Execute this script `./sonar-status.sh <host> <auth_token> <project_key>` and check for the return code.
# Example: `./sonar-status.sh http://localhost:9000 d7981b9c1333bef3ff83502cea6f6cd6ce4d70f6 test`

set -euf -o pipefail

SONAR_HOST=$1
AUTH_TOKEN=$2 # project key token or user token (see https://docs.sonarqube.org/latest/user-guide/user-token/)
PROJECT_KEY=$3

GIT_COMMIT=$(git rev-parse HEAD)

sonar_analysis_status() {
    retry=0
    while [  $retry -lt 6 ]; do
        # get latest analysis id to corresponding git commit hash
        analysis_id=$(curl -s -u $AUTH_TOKEN: $SONAR_HOST/api/project_analyses/search?project="$PROJECT_KEY" | jq '.analyses[] | select(.revision == "'"$GIT_COMMIT"'") | .key' | head -n 1)

        # remove double quotes from analysis id (e.g. "AWv6wb07Y5FuS8wxa-xk" -> AWv6wb07Y5FuS8wxa-xk)
        analysis_id=$(echo "$analysis_id" | tr -d "\"\`'")

        # get quality gate status of this anlysis
        analysis_status=$(curl -s -u $AUTH_TOKEN: $SONAR_HOST/api/qualitygates/project_status\?analysisId="$analysis_id" | jq '.projectStatus.status')

        # break this loop when analysis was found,
        # otherwise, try again (next loop iteration)
        if [ "$analysis_status" != null ]
            then
            break
        fi

        echo "Retry $retry: $PROJECT_KEY"
        (( retry=retry+1 ))
        
        sleep 10s
    done

    # print and return status depending on result
    printf "%s \t %-35s \t $SONAR_HOST/dashboard?id=%s\n" "$analysis_status" "$PROJECT_KEY" "$PROJECT_KEY"
    if [ "$analysis_status" = \""OK\"" ]
        then
            return 0
        else
            return 1
    fi
}

if ! sonar_analysis_status "$PROJECT_KEY"
then
    exit 1
else
    exit 0
fi
