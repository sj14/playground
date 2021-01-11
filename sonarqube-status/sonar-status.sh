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
# 1. Modify the SONAR_HOST and AUTH_TOKEN variable
# 2. Source this file (`source sonar-status.sh`)
# 3. Call `sonar_analysis_status YOUR_PROJECT_KEY`
# 4. Check for the return code.
#
# Alternative
# ===========
# 1. Modify the SONAR_HOST variable
# 2. Uncomment the last lines at the bottom of this file
# 3. Execute this script `./sonar-status.sh`
# 4. Check for the return code.

set -euf -o pipefail

SONAR_HOST="htts://YOUR-SONARQUBE-SERVER/"
GIT_COMMIT=$(git rev-parse HEAD)
AUTH_TOKEN="CHANGE_ME" # see https://docs.sonarqube.org/latest/user-guide/user-token/

sonar_analysis_status() {
    project_key="$1"

    retry=0
    while [  $retry -lt 6 ]; do
        # get latest analysis id to corresponding git commit hash
        analysis_id=$(curl -s -u $AUTH_TOKEN: $SONAR_HOST/api/project_analyses/search?project="$project_key" | jq '.analyses[] | select(.revision == "'"$GIT_COMMIT"'") | .key' | head -n 1)

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

        echo "Retry $retry: $project_key"
        (( retry=retry+1 ))
        
        sleep 10s
    done

    # print and return status depending on result
    printf "%s \t %-35s \t $SONAR_HOST/dashboard?id=%s\n" "$analysis_status" "$project_key" "$project_key"
    if [ "$analysis_status" = \""OK\"" ]
        then
            return 0
        else
            return 1
    fi
}

## Alternative:

# if ! sonar_analysis_status YOUR_PROJECT_KEY
# then
#     exit 1
# else
#     exit 0
# fi
