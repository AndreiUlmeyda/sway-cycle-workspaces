#!/usr/bin/jq -fr

. | .[] | "\(.output) \(.name) \(.focused)"