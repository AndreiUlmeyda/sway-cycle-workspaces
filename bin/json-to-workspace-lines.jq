#!/usr/bin/jq -fr

# TODO proper comment
. | .[] | "\(.output) \(.name) \(.focused)"