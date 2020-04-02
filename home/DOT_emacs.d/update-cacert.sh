#! /usr/bin/env bash

# Update the cacert.pem file from curl's official website.
# This will download the file only if the remote file is more recent than the
# local one.

curl \
    --ssl-reqd \
    --time-cond "$HOME/.emacs.d/cacert.pem" \
    https://curl.haxx.se/ca/cacert.pem \
    --output "$HOME/.emacs.d/cacert.pem"
