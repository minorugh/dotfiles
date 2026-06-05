#!/bin/bash
# ~/.mutt/abook-add.sh
# メールのFromヘッダーをデコードしてabookに追加する

cat | perl -MMIME::Base64 -MEncode -pe '
    s/=\?UTF-8\?B\?([A-Za-z0-9+\/=]+)\?=/Encode::decode("UTF-8", MIME::Base64::decode_base64($1))/ge;
    s/=\?UTF-8\?Q\?([^?]+)\?=/Encode::decode("UTF-8", $1)/ge;
' | abook --add-email
