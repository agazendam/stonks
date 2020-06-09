#!/bin/sh

mkdir $HOME/.secrets
# Decrypt the file
# --batch to prevent interactive command
# --yes to assume "yes" for questions
gpg --quiet --batch --yes --decrypt --passphrase="$DRIVE_SECRET_PASSPHRASE" \
--output $HOME/.secrets/my-project-92901-skicka-8a920f029b4b.json my-project-92901-skicka-8a920f029b4b.json.gpg