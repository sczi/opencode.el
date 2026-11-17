#!/usr/bin/env bash
# Run the ERT tests in a batch Emacs, using the running Emacs's load-path
set -euo pipefail

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
escaped="${repo_root//\\/\\\\}"
escaped="${escaped//\"/\\\"}"

script="$(emacsclient --eval "
(let ((default-directory (file-name-as-directory \"$escaped\")))
  (require 'opencode-edit-hooks)
  (opencode--emacs-ert-script))")"
# emacsclient prints a Lisp string
script="${script#\"}"
script="${script%\"}"

exec "$script"
