#! /bin/bash
guix shell guile-next guile-ares-rs artanis -- guile -L . -L /home/mbc/projects/platewiz -c '((@ (ares server) run-nrepl-server))'

