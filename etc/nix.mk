NIX_SSL_CERT_FILE_SH = \
  if [ -e /etc/ssl/certs/ca-certificates.crt ]; then \
    echo -n /etc/ssl/certs/ca-certificates.crt; \
  elif [ -e /etc/ssl/ca-bundle.pem ]; then \
    echo -n /etc/ssl/ca-bundle.pem; \
  elif [ -e /etc/ssl/certs/ca-bundle.crt ]; then \
    echo -n /etc/ssl/certs/ca-bundle.crt; \
  elif [ -e /etc/pki/tls/certs/ca-bundle.crt ]; then \
    echo -n /etc/pki/tls/certs/ca-bundle.crt; \
 fi
export NIX_SSL_CERT_FILE = $(shell $(NIX_CERT_FILE_SH))

# @todo: remove these later
export NIX_PATH = ${HOME}/.nix-defexpr/channels
export NIX_PROFILES = "/nix/var/nix/profiles/default ${HOME}/.nix-profile"
export PATH = $(HOME)/.nix-profile/bin:/usr/bin
