# Set up the per-user profile.
# This part should be kept in sync with nixpkgs:nixos/modules/programs/shell.nix
NIX_LINK = ${HOME}/.nix-profile

# Append ~/.nix-defexpr/channels to $NIX_PATH so that <nixpkgs>
# paths work when the user has fetched the Nixpkgs channel.
export NIX_PATH = ${NIX_PATH}:${NIX_PATH}:${HOME}/.nix-defexpr/channels

# Set up environment.
# This part should be kept in sync with nixpkgs:nixos/modules/programs/environment.nix
export NIX_PROFILES = "/nix/var/nix/profiles/default ${HOME}/.nix-profile"

NIX_SSL_CERT_FILE_SH = if [ -e /etc/ssl/certs/ca-certificates.crt ]; then \
  echo -n /etc/ssl/certs/ca-certificates.crt; \
elif [ -e /etc/ssl/ca-bundle.pem ]; then \
  echo -n /etc/ssl/ca-bundle.pem; \
elif [ -e /etc/ssl/certs/ca-bundle.crt ]; then \
  echo -n /etc/ssl/certs/ca-bundle.crt; \
elif [ -e /etc/pki/tls/certs/ca-bundle.crt ]; then \
  echo -n /etc/pki/tls/certs/ca-bundle.crt; \
elif [ -e $(NIX_LINK)/etc/ssl/certs/ca-bundle.crt ]; then \
  echo -n $(NIX_LINK)/etc/ssl/certs/ca-bundle.crt; \
elif [ -e $(NIX_LINK)/etc/ca-bundle.crt ]; then \
  echo -n $(NIX_LINK)/etc/ca-bundle.crt; fi

export NIX_SSL_CERT_FILE = $(shell $(NIX_CERT_FILE_SH))

export PATH=$(NIX_LINK)/bin
