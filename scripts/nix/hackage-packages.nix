# Hackage packages that `aihc install` must install in `nix flake check`.
#
# Add a package by appending an entry. The `hash` is the SRI hash of the
# unpacked tarball, as printed by
# `nix-prefetch-url --unpack https://hackage.haskell.org/package/NAME-VERSION/NAME-VERSION.tar.gz`
# followed by `nix hash convert --hash-algo sha256 --to sri HASH`.
#
# Each entry accepts:
#   name     Hackage package name.
#   version  Exact Hackage version.
#   hash     SRI hash of the unpacked tarball.
#   lint     Optional. Pass `--lint` to `aihc install`. Defaults to true.
#   targets  Optional list of targets. Defaults to the host targets that the
#            example tests use.
#   dependencies
#            Optional list of Hackage packages that the package depends on,
#            each with `name`, `version`, and `hash`. The test puts them next
#            to the package source, so `aihc install` finds them without
#            network access.
let
  packages = [
    {
      name = "deepseq";
      version = "1.5.2.0";
      hash = "sha256-vNcooswfE2geBWNtB08cATNIhQlJRRF587lcjfD3XyM=";
    }
    {
      name = "array";
      version = "0.5.8.0";
      hash = "sha256-YGP+ZsyP6onvdd7QbEGQJLPFH2kSubQnVfO/YgpjcwY=";
    }
    {
      name = "split";
      version = "0.2.5.1";
      hash = "sha256-cgX0dtppA/mKggnrVXQ/gpjCgnRlBQVEWFoaEPMRwrU=";
    }
    {
      name = "bytestring";
      version = "0.12.2.0";
      hash = "sha256-bBKEw1dWp24YUf+wrQYNqQ/eDqnM3m1ZtyFy6g1ZFq0=";
      dependencies = [
        {
          name = "deepseq";
          version = "1.5.2.0";
          hash = "sha256-vNcooswfE2geBWNtB08cATNIhQlJRRF587lcjfD3XyM=";
        }
      ];
    }
    {
      name = "base64-bytestring";
      version = "1.2.1.0";
      hash = "sha256-Oe2u9XbsjSFi10dsUqlZbjoz+Bl5jn+s3xTTCono/oE=";
      dependencies = [
        {
          name = "bytestring";
          version = "0.12.2.0";
          hash = "sha256-bBKEw1dWp24YUf+wrQYNqQ/eDqnM3m1ZtyFy6g1ZFq0=";
        }
        {
          name = "deepseq";
          version = "1.5.2.0";
          hash = "sha256-vNcooswfE2geBWNtB08cATNIhQlJRRF587lcjfD3XyM=";
        }
      ];
    }
    {
      name = "tagged";
      version = "0.8.10";
      hash = "sha256-PqVvvs5oh9qVXzUcUUK2kX18qnRbe2yquC2zw/+GZ7k=";
      dependencies = [
        {
          name = "deepseq";
          version = "1.5.2.0";
          hash = "sha256-vNcooswfE2geBWNtB08cATNIhQlJRRF587lcjfD3XyM=";
        }
      ];
    }
  ];

  fetchPackage = pkgs: {
    name,
    version,
    hash,
    ...
  }:
    pkgs.fetchzip {
      url = "https://hackage.haskell.org/package/${name}-${version}/${name}-${version}.tar.gz";
      inherit hash;
    };
in {
  inherit packages fetchPackage;
}
