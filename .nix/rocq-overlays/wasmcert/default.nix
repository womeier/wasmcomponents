{
  lib,
  coqPackages_9_1,
  version ? null,
}:

coqPackages_9_1.wasmcert.override { inherit version; }
