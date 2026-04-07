{
  ## DO NOT CHANGE THIS
  format = "1.0.0";
  ## unless you made an automated or manual update
  ## to another supported format.

  attribute = "wasmcomponents";

  default-bundle = "9.1";

  bundles."9.1"= {
    rocqPackages.rocq-core.override.version = "9.1.1";
    rocqPackages.wasmcert.override.version = "v2.2.0";
    push-branches = [ "main" ];
  };

  ## Cachix caches to use in CI
  ## Below we list some standard ones
  cachix.coq = {};
  cachix.math-comp = {};
  cachix.coq-community = {};

  cachix.womeier.authToken = "CACHIX_AUTH_TOKEN";
  
  ## Or if you have a signing key for a given Cachix cache:
  # cachix.my-cache.signingKey = "CACHIX_SIGNING_KEY"
}
