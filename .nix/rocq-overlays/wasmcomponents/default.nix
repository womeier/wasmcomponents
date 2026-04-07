{
  lib,
  mkRocqDerivation,
  which,
  stdlib,
  rocq-core,
  wasmcert,
  version ? null,
}:

with lib;
mkRocqDerivation {
  pname = "wasmcomponents";
  repo = "wasmcomponents";
  owner = "womeier";

  inherit version;
  defaultVersion = "0.0.1";
  /*
    defaultVersion =
      with versions;
      switch
        [ rocq-core.rocq-version ]
        [
          {
            cases = [ (range "9.0" "9.0") ];
            out = "0.0.1";
          }
        ]
        null;

    release."0.2.2".sha256 = "sha256-O50Rs7Yf1H4wgwb7ltRxW+7IF0b04zpfs+mR83rxT+E=";
  */

  propagatedBuildInputs = [
    stdlib
    wasmcert
  ];

  releaseRev = v: "v${v}";

  meta = {
    description = "Formalization of the Wasm component standard in Rocq";
    maintainers = with maintainers; [ womeier ];
    license = licenses.mit;
  };
}
