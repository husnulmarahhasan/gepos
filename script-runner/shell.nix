let
  cardanoPkgs = import ../default.nix {};
in
  cardanoPkgs.cardano-sl-script-runner.env.overrideAttrs (drv: {
    buildInputs = drv.buildInputs ++ [ cardanoPkgs.cardano-sl-node-static cardanoPkgs.cardano-sl-tools ];
  })
