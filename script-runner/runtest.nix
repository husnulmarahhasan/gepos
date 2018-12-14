let
  cardanoPkgs = import ../default.nix {};
  pkgs = cardanoPkgs.pkgs;
  runTest = binaryName: pkgs.runCommand binaryName { buildInputs = with cardanoPkgs; [ cardano-sl-script-runner cardano-sl-node-static cardano-sl-tools ]; } ''
    cat /etc/nsswitch.conf /etc/protocols > /dev/null
    mkdir $out
    cd $out
    mkdir poc-state
    ${binaryName} --configuration-file ${../lib/configuration.yaml} --db-path poc-state/db --keyfile poc-state/secret.key --log-config ${./log-config.yaml} --logs-prefix poc-state/logs --topology ${./topology-local.yaml} --no-brickui --policies ${../scripts/policies/policy_script-runner.yaml} || true # TODO
  '';
in {
  test1 = runTest "poc";
}
