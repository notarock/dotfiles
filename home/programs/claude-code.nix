{
  config,
  pkgs,
  lib,
  ...
}:

let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;

  # MCP servers we want available user-wide. They live inside ~/.claude.json,
  # a blob Claude Code also uses for local app state (history, caches, IDs),
  # so we can't manage the whole file declaratively — instead we merge these
  # in on activation and only fill in servers that aren't already present.
  # Secrets: left as placeholders on purpose, fill in manually after rebuild.
  mcpServers = {
    kubernetes-mcp-server = {
      command = "npx";
      args = [
        "-y"
        "kubernetes-mcp-server@latest"
        "--read-only"
      ];
      env.KUBECONFIG = "${config.home.homeDirectory}/src/kubeconfigs/staging-k8s-cluster-kubeconfig.yaml";
    };
    grafana = {
      type = "stdio";
      command = "mcp-grafana";
      args = [ ];
      env = {
        GRAFANA_URL = "https://civalgo.grafana.net";
        GRAFANA_SERVICE_ACCOUNT_TOKEN = "REPLACE_ME";
      };
    };
  };
in
{
  home.file.".claude/settings.json".source = ./claude/settings.json;

  home.activation.backupUnmanagedClaudeSettings = lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
    path="$HOME/.claude/settings.json"
    if [ -e "$path" ] && [ ! -L "$path" ]; then
      if [ -e "$path.pre-nix" ]; then
        echo "Refusing to overwrite $path.pre-nix" >&2
        exit 1
      fi
      mv "$path" "$path.pre-nix"
    fi
  '';

  home.activation.claudeCodeMcpServers = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    claudeJson="$HOME/.claude.json"
    [ -f "$claudeJson" ] || echo '{}' > "$claudeJson"
    desired=${lib.escapeShellArg (builtins.toJSON mcpServers)}
    tmp="$(mktemp)"
    ${pkgs.jq}/bin/jq \
      --argjson desired "$desired" \
      '.mcpServers = ((.mcpServers // {}) as $cur
        | ($desired | to_entries | map(select(.key as $k | ($cur | has($k)) | not)) | from_entries) as $missing
        | $cur + $missing)' \
      "$claudeJson" > "$tmp"
    mv "$tmp" "$claudeJson"
  '';

  home.file.".config/herdr/config.toml" = lib.mkIf isDarwin {
    source = ./claude/herdr-config.toml;
  };
}
