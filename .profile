source_list=(
  "$HOME/local/rc/profile"
  "$HOME/.local/config/profile")

path_list=(
  "$HOME/local/bin"
  "$HOME/.local/bin"
  "$HOME/bin"
  "$HOME/.cargo/bin"
  "$HOME/.screenlayout"
  "/opt/bin"
  "/opt/npm/bin"
  "/opt/adr-tools/src/"
  "/opt/omnisharp-roslyn/"
  "/opt/helm/linux-amd64/"
  "/opt/forge")

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        source "$HOME/.bashrc"
    fi
fi

for path in "${source_list[@]}"
do
  if [ -f "${path}" ]; then
    source "${path}"
  fi
done

for path in "${path_list[@]}"
do
  if [ -d "${path}" ]; then
    PATH="$PATH:${path}"
  fi
done

export DOTNET_CLI_TELEMETRY_OPTOUT=1
