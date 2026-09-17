#!/usr/bin/env bash
set -euo pipefail
repro_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_dir=$(cd -- "$repro_dir/../.." && pwd)
probe_build_dir=${1:?Pass the artifact directory}
mkdir -p "$probe_build_dir/build"
python3 - "$repo_dir" "$probe_build_dir" <<'PY'
from pathlib import Path
import sys
repo, output=map(Path,sys.argv[1:])
source=(repo/'app/Main.hs').read_text()
old='  ioEnv <- newIOEnvWithPermissions'
anchor='  case startupIntent opts of\n    IntentTui -> runTui'
assert source.count(old)==1 and source.count(anchor)==1, 'CLI changed; update instrumentation anchors'
source=source.replace('import qualified Paths_hach as Paths','import qualified Hook\nimport Data.Version (makeVersion)')
source=source.replace('Paths.version','(makeVersion [0,1,6,0])')
source=source.replace(old,'  originalEnv <- newIOEnvWithPermissions')
source=source.replace(anchor,'  ioEnv <- Hook.attach originalEnv\n\n'+anchor)
(output/'Main.hs').write_text(source)
PY
cd "$repo_dir"
cabal build lib:hach
cabal exec -- ghc -threaded -i"$repro_dir" -outputdir "$probe_build_dir/build" "$probe_build_dir/Main.hs" -o "$probe_build_dir/hach-probe"
