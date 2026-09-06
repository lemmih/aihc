-- | The WebAssembly runtime sources of the WASI P3 target. The code
-- generator is "Aihc.Wasm.Lir".
module Aihc.Wasm
  ( wasip3RuntimeSourcePath,
    wasip3RuntimeSourcePaths,
    wasip3WorldPath,
  )
where

import Paths_aihc (getDataFileName)

wasip3RuntimeSourcePath :: IO FilePath
wasip3RuntimeSourcePath = getDataFileName "compiler/wasm/runtime/aihc_wasip3.c"

wasip3RuntimeSourcePaths :: IO [FilePath]
wasip3RuntimeSourcePaths =
  mapM
    getDataFileName
    ["compiler/wasm/runtime/aihc_wasip3.c"]

wasip3WorldPath :: IO FilePath
wasip3WorldPath = getDataFileName "compiler/wasm/runtime/wit"
