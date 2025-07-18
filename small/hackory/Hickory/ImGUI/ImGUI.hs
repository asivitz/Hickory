{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Hickory.ImGUI.ImGUI where

import DearImGui (createContext)
import DearImGui.Raw

exportThis :: IO DearImGui.Raw.Context
exportThis = createContext
