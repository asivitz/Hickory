{-# LANGUAGE DuplicateRecordFields #-}

module Platforms.GLFW.DearImGui where

import Acquire.Acquire (Acquire)
import Control.Monad (void)
import DearImGui.GLFW (glfwShutdown)
import DearImGui.GLFW.Vulkan (glfwInitForVulkan)
import Graphics.UI.GLFW (Window)
import Hickory.Vulkan.Types (VulkanResources (..), Swapchain (..))
import Hickory.ImGUI.ImGUI (ImGuiResources, initDearImGui)

initDearImGuiForGLFW :: Window -> VulkanResources -> Swapchain -> Acquire ImGuiResources
initDearImGuiForGLFW window = initDearImGui (void $ glfwInitForVulkan window True) glfwShutdown
