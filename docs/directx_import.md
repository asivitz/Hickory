# Exporting from Blender
Using blender's DirectX exporter (available as an addon), we can import models with meshes, bones, skinning, and animation, all in one file. But we have to modify the export script a bit.

- Open it up (blender-2.77a-OSX_10.6-x86_64/blender.app/Contents/Resources/2.77/scripts/addons/io_scene_x/export_x.py)
- In GenericAnimationGenerator's __init__ method, comment out 'self._GenerateKeys()'. Because we don't need the keys that animate the actual armature object (we just need the bones).
- In ArmatureAnimationGenerator's _GenerateBoneKeys, change the for loop to 
        frameRange = ArmatureObject.animation_data.action.frame_range
        start = round(frameRange[0])
        end = round(frameRange[1])

        for Frame in range(start, end + 1):

    Instead of:
        for Frame in range(Scene.frame_start, Scene.frame_end + 1):
    This will cause each action to only export the frames that are actually keyframed.
- When running the export, check all the animation options
- Also you might need to select the last action in the dope sheet, otherwise it seems to skip some.
