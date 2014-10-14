# Hickory

Hickory is not really a Haskell game engine. It's more of a collection of tools that can be used to make games. It doesn't have opinions and doesn't for you into a particular paradigm.

Engine.Scene
============
These modules provide an Input/Model/View abstraction. Useful for providing high-level structure to your game.

A Scene corresponds to a layer of your game. You might have just two layers, your game world and the UI. A scene contains, essentially, a data model, astepping function, and a rendering function. Moreover, Scenes can generate events which will be provided as input to the other Scenes in the game, or to the same Scene in the next frame.

The Scene type is parameterized over your scene's model, your global input event type, and your scene's resources.
data Scene mdl ie re = ...
mdl - The model used to represent the data for this Scene
ie - The InputEvent data type shared by all scenes
re - The resources loaded by this scene

To use it, first you create a Scene.

makeScene = do
        inputStream <- newIORef (Input []) -- Used to provide a hook for the device to supply input events

        -- Create a camera that determines how this scene will be rendered
        let cam = \ss -> let w = 100
                             ar = aspectRatio ss
                             proj = Ortho (realToFrac w) 1 100
                             route = Route (v3 (realToFrac (-w)/2) (realToFrac (-(w / ar))/2) 0) Nothing
                in Camera proj route
            scene = Scene {
                          _name = "Play",
                          _model = Model newWorld
                          _renderInfo = RenderInfo mat44Identity nullSize worldLabel,
                          _loadResources = loadResources "resources",
                          _stepModel = makeStepModel processInput step,
                          _render = render,
                          _inputStream = inputStream,
                          _loadedRender = Nothing }
        return scene

Then you wrap your Scene up in a SceneOperator. A SceneOperator has no knowledge of the types of your model or resources. It does have knowledge of the InputEvent type, because it is shared between all Scenes.

Your main function might look like this:

main :: IO ()
main = do
        operators <- sequence [World.Scene.makeScene >>= makeSceneOperator
                              , UI.Scene.makeScene >>= makeSceneOperator]
         
        -- glfwMain kicks off the GLFW loop, and steps and renders your scenes
        glfwMain (Size 480 640)
            operators
            (operators !! 1) -- This is the primary scene to which glfw delivers input events
            RawEvent -- This is the function to turn a raw GLFW event into an InputEvent

Components
==========
Hickory includes an implementation of an Entity/Component system, which can be used to model your game world. It is not built by default, as it has a Lens dependency. It can be found in the Components folder.

For more information on Entity-Systems, check out [these articles](http://entity-systems.wikidot.com/).

Graphics
========
Hickory contains functions for loading textures, drawing textured squares, loading shaders, loading .fnt files, rendering text, matrix calculations and projections, and more.

## Example

-- Doesn't currently build as-is because the Lens dependency and component library was removed from the core

The Example directory contains HFreecell, a card game built on top of [tdees40's Freecell library](https://github.com/tdees40/Freecell).

To get HFreecell up and running...
```Bash
$ git clone https://github.com/asivitz/Hickory.git
$ cd Hickory
$ git submodule update --init # pull in a couple depencency projects
$ cabal sandbox init # optional
$ cabal install --dependencies
$ cabal run freecell
```

## Contributing

See CONTRIBUTING.md file for details.

## License

See LICENSE file for details.
