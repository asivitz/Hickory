# Hickory

Hickory is not really a Haskell game engine. It's more of a collection of tools and abstractions that can be used to make games. It doesn't have opinions and doesn't force you into a particular paradigm.

## Engine.Scene
These modules provide an Input/Model/View abstraction. Useful for providing high-level structure to your game.

A Scene corresponds to a layer of your game. You might have just two layers, your game world and the UI. To make a Scene, you need...

- A model (can be anything! just needs to produce a view matrix for rendering) Type: mdl
- A resource loading function. Type: IO re
- A step function. Type: RenderInfo -> Input ie -> Double -> mdl -> (mdl, [ie])
    RenderInfo gives you the previous frame's view matrix, the screen size, and the layer ID the renderer uses (called the Label)
    Input contains a list of input events, coming from either from a different scene, or this scene in a previous frame
    The Double is the amount of time elapsed since the previous frame
    The function should return the updated model and a list of generated events
- A render function. Type: re -> RenderInfo -> mdl -> IO ()
    re is the loaded resources
- The scene's layer ID.

Here's an example of creating a scene:

```Haskell
makeScene resPath = makeSceneOperator emptyWorld
                                      (loadResources resPath)
                                      stepModel
                                      render
                                      worldLabel

```

Okay, fine, we actually made a SceneOperator, which can be happily packed up with other SceneOperators and processed in a list.

Your main function might look like this:

```Haskell
main :: IO ()
main = do
    operators <- sequence [Play.Scene.makeScene rp, Edit.Scene.makeScene rp]
        
    -- glfwMain kicks off the GLFW loop, and steps and renders your scenes
    glfwMain (Size 480 640)
        operators
        (operators !! 1) -- This is the primary scene to which glfw delivers input events
        RawEvent -- This is the function to turn a raw GLFW event into an InputEvent
```

## Components
Hickory includes an implementation of an Entity/Component system, which can be used to model your game world. It is not built by default, as it has a Lens dependency. It can be found in the Components folder.

For more information on Entity-Systems, check out [these articles](http://entity-systems.wikidot.com/).

## Menus
The Menus module provides an abstraction for...
- Pushing / Popping menu screens
- Generating events from clicking on menu items
- Rendering menu items, including transitions between screens

## Graphics
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
