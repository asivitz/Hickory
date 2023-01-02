# Hickory

Hickory is a game engine for Haskell. The pieces (vulkan renderer, input systems, resource loading, game state management) are fairly modular, allowing some pieces to be used without committing to others.
<img width="1213" alt="Screen Shot 2023-01-02 at 4 52 14 PM" src="https://user-images.githubusercontent.com/607403/210281421-de04a8c6-3fa4-4719-9195-1c911189e893.png">

## Core library: 'Hickory'

- Vulkan Graphics
- 3D Model Loading / Animation
- Camera Projection/Unprojection
- Text Rendering
- Raw Input (Mouse clicks / touches and key pressses)
- Misc. Math Utilities

## Functional Reactive Paradigm: 'Hickory-FRP'

- Built on the 'Reactive Banana' FRP library
- Game loop
- Separate rendering and physics time steps
- Game state recording and time-travel
- High level UI widget abstraction

## GLFW Backend: 'Hickory-GLFW'

- Easy initialization of a Vulkan context and input handling for GLFW (a cross platform desktop library)

## iOS Backend: 'Hickory-iOS'

- Easy initialization of a Vulkan context and input handling for iOS
- Some additional fiddly work needed to get a working binary, but overall this is a relatively painless way to get into iOS development

## Getting started

The Examples/ directory has a simple action game example.

```
$ stack run shooter
```


## Contributing

See CONTRIBUTING.md file for details.

## License

See LICENSE file for details.
