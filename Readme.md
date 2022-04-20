# Hickory

Hickory could be considered a game engine, but really it's a collection of tools and abstractions for making interactive programs in Haskell. It is relatively opinionless, which means the pieces may be used separately or together.

## Core library: 'Hickory'

- OpenGL Graphics
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

- Easy initialization of a GL context and input handling for GLFW (a cross platform desktop library)

## iOS Backend: 'Hickory-iOS'

- Easy initialization of a GL context and input handling for iOS
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
