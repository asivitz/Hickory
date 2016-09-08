# Hickory

Hickory is a collection of haskell tools and abstractions for making interactive programs. It could be considered a game engine, but it is largely opinionless. You can use whatever library you want for actually organizing your game logic (e.g. any FRP library, my Layer library, etc...)

It renders using OpenGL and has support for both GLFW (for desktop) and WebGL through GHCJS.

## Documentation

Proper documentation is lacking for now. Please check out Examples/Shooter in the meantime.

```Bash
$ git clone https://github.com/asivitz/Hickory.git
$ git clone https://github.com/asivitz/layer.git
$ cd Hickory/Example/Shooter
$ stack build
$ stack exec shooter
```

## Contributing

See CONTRIBUTING.md file for details.

## License

See LICENSE file for details.
