# Hickory

A practical game engine built in Haskell.

Hickory is based on the concept of Entity-Systems. For more information on Entity-Systems, check out [these articles](http://entity-systems.wikidot.com/).

## Example

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

## Documentation

Hickory games are made up of Systems, Events, Resources, and Components.

# Systems

Systems are modules of code that run independently. Hickory comes with many general purpose systems, including a draw system, an FPS counter, a texture loader, a camera system, and many others.

Systems generally operate on their own data, declared like this:

```Haskell
data SysData = SysData Int
```

The system type itself just contains a run function, but that run function is given a reference to a SysData value, so that it can change the data over time.

We define 'make' and 'run' functions to build the system.

```Haskell
make = do
    sysdata <- liftIO $ newIORef (SysData 42)
    return $ System (run sysdata)

run sysdata = do
    SysData num <- getSysData sysdata
    liftIO $ print num
```

'make' and 'run' both operate within a monad (called SysMonad) which is layered on top of IO. 

# Events

Systems can broadcast and receive events to and from other systems.

## Contributing

See CONTRIBUTING.md file for details.

## License

See LICENSE file for details.
