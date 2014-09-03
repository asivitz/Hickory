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

### Systems

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

### Events

Systems communicate by broadcasting and receiving events. They receive events by registering functions in an RSC structure (Remote System Call). For example, if we want a system to act on the 'printAll' debug event by printing its data, we can change the make function to the following:

```Haskell
make = do
    ...
    registerEvent systemContext printAll (printSysData sysdata)
    ...
```

Similarly, we can broadcast the printAll event in our run function by doing the following:

```Haskell
run sysdata = do
    ...
    runEventId systemContext printAll
```

The systemContext parameter indicates that the event is built-in to the engine. New events specific to your particular game are added to the GameRSC type in GameContext.hs, and can then be used with the gameContext parameter. The two contexts may also be abbreviated sysCon and gameCon, respectively. For a list of all built-in events, checkout the RSC type in Engine.World.

### Resources

Resources are similar to events, but instead of being used to notify other systems, they are used to pull in information from other systems. For example, the Textures system provides a reserveTex resource that other systems can use to load textures for drawing.

```Haskell
-- Systems/Textures.hs
make = do
        textures <- liftIO $ newIORef empty
        registerResource sysCon reserveTex (reserveTex' textures)
```

Note: In this example, the local implementation of the loading function (called reserveTex') is given a reference to the texture SysData, so that it can store the texture once it has been loaded.

To use the resource function, first we grab the registered function from the RSC structure (prefixed with an underscore), and then call it.

```Haskell
-- Systems/MyGameSys.hs
make = do
        RSC { _reserveTex } <- getRSC sysCon
        square_tex <- _reserveTex "square.png"
```

### Components

In-game objects are made up of an Entity combined with various Components. In this example, we'll respond to a new game event by creating a player.

```Haskell
make = do
    registerEvent gameCon newGame createPlayer

createPlayer = do
        e <- spawnEntity
        {- This component gives it a draw location -}
        addComp sysCon e drawStates $ DrawState (v3 50.0 50.0 0.0)
        {- This component renders the player with a solid red square -}
        addComp sysCon e drawables $ Drawable (SolidSquare 5.0 red)
        {- This component gives the player a starting velocity of 10/s in the positive X direction -}
        addComp sysCon e newtonianMovers $ NewtonianMover (v3 10.0 0 0) (v3 0 0 0)
        {- This player component is handled by the game -}
        addComp gameCon e players $ Player "Player One"
```

(Note: The shader, which needs to be loaded and given to SolidSquare, was left out for the sake of example.)

That last component, "players", is not built into the engine, and so will need to be added to the GameContext and processed by some game system.

## Contributing

See CONTRIBUTING.md file for details.

## License

See LICENSE file for details.
