import Engine.Scene.Scene
import Platform.IPhone
import Platforms.IOS

{- To use the iosMain entry function, your iPhone project must implement the following callbacks:
- void getResourcePath(char * buf, int maxLength)
- void register_touch_began_callback(void (*_touchDown)(int, double, double))
- void register_touch_ended_callback(void (*_touchUp)(int, double, double))
- void register_touch_moved_callback(void (*_touchMoved)(int, double, double))
- void register_init_draw_callback(void (*_initDraw)(int, int))
- void register_draw_frame_callback(void (*_drawFrame)(void))
- void c_main()
-
- See main.m for a good implementation of these callbacks.
-
- See GameViewController.m for a good way to use these callbacks.
-}

main :: IO ()
main = do
        rp <- resourcesPath
        sceneOp <- makeScene rp

        let operators = [sceneOp]
        iosMain operators (_addEvent sceneOp)
