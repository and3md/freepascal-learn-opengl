# freepascal-learn-opengl
Free Pascal (FPC) port of https://github.com/JoeyDeVries/LearnOpenGL

## Changes compared to the original c++ sources

I try to keep the sources as close as possible to the C ++ sources but some things I preferred to do more like pascal:
* The sources of the examples are in the .lpr files and can be easy showed in Lazarus (menu Project->View Project Source) 
* Images are loaded with fpimage with my litle helper functions defined in [UMyFPImage unit](https://github.com/and3md/freepascal-learn-opengl/blob/master/include/umyfpimage.pas)
* I tried to use standard freepascal matrix unit but it's lack some functions so I implemented them in [UMyMatrixExt](https://github.com/and3md/freepascal-learn-opengl/blob/master/include/umymatrixext.pas)
* I used standard GL and GLext FPC units instead of GLAD.
* I used OpenGL variable types.

## If you looking for game engine in pascal language

I recommend [Castle Game Engine](https://castle-engine.io/). It's open source, cross-platform (desktop, mobile, console) 3D and 2D game engine supporting many asset formats (glTF, X3D, Spine...) and using modern Object Pascal. Check my [android game](https://play.google.com/store/apps/details?id=com.digitalkarabela.cge.colorpick) for example. There are also some articles about CGE on [my web site](https://digitalkarabela.com/category/gamedev/castle-game-engine/).

## GLFW pascal headers
I used Pascal header translate by: Jorge Turiel (Aka BlueIcaro) - https://github.com/Blueicaro/GLFW with some small improvements for linux linking and `GLFW_TRUE`/`GLFW_FALSE` definitions added.
