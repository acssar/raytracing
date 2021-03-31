Haskell simple ray tracer
-------------------------------
for studying purpose
==========================

* test.ppm - example of the scene made from inputexample.txt
* inputfilename - name of the file with scene (like inputexample.txt)

:Lights:

 * 1 - Pointlight
 * else - Ambientlight (for Ambient u should put 3 zero on coordinates)
 * 1 int
 * 6 double

example: PointLight (30.0,0.0,100.0) (0.5,0.5,0.5)

:TexturedObjects:

 * 1 - Plane
 * else - Sphere
 * 1 - Solid
 * else - (now error)
 * 8 double
 * 1 int
 * 2 double

example: (Sphere 15.0 (0.0,0.0,-400.0),Texture ( Solid (0.8,0.8,0.8) ) 1.0 60 0.0 0.0)

:Background:

 * 3 double

example: (0.3,0.3,0.3)

:Camera:

 * 3 Double

example: Camera (0.0,0.0,175.0) (100,75)
