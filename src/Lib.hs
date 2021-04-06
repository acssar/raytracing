module Lib where
import Prelude hiding ((*>),(<*>),(<+>),(<->))
import Data.Maybe
import System.IO
import Control.Monad


type Point2D = (Int,Int)
type Point3D = (Double,Double,Double)
type Vector = (Double,Double,Double)


data Ray = Ray Point3D Vector


data Object = Sphere Double Point3D
 | Plane (Double,Double,Double,Double)
 | Triangle (Point3D, Point3D, Point3D)

type Resolution = (Int,Int)
type Dimension = (Int,Int)


 --colors
type Color = (Double,Double,Double)


 -- diffusion
data Diff = Solid Color |
 Perlin (Point3D -> Color)


 -- Texture
data Texture = Texture Diff Double Int Double Double


 -- Finally objects
type TexturedObject = (Object,Texture)


 -- color of light's sourse
type Intensity = (Double,Double,Double)

data Light = PointLight Point3D Intensity
 | AmbientLight Intensity


 -- OMG IT'S A CAMERA
data Camera = Camera Point3D Dimension


-- intersecton
data Intersection = Intersection Double Ray TexturedObject


-- points to colors in image
type Image = Point2D -> Color


(<+>) :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
(x1,y1,z1) <+> (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

(<->) :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
(x1,y1,z1) <-> (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)

(<*>) :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
(x1,y1,z1) <*> (x2,y2,z2) = (x1*x2,y1*y2,z1*z2)

(*>) :: (Double,Double,Double) -> Double -> (Double,Double,Double)
(x,y,z) *> f = (x*f,y*f,z*f)


--busted max-min functions


maxF :: Double -> (Double,Double,Double) -> (Double,Double,Double)
maxF f (x,y,z) = (max x f, max y f, max z f)

minF :: Double -> (Double,Double,Double) -> (Double,Double,Double)
minF f (x,y,z) = (min x f, min y f, min z f)


--simple operations on vectors


(*.) :: Vector -> Vector -> Double  --scalar
(x1,y1,z1) *. (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

crossProduct :: Vector -> Vector -> Vector
crossProduct (x1,y1,z1) (x2,y2,z2) = (y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2)

len :: Vector -> Double   --length
len v = sqrt (v *. v)

norm :: Vector -> Vector  --normalize
norm v
 | len v < 10**(-9) = (0.0,0.0,0.0)
 | otherwise = v *> (1/(len v))



mkNormVect :: Point3D -> Point3D -> Vector --MakeNormalizedVector
mkNormVect v w = norm (w <-> v)


--distance between 2 points and clipping function


dist :: Point3D -> Point3D -> Double
dist p0 p1 = sqrt ((p1 <-> p0) *. (p1 <-> p0))

clip :: (Double,Double,Double) -> (Double,Double,Double)
clip = (maxF 0.0) . (minF 1.0)


-- quadratik solver


solveq :: (Double,Double,Double) ->[Double]
solveq (a,b,c)
 | (d < 0) = []
 | (d > 0) = [(- b - sqrt d)/(2*a), (- b + sqrt d)/(2*a)]
 | otherwise = [-b/(2*a)]
 where
 d = b*b - 4*a*c


 -- just making ray


mkRay :: Point3D -> Point3D -> Ray
mkRay p1 p2 = Ray p1 (mkNormVect p1 p2)


--inspecting for collision (TODO: square, triangle etc.)


intRayWith :: Ray -> Object -> [Double]
intRayWith (Ray start dir) (Sphere rad cen) = solveq (dir *. dir, 2*(dir *. d), (d *. d) - rad^2)
 where
   d = start <-> cen
intRayWith (Ray start dir) (Plane (a,b,c,d)) = if (abs(part) < 10**(-9)) then [] else [- (d + ((a,b,c) *. start) ) / part]
 where
  part = (a,b,c) *. dir
intRayWith (Ray (x0,y0,z0) (p1,p2,p3)) (Triangle ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3))) =
  if (pointInTriangle (x,y,z) (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) )then intRayWith (Ray (x0,y0,z0) (p1,p2,p3)) pl
  else []
  where
  pl@(Plane (aa,bb,cc,dd)) = planefromtriangle (Triangle ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)))
  y = (p3*cc*y0 + aa*p1*y0 - p2*aa*x0 - p2*cc*z0)/(aa*p1 + bb + cc*p3)
  x = (p1*y - p1*y0 + p2*x0)/p2
  z = (p3*y -p3*y0 + p2*z0)/p2
-- normal for objects (TODO: square, triangle etc.)

planefromtriangle :: Object -> Object
planefromtriangle (Triangle ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3))) = Plane (aa,bb,cc,dd)
 where
  aa = (y2-y1)*(z3-z1) - (y3-y1)*(z2-z1)
  bb = (x3-x1)*(z2-z1) - (x2-x1)*(z3-z1)
  cc = (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
  dd = (-1.0)*(aa + bb + cc)

 -- normal for objects  (TODO: square, triangle etc.

normal :: Point3D -> Object -> Vector
normal p (Sphere rad cen) = norm ((p <-> cen) *> (1/rad))
normal _ (Plane (a,b,c,d)) = norm (a,b,c)
normal _ (Triangle ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3))) = norm (p,q,s) where
  p = (y2-y1)*(z3-z2) - (y3-y1)*(z2-z1)
  q = (x3-x2)*(z2-z1) - (x2-x1)*(z3-z2)
  s = (x2-x1)*(y3-y2) - (x3-x2)*(y2-y1)

sameSide :: Point3D -> Point3D -> Point3D -> Point3D -> Bool
sameSide p1@(xp1,yp1,zp1) p2@(xp2,yp2,zp2) a@(xa,ya,za) b@(xb,yb,zb) = if ((cp1) *. (cp2) >= 0) then True else False
      where
      vec1 = (xb-xa,yb-ya,zb-za)
      vec2 = (xp1-xa,yp1-ya,zp1-za)
      vec3 = (xp2-xa,yp2-ya,zp2-za)
      cp1 = crossProduct vec1 vec2
      cp2 = crossProduct vec1 vec3

pointInTriangle :: Point3D -> Point3D -> Point3D -> Point3D -> Bool
pointInTriangle p a b c = if ((sameSide p a b c) && (sameSide p b a c) && (sameSide p c a b)) then True else False
-- baricentric_test :: Ray -> Object
-- reflection of vector


-------------baracentric check ( can use instead of sameSide and (pointInTriangle))--- t = 1 -u -v
baracentric :: Ray -> Object -> Bool
baracentric (Ray pr vr) (Triangle (a, b, c)) = if (t == 1 - u - v) then True else False
  where
    e1 = b <-> a
    e2 = c <-> a
    t_ = pr <-> a
    d = vr
    p = crossProduct d e2
    q = crossProduct t_ e1
    t = (1 / (p *. e1)) * (q *. e2)
    u = (1 / (p *. e1)) * (p *. t_)
    v = (1 / (p *. e1)) * (q *. d)





reflectDir :: Vector -> Vector -> Vector
reflectDir i n = i <-> (n *> (2*(n *. i)))


-- refraction of vector     (now unused)

refractDir :: Vector -> Vector -> Double -> Vector
refractDir i n r = if (v < 0) then (0.0, 0.0, 0.0) else norm $ (i *> r_c) <+> (n *> (r_c*(abs c) - sqrt v))
 where
 c = n *. (i *> (-1))
 r_c = if (c < 0) then r else 1/r
 v = 1 + (r_c^2) * (c^2 - 1)


 -- map to win (for user's resolution)


mapToWin :: Resolution -> Dimension -> Point2D -> Point3D
mapToWin (rx,ry) (w,h) (px,py) = (x/rxD,y/ryD,0.0)
  where
  (rxD,ryD) = (fromIntegral rx, fromIntegral ry)
  (pxD,pyD) = (fromIntegral px, fromIntegral py)
  (wD,hD) = (fromIntegral w, fromIntegral h)
  (x,y) = ((pxD-rxD/2)*wD,(pyD-ryD/2)*hD)




-----------------------------------ENGINE---------------------------------

-- MAYBE FUNCTIONS


intDist :: (Maybe Intersection) -> Double
intDist Nothing = 0.0
intDist (Just (Intersection d _ _)) = d

intText :: (Maybe Intersection) -> Texture
intText Nothing = Texture (Solid (0.0,0.0,0.0)) 0.0 0 0.0 0.0
intText (Just (Intersection _ _ (_,t))) = t

colorAt :: (Maybe Intersection) -> Color
colorAt Nothing = (0.0,0.0,0.0)
colorAt (Just (Intersection _ _ (_,Texture (Solid color) _ _ _ _) )) = color
colorAt i@(Just (Intersection _ _ (_,Texture (Perlin f) _ _ _ _) )) = f (intPt i)

normalAt :: (Maybe Intersection) -> Vector
normalAt Nothing = (0.0,0.0,0.0)
normalAt i@(Just (Intersection _ _ (o,_) )) = normal (intPt i) o

intPt :: (Maybe Intersection) -> Point3D
intPt Nothing = (0.0,0.0,0.0)
intPt (Just (Intersection d (Ray start dir) _)) = start <+> (dir *> d)


-- registration of intersecton (fst > 0 from list of distances beyond obj and ray)


fstPos :: [Double] -> Double
fstPos [] = 0.0
fstPos (l:ls) = if l > 10**(-6) then l else fstPos ls


-- finding closest intersection


closestInt :: Ray -> (Maybe Intersection) -> TexturedObject -> (Maybe Intersection)
closestInt r i (o,m) = if d > 10**(-6) && ((isNothing i) || d < (intDist i))
 then Just (Intersection d r (o,m))
 else i
 where
 d = fstPos (intRayWith r o)


 -- foldling list of objects


intersect :: Ray -> [TexturedObject] -> (Maybe Intersection)
intersect r o = foldl (closestInt r) Nothing o


--  some functions for properties of materials


diff :: (Maybe Intersection) -> Light -> Color
diff _ (AmbientLight _) = (0.0,0.0,0.0)
diff i (PointLight pos int) = (int *> ((mkNormVect (intPt i) pos) *. (normalAt i))) <*> (colorAt i)

spec :: (Maybe Intersection) -> Vector -> Light -> Color
spec _ _ (AmbientLight _) = (0.0,0.0,0.0)
spec i d (PointLight pos int) = int *> (reflCoef * ( ((normalAt i) *. h)**(fromIntegral specCoef) ))
 where
 h = norm ((d *> (-1)) <+> (mkNormVect (intPt i) pos))
 (Texture _ reflCoef specCoef _ _) = intText i

shadePt :: Intersection -> Vector -> [TexturedObject] -> Light -> Color
shadePt i d o (AmbientLight int) = int
shadePt i d o l@(PointLight pos int)
 | s = (0.0,0.0,0.0)
 | otherwise = (diff (Just i) l) <+> (spec (Just i) d l)
 where
 s = not (isNothing i_s) && (intDist i_s) <= dist (intPt (Just i)) pos
 i_s = intersect (mkRay (intPt (Just i)) pos) o


 --


reflectPt :: Int -> Intersection -> Vector -> [TexturedObject] -> [Light] -> Color
reflectPt depth i d = colorPt depth (Ray (intPt (Just i)) (reflectDir d (normalAt (Just i)))) (0.0,0.0,0.0)

refractPt :: Int -> Intersection -> Vector -> Color -> [TexturedObject] -> [Light] -> Color
refractPt depth i d b = if refractedDir == (0.0,0.0,0.0) then (\x y -> (0.0,0.0,0.0))
 else colorPt depth (Ray (intPt (Just i)) refractedDir) (b *> refrCoef)
 where
 refractedDir = refractDir d (normalAt (Just i)) refrIndex
 (Texture _ _ _ refrCoef refrIndex) = intText (Just i)


 -------------------------------------------------------MAIN FUNCTIONS---------------------------------------------------------------


colorPt :: Int -> Ray -> Color -> [TexturedObject] -> [Light] -> Color
colorPt (-1) _ _ _ _ = (0.0, 0.0, 0.0)
colorPt d r@(Ray _ dir) b o l = if (isNothing i) then b else clip $ shadeColor <+> reflectColor <+> refractColor
  where
    shadeColor = foldl (<+>) (0.0,0.0,0.0) (map (shadePt (fromJust i) dir o) l)
    reflectColor = if (reflCoef == 0.0) then (0.0, 0.0, 0.0)
                                        else (reflectPt (d-1) (fromJust i) dir o l) *> reflCoef
    refractColor = if (refrCoef == 0.0) then (0.0, 0.0, 0.0)
                                        else (refractPt (d-1) (fromJust i) dir b o l) *> refrCoef
    i = intersect r o
    (Texture _ reflCoef _ refrCoef _) = intText i

rayTracePt :: Int -> Scene -> Point3D -> Color
rayTracePt d (Scene (Camera eye _) b o l) p = colorPt d (Ray p (mkNormVect eye p)) b o l

rayTrace :: Int -> Resolution -> Scene -> Image
rayTrace d r s@(Scene (Camera _ dim) _ _ _) = (rayTracePt d s) . (mapToWin r dim)

data Scene = Scene Camera Color [TexturedObject] [Light]

getImage :: Int -> Resolution -> Scene -> [Color]
getImage d r@(rx,ry) s = [image (fromIntegral x,fromIntegral (-y)) | y <- [-(ry-1)..0], x <- [0..(rx-1)]]
  where
  image = rayTrace d r s

createPPM :: Resolution -> [Color] -> String
createPPM (w,h) colors = ("P3\n"++) . shows w . (' ':) . shows h . ("\n255\n"++) . stringify colors $ ""
  where stringify = flip $ foldr showC where
                                 showC (r,g,b) = shows (round (r*255)) . (' ':) . shows (round (g*255)). (' ':) . shows (round (b*255)) . (' ':)

-- 1) GUI +-
-- 2) dop. obj -
-- 3) parsing +
-- 4) seperate proj. on modules -

rayfunc :: IO ()
rayfunc = do
  scana <- scene
  writeFile "result.ppm" (createPPM (500, 400) (getImage 3 (500, 400) scana))


--------------------------------------------------------------PARSER-----------------------------------------------------------------


------------------------------------------------------------scanning lights----------------------------------------------------------
scanfLights :: Handle -> FilePath -> Int -> [Light] -> IO ([Light])
scanfLights h file 0 xs = do
  return(xs)
scanfLights h file n xs = do
  str <- hGetLine h
  let a = read str
  str <- hGetLine h
  let x = read str
  str <- hGetLine h
  let y = read str
  str <- hGetLine h
  let z = read str
  str <- hGetLine h
  let p1 = read str
  str <- hGetLine h
  let p2 = read str
  str <- hGetLine h
  let p3 = read str
  if (a == 1) then scanfLights h file (n-1) ((PointLight (x,y,z) (p1,p2,p3)):[] ++ xs) else scanfLights h file (n-1) ((AmbientLight (p1,p2,p3)):[] ++ xs)

getLights :: Handle -> FilePath -> IO ([Light])
getLights h1 inp = do
        str <- hGetLine h1
        let n = read str :: Int
        scanfLights h1 inp n []
-------------------------------------------------------------scanning Objects-------------------------------------------------------
scanfTexturedObjects :: Handle -> FilePath -> Int -> [TexturedObject] -> IO ([TexturedObject])

scanfTexturedObjects h file 0 xs = do
	return (xs)

scanfTexturedObjects h file n xs = do
	str <- hGetLine h
 	let obj = read str
 	if (obj == 3) then getTriangle h file n xs else buffunc obj h file n xs

getTriangle :: Handle -> FilePath -> Int -> [TexturedObject] -> IO ([TexturedObject])
getTriangle h file n xs = do
	str <- hGetLine h
 	let tex = read str
 	str <- hGetLine h
 	let x1 = read str
 	str <- hGetLine h
 	let y1 = read str
 	str <- hGetLine h
 	let z1 = read str
 	str <- hGetLine h
 	let x2 = read str
 	str <- hGetLine h
 	let y2 = read str
 	str <- hGetLine h
 	let z2 = read str
 	str <- hGetLine h
 	let x3 = read str
 	str <- hGetLine h
 	let y3 = read str
 	str <- hGetLine h
 	let z3 = read str
 	str <- hGetLine h
 	let t1 = read str
 	str <- hGetLine h
 	let t2 = read str
 	str <- hGetLine h
 	let t3 = read str
 	str <- hGetLine h
 	let t4 = read str
 	str <- hGetLine h
 	let tint = read str
 	str <- hGetLine h
 	let t5 = read str
 	str <- hGetLine h
 	let t6 = read str
 	let trito = (Triangle ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)), Texture ( Solid (t1,t2,t3) ) t4 tint t5 t6)
 	if (tex == 1) then scanfTexturedObjects h file (n-1) (trito:[] ++ xs) else error "perlin is not ready"

buffunc :: Integer -> Handle -> FilePath -> Int -> [TexturedObject] -> IO ([TexturedObject])
buffunc obj h file n xs = do
	str <- hGetLine h
 	let tex = read str
	str <- hGetLine h
 	let c1 = read str
 	str <- hGetLine h
 	let c2 = read str
 	str <- hGetLine h
 	let c3 = read str
 	str <- hGetLine h
 	let c4 = read str
 	str <- hGetLine h
 	let t1 = read str
 	str <- hGetLine h
 	let t2 = read str
 	str <- hGetLine h
 	let t3 = read str
 	str <- hGetLine h
 	let t4 = read str
 	str <- hGetLine h
 	let tint = read str
 	str <- hGetLine h
 	let t5 = read str
 	str <- hGetLine h
 	let t6 = read str
 	let ss = (Sphere c1 (c2,c3,c4),Texture ( Solid (t1,t2,t3) ) t4 tint t5 t6)
 	let ps = (Plane (c1,c2,c3,c4),Texture ( Solid (t1,t2,t3) ) t4 tint t5 t6)
 	if(obj == 1) then if (tex == 1) then scanfTexturedObjects h file (n-1) (ps:[] ++ xs) else error "perlin is not ready" else if (tex == 1) then scanfTexturedObjects h file (n-1) (ss:[] ++ xs) else error "perlin is not ready"

getTexturedObjects :: Handle -> FilePath -> IO ([TexturedObject])
getTexturedObjects h1 inp = do
          str <- hGetLine h1
          let n = read str :: Int
          scanfTexturedObjects h1 inp n []
-----------------------------------------------------------reading background----------------------------------------------------------------

getBack ::Handle -> FilePath -> IO (Color)
getBack h inp = do
                str <- hGetLine h
                let r = read str
                str <- hGetLine h
                let g = read str
                str <- hGetLine h
                let b = read str
                return(r,g,b)

------------------------------------------------------------getting camera--------------------------------------------------------------------

getCamera ::Handle -> FilePath -> IO (Camera)
getCamera h inp = do
  str <- hGetLine h
  let camx = read str
  str <- hGetLine h
  let camy = read str
  str <- hGetLine h
  let camz = read str
  str <- hGetLine h
  let dim1 = read str
  str <- hGetLine h
  let dim2 = read str
  return(Camera (camx,camy,camz) (dim1,dim2))
----------------------------------------------------------------------------------------------------------------------------------------------
scene :: IO (Scene)
scene = do
  hPutStr stderr "Type an inputfilename: "
  inp <- getLine
  src <- readFile inp
  h <- openFile inp ReadMode
  light <- getLights h inp
  obj <- getTexturedObjects h inp
  color <- getBack h inp
  cam <- getCamera h inp
  return $ Scene cam color obj light

--scene = Scene camera background objects lights
