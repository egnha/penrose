-- | "Shapes" contains all geometric primitives that Penrose supports

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Shapes where
-- module Shapes (Obj, Obj) where
import Utils
import Data.Aeson
import Data.Monoid ((<>))
import GHC.Generics

type Name = String

class Located a b where
      getX :: a -> b
      getY :: a -> b
      setX :: b -> a -> a
      setY :: b -> a -> a

class Selectable a where
      select :: a -> a
      deselect :: a -> a
      selected :: a -> Bool

class Sized a where
      getSize :: a -> Float
      setSize :: Float -> a -> a

class Named a where
      getName :: a -> Name
      setName :: Name -> a -> a

data BBox = BBox {
    cx :: Float,
    cy :: Float,
    h :: Float,
    w :: Float
} deriving (Show, Eq, Generic)
instance ToJSON BBox
instance FromJSON BBox

-- TODO: resolve name clashes
data Color a = Color {
    cr :: a,
    cg :: a,
    cb :: a,
    ca :: a
} deriving (Show, Eq, Generic)

instance (Real a, Floating a, Show a, Ord a, FromJSON a) => FromJSON (Color a) where
    parseJSON = withObject "Color" $ \v -> makeColor
           <$> v .: "r"
           <*> v .: "g"
           <*> v .: "b"
           <*> v .: "a"

instance (Real a, Floating a, Show a, Ord a, ToJSON a) => ToJSON (Color a) where
    -- this generates a Value
    toJSON c =
        let (r, g, b, a) = rgbaOfColor  c in
        object ["r" .= r, "g" .= g, "b" .= b, "a" .= a]
    toEncoding c =
        let (r, g, b, a) = rgbaOfColor  c in
        pairs ("r" .= r <> "g" .= g <> "b" .= b <> "a" .= a)

rgbaOfColor :: (Real a, Floating a, Show a, Ord a) => Color a -> (a, a, a, a)
rgbaOfColor c = (r2f $ cr c, r2f $ cg c, r2f $ cb c, r2f $ ca c)

makeColor :: (Real a, Floating a, Show a, Ord a) => a -> a -> a -> a -> Color a
makeColor r g b a = Color { cr = r, cg = g, cb = b, ca = a }


--------------------------------------------------------------------------------
-- Polymorphic versions of the primitives

-- data Obj a =
--     SolidArrow
--         { startx :: a
--         , starty :: a
--         , endx :: a
--         , endy :: a
--         , thickness :: a -- the maximum thickness, i.e. the thickness of the head
--         , sel :: Bool -- is the circle currently selected? (mouse is dragging it)
--         , name :: String
--         , color :: Color }
--     | Circ
--         { x :: a
--         , y :: a
--         , r :: a
--         , sel :: Bool -- is the circle currently selected? (mouse is dragging it)
--         , name :: String
--         , color :: Color }
--     | Label
--         { x :: a
--         , y :: a
--         , w :: a
--         , h :: a
--         , text :: String
--         , sel :: Bool -- selected label
--         , name :: String }
--     | Pt
--         { x :: a
--         , y :: a
--         , sel :: Bool
--         , name :: String }
--     | Square
--         { x :: a
--         , y :: a
--         , side :: a
--         , ang  :: Float -- angle for which the obj is rotated
--         , sel :: Bool
--         , name :: String
--         , color :: Color }
--         deriving (Eq, Show)

data Obj a
    = C (Circ a)
    | E (Ellipse a)
    | L (Label a)
    | P (Pt a)
    | S (Square a)
    | A (SolidArrow a)
    | CB (CubicBezier a)
    deriving (Eq, Show)

data SolidArrow a = SolidArrow {
    startx    :: a,
    starty    :: a,
    endx      :: a,
    endy      :: a,
    thickness :: a, -- the maximum thickness, i.e. the thickness of the head
    selsa     :: Bool, -- is the circle currently selected? (mouse is dragging it)
    namesa    :: String,
    colorsa   :: Color a
} deriving (Eq, Show)

data Circ a = Circ {
    xc     :: a,
    yc     :: a,
    r      :: a,
    selc   :: Bool, -- is the circle currently selected? (mouse is dragging it)
    namec  :: String,
    colorc :: Color a
} deriving (Eq, Show)

data Ellipse a = Ellipse {
    xe :: a,
    ye :: a,
    rx :: a,
    ry :: a,
    namee  :: String,
    colore :: Color a
} deriving (Eq, Show)

data Label a = Label { xl :: a
                       , yl :: a
                       , wl :: a
                       , hl :: a
                       , textl :: String
                       , sell :: Bool -- selected label
                       , namel :: String }
                       deriving (Eq, Show)

data Pt a = Pt { xp :: a
                 , yp :: a
                 , selp :: Bool
                 , namep :: String }
                 deriving (Eq, Show)

data Square a  = Square { xs :: a
                     , ys :: a
                     , side :: a
                     , ang  :: Float -- angle for which the obj is rotated
                     , sels :: Bool
                     , names :: String
                     , colors :: Color a }
                     deriving (Eq, Show)

data CubicBezier a = CubicBezier {
    pathcb           :: [(a, a)],
    namecb           :: String,
    stylecb          :: String,
    colorcb          :: Color a
} deriving (Eq, Show)

instance Named (SolidArrow a) where
         getName = namesa
         setName x sa = sa { namesa = x }

instance Named (Circ a) where
         getName = namec
         setName x c = c { namec = x }

instance Named (Ellipse a) where
         getName = namee
         setName x c = c { namee = x }

instance Named (Square a) where
         getName = names
         setName x s = s { names = x }

instance Named (Label a) where
         getName = namel
         setName x l = l { namel = x }

instance Named (Pt a) where
         getName = namep
         setName x p = p { namep = x }

instance Named (CubicBezier a) where
         getName = namecb
         setName x cb = cb { namecb = x }

instance Named (Obj a) where
         getName o = case o of
                 C c   -> getName c
                 E c   -> getName c
                 L l   -> getName l
                 P p   -> getName p
                 S s   -> getName s
                 A a   -> getName a
                 CB cb -> getName cb
         setName x o = case o of
                C c   -> C $ setName x c
                S s   -> S $ setName x s
                L l   -> L $ setName x l
                P p   -> P $ setName x p
                A a   -> A $ setName x a
                CB cb -> CB $ setName x cb
--
--
instance Located (Circ a) a where
         getX = xc
         getY = yc
         setX x c = c { xc = x }
         setY y c = c { yc = y }

instance Located (Ellipse a) a where
         getX = xe
         getY = ye
         setX x e = e { xe = x }
         setY y e = e { ye = y }

instance Located (Square a) a where
         getX = xs
         getY = ys
         setX x s = s { xs = x }
         setY y s = s { ys = y }

instance Located (SolidArrow a) a where
         getX  = startx
         getY  = starty
         setX x c = c { startx = x } -- TODO
         setY y c = c { starty = y }

instance Located (Label a) a where
         getX = xl
         getY = yl
         setX x l = l { xl = x }
         setY y l = l { yl = y }

instance Located (Pt a) a where
         getX = xp
         getY = yp
         setX x p = p { xp = x }
         setY y p = p { yp = y }

-- TODO: Added context for max and min functions. Consider rewriting the whole `Located` interface. For general shapes, simply setX and getX does NOT make sense.
instance (Real a, Floating a, Show a, Ord a) => Located (CubicBezier a) a where
         getX c   = let xs = map fst $ pathcb c in maximum xs - minimum xs
         getY c   = let ys = map snd $ pathcb c in maximum ys - minimum ys
         setX x c = let xs = map fst $ pathcb c
                        dx = x - (maximum xs - minimum xs) in
                        c { pathcb = map (\(xx, yy) -> (xx + dx, yy)) $ pathcb c }
         setY y c = let ys = map snd $ pathcb c
                        dy = y - (maximum ys - minimum ys) in
                        c { pathcb = map (\(xx, yy) -> (xx, yy - dy)) $ pathcb c }

instance Located (Obj a) a  where
         getX o = case o of
             C c -> xc c
             E e -> xe e
             L l -> xl l
             P p -> xp p
             S s -> xs s
             A a -> startx a
         getY o = case o of
             C c -> yc c
             E e -> ye e
             L l -> yl l
             P p -> yp p
             S s -> ys s
             A a -> starty a
         setX x o = case o of
             C c -> C $ setX x c
             E e -> E $ setX x e
             L l -> L $ setX x l
             P p -> P $ setX x p
             S s -> S $ setX x s
             A a -> A $ setX x a
         setY y o = case o of
             C c -> C $ setY y c
             E e -> E $ setY y e
             L l -> L $ setY y l
             P p -> P $ setY y p
             S s -> S $ setY y s
             A a -> A $ setY y a
