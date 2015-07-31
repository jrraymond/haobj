module Types where

--TODO switch to arrays/vectors

data V3 = V3 !Float !Float !Float deriving (Eq,Read,Show)

--TODO incomplete, at least need to add texture maps
data Material = Material { mNs    :: !Float   --specular exponent
                         , mKa    :: !V3      --ambient color
                         , mKd    :: !V3      --diffuse color
                         , mKs    :: !V3      --specular color
                         , mNi    :: !Float   --optical density (refraction)
                         , mD     :: !Float    --dissolved (halo factor)
                         , mIllum :: !Int     --illumination model
                         } deriving (Eq,Read,Show)

data Mesh = Mesh { mVertices  :: [V3]
                 , mNormals   :: [V3]
                 , mFaces     :: [(Material,Bool,[Face])]
                 } deriving (Eq,Read,Show)

newtype Face = Face [FaceV] deriving (Eq,Read,Show)

data FaceV = FaceV { fvVertex  :: !Int
                   , fvTexture :: !Int
                   , fvNormal  :: !Int 
                   } deriving (Eq,Show,Read)


{- Illumination models:
-     0. Color on and Ambient off
-     1. Color on and Ambient on
-     2. Highlight on
-     3. Reflection on and Ray trace on
-     4. Transparency: Glass on, Reflection: Ray trace on
-     5. Reflection: Fresnel on and Ray trace on
-     6. Transparency: Refraction on, Reflection: Fresnel off and Ray trace on
-     7. Transparency: Refraction on, Reflection: Fresnel on and Ray trace on
-     8. Reflection on and Ray trace off
-     9. Transparency: Glass on, Reflection: Ray trace off
-     10. Casts shadows onto invisible surfaces
-}
