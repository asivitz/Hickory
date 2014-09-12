module Engine.Component.Entity where
{-(  Entity,-}
   {-genEntity,-}
   {-genEntities,-}
   {-newEntitySet,-}
   {-deleteEntity) where-}
   
import Data.List
import qualified Data.Set as Set
import Data.Hashable

newtype Entity = Entity { getEntity :: Int } deriving (Eq, Ord)

instance Show Entity where
      show (Entity e) = 'e' : show e

instance Data.Hashable.Hashable Entity where
      hashWithSalt s a = hashWithSalt s (getEntity a)

newtype EntitySet = EntitySet { getSet :: Set.Set Entity }

instance Show EntitySet where  
      show (EntitySet s) = show $ Set.toList s

newEntitySet :: EntitySet
newEntitySet = EntitySet Set.empty

addEntity :: EntitySet -> Entity -> EntitySet
addEntity (EntitySet s) e = EntitySet (e `Set.insert` s)

addEntities :: EntitySet -> [Entity] -> EntitySet
addEntities (EntitySet s) xs = EntitySet $ s `Set.union` Set.fromList xs

genEntity :: EntitySet -> (Entity, EntitySet)
genEntity es = let s = getSet es
                   Just e = find (\x -> Set.notMember (Entity x) s) [1..]
                   ent = Entity e
                   in (ent, addEntity es ent)

genEntities :: EntitySet -> Int -> ([Entity], EntitySet)
genEntities es num = let s = getSet es
                         lst = take num $ filter (\x -> Set.notMember (Entity x) s) [1..]
                         entlst = map Entity lst
                         in (entlst, addEntities es entlst)

deleteEntity :: EntitySet -> Entity -> EntitySet
deleteEntity (EntitySet s) ent = EntitySet (ent `Set.delete` s)

deleteEntities :: EntitySet -> [Entity] -> EntitySet
deleteEntities (EntitySet s) lst = EntitySet (s `Set.difference` Set.fromList lst)
