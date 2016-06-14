module SubHask.Algebra.Logic
    where

import Control.Monad
import qualified Prelude as P
import Test.QuickCheck.Gen (suchThat,oneof)

import SubHask.Algebra
import SubHask.Category
import SubHask.Compatibility.Base
import SubHask.SubType
import SubHask.Internal.Prelude
import SubHask.TemplateHaskell.Deriving

class (Ord r, Ring r) => OrdRing_ r
instance (Ord r, Ring r) => OrdRing_ r

--------------------------------------------------------------------------------

-- | The Goedel fuzzy logic is one of the simpler fuzzy logics.
-- In particular, it is an example of a Heyting algebra that is not also a Boolean algebra.
--
-- See the <plato.stanford.edu/entries/logic-fuzzy standford encyclopedia of logic>
type Goedel = Goedel_ Rational

newtype Goedel_ r = Goedel_ r

deriveHierarchyFiltered ''Goedel_ [ ''Eq_ ] [ ''Arbitrary ]

instance (OrdRing_ r, Arbitrary r) => Arbitrary (Goedel_ r) where
    arbitrary = fmap Goedel_ $ arbitrary `suchThat` ((>=0) && (<=1))

instance OrdRing_ r => POrd_ (Goedel_ r) where
    inf (Goedel_ r1) (Goedel_ r2) = Goedel_ $ min r1 r2

instance OrdRing_ r => Lattice_ (Goedel_ r) where
    sup (Goedel_ r1) (Goedel_ r2) = Goedel_ $ max r1 r2

instance OrdRing_ r => Ord_ (Goedel_ r)

instance OrdRing_ r => MinBound_  (Goedel_ r) where
    minBound = Goedel_ 0

instance OrdRing_ r => Bounded  (Goedel_ r) where
    maxBound = Goedel_ 1

instance OrdRing_ r => Heyting (Goedel_ r) where
--     (Goedel_ r1)==>(Goedel_ r2) = if r1 <= r2 then Goedel_ 1 else Goedel_ (1 - r1 + r2)
    (Goedel_ r1)==>(Goedel_ r2) = if r1 <= r2 then Goedel_ 1 else Goedel_ r2

---------------------------------------

-- | H3 is the smallest Heyting algebra that is not also a boolean algebra.
-- In addition to true and false, there is a value to represent whether something's truth is unknown.
-- AFAIK it has no real applications.
--
-- See <https://en.wikipedia.org/wiki/Heyting_algebra#Examples wikipedia>
data H3
    = HTrue
    | HFalse
    | HUnknown
    deriving (Read,Show)

instance NFData H3 where
    rnf HTrue = ()
    rnf HFalse = ()
    rnf HUnknown = ()

instance Arbitrary H3 where
    arbitrary = oneof $ map return [HTrue, HFalse, HUnknown]

type instance Logic H3 = Bool

instance Eq_ H3 where
    HTrue    == HTrue    = True
    HFalse   == HFalse   = True
    HUnknown == HUnknown = True
    _        == _        = False

instance POrd_ H3 where
    inf HTrue    HTrue    = HTrue
    inf HTrue    HUnknown = HUnknown
    inf HUnknown HTrue    = HUnknown
    inf HUnknown HUnknown = HUnknown
    inf _        _        = HFalse

instance Lattice_ H3 where
    sup HFalse    HFalse   = HFalse
    sup HFalse    HUnknown = HUnknown
    sup HUnknown  HFalse   = HUnknown
    sup HUnknown  HUnknown = HUnknown
    sup _         _        = HTrue

instance Ord_ H3

instance MinBound_ H3 where
    minBound = HFalse

instance Bounded H3 where
    maxBound = HTrue

instance Heyting H3 where
    _        ==> HTrue    = HTrue
    HFalse   ==> _        = HTrue
    HTrue    ==> HFalse   = HFalse
    HUnknown ==> HUnknown = HTrue
    HUnknown ==> HFalse   = HFalse
    _        ==> _        = HUnknown

---------------------------------------

-- | K3 stands for Kleene's 3-valued logic.
-- In addition to true and false, there is a value to represent whether something's truth is unknown.
-- K3 is an example of a logic that is neither Boolean nor Heyting.
--
-- See <http://en.wikipedia.org/wiki/Three-valued_logic wikipedia>.
--
-- FIXME: We need a way to represent implication and negation for logics outside of the Lattice hierarchy.
data K3
    = KTrue
    | KFalse
    | KUnknown
    deriving (Read,Show)

instance NFData K3 where
    rnf KTrue = ()
    rnf KFalse = ()
    rnf KUnknown = ()

instance Arbitrary K3 where
    arbitrary = oneof $ map return [KTrue, KFalse, KUnknown]

type instance Logic K3 = Bool

instance Eq_ K3 where
    KTrue    == KTrue    = True
    KFalse   == KFalse   = True
    KUnknown == KUnknown = True
    _        == _        = False

instance POrd_ K3 where
    inf KTrue    KTrue    = KTrue
    inf KTrue    KUnknown = KUnknown
    inf KUnknown KTrue    = KUnknown
    inf KUnknown KUnknown = KUnknown
    inf _        _        = KFalse

instance Lattice_ K3 where
    sup KFalse    KFalse   = KFalse
    sup KFalse    KUnknown = KUnknown
    sup KUnknown  KFalse   = KUnknown
    sup KUnknown  KUnknown = KUnknown
    sup _         _        = KTrue

instance Ord_ K3

instance MinBound_ K3 where
    minBound = KFalse

instance Bounded K3 where
    maxBound = KTrue

--------------------------------------------------------------------------------
-- | A Boolean algebra is a special type of Ring.
-- Their applications (set-like operations) tend to be very different than Rings, so it makes sense for the class hierarchies to be completely unrelated.
-- The "Boolean2Ring" type, however, provides the correct transformation.

newtype Boolean2Ring b = Boolean2Ring b

deriveHierarchy ''Boolean2Ring [ ''Boolean ]

mkBoolean2Ring :: Boolean b => b -> Boolean2Ring b
mkBoolean2Ring = Boolean2Ring

instance (IsMutable b, Boolean b, ValidLogic b) => Semigroup (Boolean2Ring b) where
    (Boolean2Ring b1)+(Boolean2Ring b2) = Boolean2Ring $ (b1 || b2) && not (b1 && b2)

instance (IsMutable b, Boolean b, ValidLogic b) => Abelian (Boolean2Ring b)

instance (IsMutable b, Boolean b, ValidLogic b) => Monoid (Boolean2Ring b) where
    zero = Boolean2Ring $ false

instance (IsMutable b, Boolean b, ValidLogic b) => Cancellative (Boolean2Ring b) where
    (-)=(+)

instance (IsMutable b, Boolean b, ValidLogic b) => Group (Boolean2Ring b) where
    negate = id

instance (IsMutable b, Boolean b, ValidLogic b) => Rg (Boolean2Ring b) where
    (Boolean2Ring b1)*(Boolean2Ring b2) = Boolean2Ring $ b1 && b2

instance (IsMutable b, Boolean b, ValidLogic b) => Rig (Boolean2Ring b) where
    one = Boolean2Ring $ true

instance (IsMutable b, Boolean b, ValidLogic b) => Ring (Boolean2Ring b)
