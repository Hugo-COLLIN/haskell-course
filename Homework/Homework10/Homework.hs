{-
-- Question 1 --
Continuing with the logistics software of the lesson:
 1. After using the `Container` type class for a while, you realize that it might need a few adjustments:
  	- First, write down the `Container` type class and its instances, same as we did in the lesson
  	  (try to do it without looking and check at the end or if you get stuck).
  	- Then, add a function called `unwrap` that gives you back the value inside a container.
 2. Create an instance for the `MailedBox` data type.
 	- The MailedBox data type represents a box sent through the mail.
 	- The parameter `t` is a tag with a person's identifier
 	- The parameter `d` is the person's details (address,etc).
 	- The parameter `a` is the content of the MailedBox
-}
data Box a       = Empty          | Has a          deriving (Show)
data Present t a = EmptyPresent t | PresentFor t a deriving (Show)

class Container c where
    isEmpty :: c a -> Bool
    contains :: (Eq a) => c a -> a -> Bool
    replace :: c a -> b -> c b
    unwrap :: c a -> Maybe a

instance Container Box where
    isEmpty Empty = True
    isEmpty _ = False

    contains (Has x) y = x == y
    contains Empty   _ = False

    replace _ = Has

    unwrap (Has x) = Just x
    unwrap Empty = Nothing

instance Container (Present t) where

    isEmpty (EmptyPresent _) = True
    isEmpty _                = False

    contains (PresentFor _ x) y = x == y
    contains (EmptyPresent _) _ = False

    replace (PresentFor tag _) x = PresentFor tag x
    replace (EmptyPresent tag) x = PresentFor tag x

    unwrap (PresentFor _ x) = Just x
    unwrap (EmptyPresent _) = Nothing

-- 2
data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a

instance Container (MailedBox t d) where
    isEmpty (EmptyMailBox _ _) = True
    isEmpty _ = False

    contains (MailBoxTo _ _ x) y = x == y
    contains (EmptyMailBox _ _) _ = False

    replace (MailBoxTo tag details _) x = MailBoxTo tag details x
    replace (EmptyMailBox tag details) x = MailBoxTo tag details x

    unwrap (MailBoxTo _ _ x) = Just x
    unwrap (EmptyMailBox _ _) = Nothing




-- Question 2 --
-- Create instances for Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

data Position = Intern | Junior | Senior | Manager | Chief
    deriving (Show, Eq, Ord)

data Experience = Programming | Managing | Leading
    deriving (Show, Eq, Ord)

type Address = String

data Salary = USD Double | EUR Double
    deriving (Show, Eq)

instance Ord Salary where
    compare (USD s1) (USD s2) = compare s1 s2
    compare (EUR s1) (EUR s2) = compare s1 s2
    compare (USD _) (EUR _) = error "Not the same currency"
    compare (EUR _) (USD _) = error "Not the same currency"

data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address
  deriving (Eq)

instance Show Relationship where
  show (Contractor pos exp sal addr) =
    "Contractor " ++ show pos ++ " " ++ show exp ++ " " ++ show sal ++ " " ++ addr
  show (Employee pos exp sal addr) =
    "Employee " ++ show pos ++ " " ++ show exp ++ " " ++ show sal ++ " " ++ addr

instance Ord Relationship where
  compare r1 r2 =
    case (r1, r2) of
      -- Employees are considered "greater than" Contractors
      (Employee {}, Contractor {}) -> GT
      (Contractor {}, Employee {}) -> LT
      -- Same constructor - compare by position first
      (_, _) -> compare (getPos r1) (getPos r2)
    where
      getPos (Contractor p _ _ _) = p
      getPos (Employee p _ _ _) = p

data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  } deriving (Show, Eq, Ord)

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3

-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

-- Team memeber experience in years
newtype Exp = Exp Double deriving (Show, Eq, Ord)

instance Num Exp where
    (Exp x) + (Exp y) = Exp (x + y)
    (Exp x) * (Exp y) = Exp (x * y)
    abs (Exp x) = Exp (abs x)
    signum (Exp x) = Exp (signum x)
    fromInteger x = Exp (fromInteger x)
    negate (Exp x) = Exp (negate x)

-- Team memeber data
type TeamMember = (String, Exp)

-- List of memeber of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]

-- Function to check the combined experience of the team
-- This function applied to `team` using GHCi should work
combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0
