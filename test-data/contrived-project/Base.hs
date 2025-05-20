{-# LANGUAGE NoFieldSelectors #-}
module Base where

data Person = Person
  { name :: String
  , age :: Int
  }

data Address = Address
  { street :: String
  , city :: String
  , country :: String
  }

type PersonWithAddress = (Person, Address)

-- Some functions
getFullName :: Person -> String
getFullName (Person n _) = n

getFullAddress :: Address -> String
getFullAddress (Address s c co) = s ++ ", " ++ c ++ ", " ++ co

-- Some type classes and instances
class Describable a where
  describe :: a -> String

instance Describable Person where
  describe (Person n a) = "Person named " ++ n ++ " aged " ++ show a

instance Describable Address where
  describe addr = "Address: " ++ getFullAddress addr 