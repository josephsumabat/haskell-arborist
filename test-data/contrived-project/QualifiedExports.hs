module QualifiedExports
  ( Person
  , Address
  , PersonWithAddress
  , getFullName
  , getFullAddress
  , describe
  ) where

import qualified Base as B
import Base (Person(..), Address(..), PersonWithAddress)

-- Re-export the types
type Person = B.Person
type Address = B.Address
type PersonWithAddress = B.PersonWithAddress

-- Re-export the functions
getFullName :: Person -> String
getFullName = B.getFullName

getFullAddress :: Address -> String
getFullAddress = B.getFullAddress

-- Re-export the type class method
describe :: B.Describable a => a -> String
describe = B.describe 