import Data.Array

data Boundary = Boundary {
  north :: Bool, 
    east :: Bool,
    south :: Bool,
    west :: Bool
}

firstMap = listArray ((1, 1), (6, 6))
  [ Boundary True False False True,
    Boundary True False True False,
    Boundary True True False False,
    Boundary True False False True,
    Boundary True False True False,
    Boundary True True True False,
    
    Boundary False True False True,
    Boundary True False False True,
    Boundary False True True False,
    Boundary False False True True,
    Boundary True False True False,
    Boundary True True False False,
    
    Boundary False True False True,
    Boundary False False True True,
    Boundary True True False False,
    Boundary True False False True,
    Boundary True False True False,
    Boundary False True False False,
    
    Boundary False True False True,
    Boundary True False True True,
    Boundary False False True False,
    Boundary False True True False,
    Boundary True False True True,
    Boundary False True False False,
    
    Boundary False False False True,
    Boundary True False True False,
    Boundary True True False False,
    Boundary True False False True,
    Boundary True True True False,
    Boundary False True False True,
  
    Boundary False False True True,
    Boundary True True True False,
    Boundary False False True True,
    Boundary False True True False,
    Boundary True False True True,
    Boundary False True True False
  ]

secondMap = listArray ((1, 1), (6, 6))
[
Boundary  True False True True,
Boundary True False False False,
Boundary True True True False,
Boundary True False False True,
Boundary True False False False,
Boundary True True True False,
  
Boundary  True False False True,
Boundary False True True False,
Boundary True False False True,
Boundary False True True False,
Boundary False False True True,
Boundary True True False False,
  
Boundary  False True False True,
Boundary True False False True,
Boundary False True True False,
Boundary True False False True,
Boundary True False True False,
Boundary False True False False,
  
Boundary  False False False True,
Boundary False True True False,
Boundary True False False True,
Boundary False True True False,
Boundary True True False True,
Boundary False True False True,
  
Boundary  False True False True,
Boundary True True False True,
Boundary False True False True,
Boundary True False False True,
Boundary False True True False,
Boundary False True False True,
  
Boundary  False True True True,
Boundary False False True True,
Boundary False True True False,
Boundary False False True True,
Boundary True False True False,
Boundary False True True False

  ]

thirdMap = listArray ((1, 1), (6, 6))
[
Boundary  True False False True,
Boundary True False True False,
Boundary True True False False,
Boundary True True False True,
Boundary True False False True,
Boundary True True False False,

Boundary  False True True True,
Boundary True True False True,
Boundary False True False True,
Boundary False False True True,
Boundary False True True False,
Boundary False True False True,

Boundary  True False False True,
Boundary False True False False,
Boundary False True False True,
Boundary True False False True,
Boundary True True False False,
Boundary False True False True,

Boundary  False True False True,
Boundary False True False True,
Boundary False True False True,
Boundary False True False True,
Boundary False True False True,
Boundary False True False True,

Boundary  False True False True,
Boundary False False True True,
Boundary False True True False,
Boundary False True False True,
Boundary False True False True,
Boundary False True False True,

Boundary  False False True True,
Boundary True False True False,
Boundary True False True False,
Boundary False True True False,
Boundary False False True True,
Boundary False True True False]


fourthMap = listArray ((1, 1), (6, 6))
[

Boundary  True False False True,
Boundary True True False False,
Boundary True False True True,
Boundary True False True False,
Boundary True False True False,
Boundary True True False False,

Boundary  False True False True,
Boundary False True False True,
Boundary True False False True,
Boundary True False True False,
Boundary True False True False,
Boundary False True False False,

Boundary  False True False True,
Boundary False False True True,
Boundary False True True False,
Boundary True False False True,
Boundary True True True False,
Boundary False True False True,

Boundary  False True False True,
Boundary True False True True,
Boundary True False True False,
Boundary False False True False,
Boundary True False True False,
Boundary False True False False,

Boundary  False False False True,
Boundary True False True False,
Boundary True False True False,
Boundary True False True False,
Boundary True True False False,
Boundary False True False True,

Boundary  False False True True,
Boundary True False True False,
Boundary True True True False,
Boundary True False True True,
Boundary False True True False,
Boundary False True True True]













fifthMap = listArray ((1, 1), (6, 6))
[

Boundary  True False True True,
Boundary True False True False,
Boundary True False True False,
Boundary True False True False,
Boundary True False False False,
Boundary True True False False,

Boundary  True False False True,
Boundary True False True False,
Boundary True False True False,
Boundary True False False False,
Boundary False True True False,
Boundary False True True True,

Boundary  False False False True,
Boundary True True False False,
Boundary True False True True,
Boundary False True True False,
Boundary True False False True,
Boundary True True False False,

Boundary  False True False True,
Boundary False False True True,
Boundary True False True False,
Boundary True True False False,
Boundary False True True True,
Boundary False True False True,

Boundary  False True False True,
Boundary True False False True,
Boundary True False True False,
Boundary False False True False,
Boundary True True True False,
Boundary False True False True,

Boundary  False True True True,
Boundary False False True True,
Boundary True False True False,
Boundary True False True False,
Boundary True False True False,
Boundary False True True False]


sixthMap = listArray ((1, 1), (6, 6))
[

Boundary  True True False True,
Boundary True False False True,
Boundary True True False False,
Boundary True False True True,
Boundary True False False False,
Boundary True True False False,

Boundary  False True False True,
Boundary False True False True,
Boundary False True False True,
Boundary True False False True,
Boundary False True True False,
Boundary False True False True,

Boundary  False False False True,
Boundary False True True False,
Boundary False True True True,
Boundary False True False True,
Boundary True False False True,
Boundary False True True False,

Boundary  False False True True,
Boundary True True False False,
Boundary True False False True,
Boundary False True False False,
Boundary False True False True,
Boundary True True False True,

Boundary  True False False True,
Boundary False True True False,
Boundary False True True True,
Boundary False True False True,
Boundary False False True True,
Boundary False True False False,

Boundary  False False True True,
Boundary True False True False,
Boundary True False True False,
Boundary False True True False,
Boundary True False True True,
Boundary False True True False]









seventhMap = listArray ((1, 1), (6, 6))
[

Boundary  True False False True,
Boundary True False True False,
Boundary True False True False,
Boundary True True False False,
Boundary True False False True,
Boundary True True False False,

Boundary  False True False True,
Boundary True False False True,
Boundary True True True False,
Boundary False False True True,
Boundary False True True False,
Boundary False True False True,

Boundary  False False True True,
Boundary False True True False,
Boundary True False False True,
Boundary True True True False,
Boundary True False False True,
Boundary False True True False,

Boundary  True False False True,
Boundary True True False False,
Boundary False False False True,
Boundary True False True False,
Boundary False True True False,
Boundary True True False True,

Boundary  False True False True,
Boundary False True True True,
Boundary False False True True,
Boundary True False True False,
Boundary True True False False,
Boundary False True False True,

Boundary  False False True True,
Boundary True False True False,
Boundary True False True False,
Boundary True False True False,
Boundary False False True False,
Boundary False True True False]



eighthMap = listArray ((1, 1), (6, 6))
[
Boundary  True True False True,
Boundary True False False True,
Boundary True False True False,
Boundary True True False False,
Boundary True False False True,
Boundary True True False False,

Boundary  False False False True,
Boundary False False True False,
Boundary True True True False,
Boundary False False True True,
Boundary False True True False,
Boundary False True False True,

Boundary  False True False True,
Boundary True False False True,
Boundary True False True False,
Boundary True False True False,
Boundary True True False False,
Boundary False True False True,

Boundary  False True False True,
Boundary False False True True,
Boundary True True False False,
Boundary True False True True,
Boundary False False True False,
Boundary False True True False,

Boundary  False True False True,
Boundary True True False True,
Boundary False False True True,
Boundary True False True False,
Boundary True False True False,
Boundary True True True False,

Boundary  False False True True,
Boundary False False True False,
Boundary True False True False,
Boundary True False True False,
Boundary True False True False,
Boundary True True True False]

ninthMap = listArray ((1, 1), (6, 6))
[
Boundary  True True False True,
Boundary True False False True,
Boundary True False True False,
Boundary True False True False,
Boundary True False False False,
Boundary True True False False,

Boundary  False True False True,
Boundary False True False True,
Boundary True False False True,
Boundary True True True False,
Boundary False True False True,
Boundary False True False True,

Boundary  False False False True,
Boundary False False True False,
Boundary False True True False,
Boundary True False False True,
Boundary False True True False,
Boundary False True False True,

Boundary  False True False True,
Boundary True True False True,
Boundary True False False True,
Boundary False True True False,
Boundary True False True True,
Boundary False True False False,

Boundary  False True False True,
Boundary False True False True,
Boundary False True False True,
Boundary True False False True,
Boundary True True False False,
Boundary False True True True,

Boundary  False False True True,
Boundary False True True False,
Boundary False False True True,
Boundary False True True False,
Boundary False False True True,
Boundary True True True False
  ]
