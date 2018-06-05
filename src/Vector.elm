module Vector exposing (..)


type alias Vector =
    { x : Float, y : Float }


zero : Vector
zero =
    { x = 0, y = 0 }


scale : Float -> Vector -> Vector
scale factor vec =
    { x = vec.x * factor, y = vec.y * factor }


add : Vector -> Vector -> Vector
add a b =
    { x = a.x + b.x, y = a.y + b.y }


length : Vector -> Float
length vec =
    sqrt (vec.x * vec.x + vec.y * vec.y)
