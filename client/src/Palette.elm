module Palette exposing
    ( backgroundColor
    , black
    , debt
    , grayBlue
    , money
    , textWithBorder
    , white
    )

import Element exposing (Attribute, Color, rgb255)
import Element.Font exposing (color, shadow)


grayBlue : Color
grayBlue =
    rgb255 33 28 101


white : Color
white =
    rgb255 255 255 255


black : Color
black =
    rgb255 0 0 0


backgroundColor : Color
backgroundColor =
    rgb255 87 86 195


money : Color
money =
    rgb255 23 124 47


debt : Color
debt =
    rgb255 195 61 50



-- Text


textWithBorder : List (Attribute msg)
textWithBorder =
    [ color white
    , shadow
        { offset = ( 1, 2 )
        , blur = 3
        , color = black
        }
    ]
