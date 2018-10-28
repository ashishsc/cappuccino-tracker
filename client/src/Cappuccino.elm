module Cappuccino exposing (capSvg)

import Svg exposing (..)
import Svg.Attributes as SAttrs exposing (..)


capSvg : String -> Svg msg
capSvg lidColor =
    let
        color1 =
            "603D13"

        color3 =
            "A56905"

        fillColor color =
            "fill:#" ++ color ++ ";"
    in
    svg [ x "0px", y "0px", viewBox "0 0 297.157 297.157", SAttrs.style "enable-background:new 0 0 297.157 297.157;" ]
        [ g []
            [ Svg.path
                [ SAttrs.style (fillColor color1)
                , d "M42.309,141.157c4.176,21.306,10.16,37.929,16.037,54.237c9.015,25.015,17.563,48.734,18.646,85.763   h102.941c1.083-37.028,9.631-60.748,18.646-85.763c5.878-16.309,11.862-32.932,16.038-54.237H42.30z"
                ]
                []
            , Svg.path
                [ SAttrs.style (fillColor lidColor)
                , d "M220.103,86.157H36.823c0.272,14.67,1.293,27.505,2.844,39h177.593   C218.81,113.661,219.831,100.826,220.103,86.157z"
                ]
                []
            , Svg.path
                [ SAttrs.style (fillColor color3)
                , d "M178.277,40.636c-26.604,0-39.796-6.566-49.917-18.873c-10.122,12.307-23.313,18.873-49.917,18.873   c-19.149,0-38.901,10.952-41.538,29.521h182.909C217.178,51.588,197.425,40.636,178.277,40.636z"
                ]
                []
            , Svg.path
                [ d "M276.547,132.976c-0.099-4.347-3.65-7.819-7.998-7.819h-35.192c1.76-13.705,2.763-37.504,2.763-50.752   c0-13.248-5.797-25.452-15.904-34.661c-10.692-9.742-25.586-15.107-41.938-15.107c-28.104,0-35.009-7.879-43.04-20.854   c-1.528-2.471-4.184-3.809-6.892-3.782c-2.697-0.018-5.34,1.32-6.863,3.782c-8.031,12.975-14.935,20.854-43.039,20.854   c-16.352,0-31.246,5.365-41.939,15.107C26.397,48.952,20.6,61.585,20.6,74.405c0,0,0.166,7.029,0.17,7.113   c0.44,57.982,12.156,90.533,22.524,119.301c9.16,25.418,17.813,49.427,17.813,88.338c0,4.418,3.582,8,8,8h118.711   c4.418,0,8-3.582,8-8c0-17.316,1.716-31.677,4.426-44.528c32.09-4.289,55.422-24.803,67.601-59.536   C277.02,158.931,276.57,134.024,276.547,132.976z M78.443,40.636c26.604,0,39.795-6.566,49.917-18.873   c10.121,12.307,23.313,18.873,49.917,18.873c19.148,0,38.901,10.952,41.537,29.521H36.905   C39.542,51.588,59.295,40.636,78.443,40.636z M36.823,86.157h183.28c-0.272,14.67-1.293,27.505-2.844,39H39.667   C38.116,113.661,37.095,100.826,36.823,86.157z M198.579,195.394c-9.015,25.015-17.563,48.734-18.646,85.763H76.992   c-1.083-37.028-9.631-60.748-18.646-85.763c-5.877-16.309-11.861-32.932-16.037-54.237h172.308   C210.441,162.462,204.457,179.085,198.579,195.394z M252.613,180.178c-9.34,26.339-25.478,42.247-48.037,47.425   c2.727-9.218,5.859-17.916,9.056-26.784c6.146-17.054,12.766-35.443,17.231-59.662h29.432   C259.735,149.957,257.992,165.009,252.613,180.178z" ]
                []
            ]
        ]
