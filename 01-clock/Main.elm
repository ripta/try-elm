import Color
import Graphics.Collage exposing (collage, filled, ngon, outlined, segment, solid, traced)
import Time exposing (every, inSeconds, second)

main =
  every second
  |> Signal.map drawClock

drawClock time =
  collage 450 450 [
      clockShape |> filled Color.lightGrey
    , clockShape |> outlined (solid Color.grey)
    , drawHourHand time
    , drawMinuteHand time
    , drawSecondHand time
  ]

clockShape = ngon 12 110

drawHand color length tick =
  let
    secs  = inSeconds tick
    angle = degrees (90 - 6 * secs)
  in
    fromPolar (length, angle)
    |> segment (0, 0)
    |> traced (solid color)

drawHourHand tick   = drawHand Color.orange   100 tick
drawMinuteHand tick = drawHand Color.charcoal 100 (tick / 60)
drawSecondHand tick = drawHand Color.charcoal  60 (tick / 720)
