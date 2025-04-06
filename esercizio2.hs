let ARENA_W = 480
let ARENA_H = 360
let BALL_W = 20
let BALL_H = 20

type posX = Int
type posY = Int
type posDx = Int
type posDy = Int
type Ball = (posX, posY, posDx, posDy)

move :: Ball -> Ball
move = \(x, y, dx, dy) -> 
    let newDx = if x + dx < 0 || x + dx + BALL_W > ARENA_W then -dx else dx
        newDy = if y + dy < 0 || y + dy + BALL_H > ARENA_H then -dy else dy
    in (x + newDx, y + newDy, newDx, newDy)