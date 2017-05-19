case class Pos(x: Int, y: Int) {
  /** The position obtained by changing the `x` coordinate by `d` */
  def dx(d: Int) = copy(x = x + d)

  /** The position obtained by changing the `y` coordinate by `d` */
  def dy(d: Int) = copy(y = y + d)
}

case class Block(b1: Pos, b2: Pos)



val a = Pos(1, 2)

val b = Pos(1, 2)

a == b

a.x == b.x

