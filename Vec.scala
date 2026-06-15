package spacegame

import bearlyb.rect.Rect
import scala.util.Random

type Vec[A] = (A, A)
object Vec:
  extension [A](u: Vec[A])
    inline def x = u(0)
    inline def y = u(1)
    inline def map[B](f: A => B) = (f(u.x), f(u.y))
    inline def zip[B](v: Vec[B]): Vec[(A, B)] = ((u.x, v.x), (u.y, v.y))

  extension [A: Numeric as num](u: Vec[A])
    def +(v: Vec[A]) = u.zip(v).map(num.plus)
    def -(v: Vec[A]) = u.zip(v).map(num.minus)
    def *(a: A): Vec[A] = u.map(num.times(_, a))
    def unary_- = u.map(num.negate)

    /** [ cos(angle) -sin(angle) ] => [ cos(angle)*u.x - sin(angle)*u.y ]
      * [ sin(angle)  cos(angle) ]    [ sin(angle)*u.x + cos(angle)*u.y ]
      */
    def rotate(angle: Double): Vec[Double] =
      val c = math.cos(angle)
      val s = math.sin(angle)
      val (xDouble, yDouble) = u.map(num.toDouble)
      (
        c*xDouble - s*yDouble,
        s*xDouble + c*yDouble
      )

    inline def sum = num.plus(u.x, u.y)

    def abs: Double =
      math.sqrt(u.toDoubleVec.map(n => n*n).sum)

    def absSqrd: Double =
      u.toDoubleVec.map(n => n*n).sum

    inline def normalize: Vec[Double] =
      u.toDoubleVec / u.abs

    infix def dot(v: Vec[A]): A =
      u.zip(v).map(num.times).sum

    inline def toIntVec: Vec[Int] = u.map(num.toInt)
    inline def toDoubleVec: Vec[Double] = u.map(num.toDouble)

  extension [A: ([t] =>> Fractional[t] | Integral[t]) as num](inline u: Vec[A])
    inline def /(inline a: A): Vec[A] =
      inline num match
        case frac: Fractional[A] => u.map(frac.div(_, a))
        case inte: Integral[A] => u.map(inte.quot(_, a))

  extension [A: Fractional](from: Vec[A])
    def lerp(to: Vec[A], t: A): Vec[A] =
      from + (to - from) * t

  extension [A: Fractional as frac](a: Vec[A])
    def project(on: Vec[A]): Vec[A] =
      import scala.math.Fractional.Implicits.infixFractionalOps
      (a.dot(on)/on.dot(on))*on

    def reflect(n: Vec[A]): Vec[A] =
      a - a.project(n)*frac.fromInt(2)

  extension (u: Vec[Double])
    def floor: Vec[Double] = u.map(_.floor)
    def dir: Double =
      math.atan2(u.y, u.x)

  extension [A: Numeric](a: A)
    inline def *(u: Vec[A]): Vec[A] = u*a

  def randomInRect(rect: Rect[Double]): Vec[Double] =
    (
      Random.between(rect.x, rect.xmax),
      Random.between(rect.y, rect.ymax),
    )

  def randomInRect(rect: Rect[Double], notInRect: Rect[Double]): Vec[Double] =
    // split rect into four rects surrounding the notInRect rect
    // |----|--------------
    // |    |             |
    // |    |             |
    // |    |--------------
    // |    |     |       |
    // |    |     |       |
    // -----------|       |
    // |          |       |
    // |          |       |
    // -----------|-------|
    lazy val Rect(rx, ry, _, _) = rect
    lazy val Rect(nx, ny, _, _) = notInRect
    lazy val topLeft = Rect(rx, ry, nx-rx, notInRect.ymax-ry)
    lazy val bottomLeft = Rect(rx, notInRect.ymax, notInRect.xmax-rx, rect.ymax-notInRect.ymax)
    lazy val bottomRight = Rect(notInRect.xmax, ny, rect.xmax-notInRect.xmax, rect.ymax-ny)
    lazy val topRight = Rect(nx, ry, rect.xmax-nx, ny-ry)
    val chosenRect = Random.between(0, 4) match
      case 0 => topLeft
      case 1 => bottomLeft
      case 2 => bottomRight
      case 3 => topRight
    randomInRect(chosenRect)
end Vec
