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
  end extension

  extension [A: Numeric](a: A)
    inline def *(u: Vec[A]): Vec[A] = u*a

  inline def randomInRect(rect: Rect[Double]): Vec[Double] =
    (
      Random.between(rect.x, rect.xmax),
      Random.between(rect.y, rect.ymax),
    )
    
end Vec
