package spacegame

import bearlyb.Rect as BlRect
import bearlyb.render.{VertexBuffer, Vertex}
import Vec.*
import scala.util.Using
import scala.util.boundary
import util.clamp

enum Shape:
  case Rect(x: Double, y: Double, w: Double, h: Double)
  case Circle(at: Vec[Double], r: Double)

  def overlaps(other: Shape): Boolean =
    (this, other) match
      case (a: Rect, b: Rect) =>
        a.toBlRect.hasIntersection(b.toBlRect)
      case (Circle(p1, r1), Circle(p2, r2)) =>
        (p1-p2).abs < (r1+r2)
      case (rect: Rect, circle: Circle) =>
        val blr = rect.toBlRect
        val nearest = circle.at.clamp(blr)
        (circle.at-nearest).abs < circle.r
      case (circle: Circle, rect: Rect) =>
        rect.overlaps(circle)

  def minTrVecs(other: Shape): Option[(`this`: Vec[Double], other: Vec[Double])] =
    (this, other) match
      case (a: Rect, b: Rect) =>
        val bla = a.toBlRect
        val blb = b.toBlRect
        val overlap = bla.intersection(blb)
        if overlap.isEmpty then
          None
        else
          if overlap.w < overlap.h then
            if a.x < b.x then
              Some((-overlap.w/2, 0.0), (overlap.w/2, 0.0))
            else
              Some((overlap.w/2, 0.0), (-overlap.w/2, 0.0))
          else
            if a.y < b.y then
              Some((0.0, -overlap.h/2), (0.0, overlap.h/2))
            else
              Some((0.0, overlap.h/2), (0.0, -overlap.h/2))
      case (Circle(p1, r1), Circle(p2, r2)) =>
        val diff = p2 - p1
        val dist = diff.abs
        lazy val normal = diff / dist
        val depth = (r1 + r2) - dist
        lazy val mtv = depth*normal
        if depth < 0 then
          None
        else
          Some(-mtv/2, mtv/2)
      case (c: Circle, r: Rect) =>
        r.minTrVecs(c).map(_.toTuple.reverse)
      case (r: Rect, c: Circle) =>
        val blr = r.toBlRect
        lazy val nearest = c.at.clamp(blr)
        lazy val dist = (c.at - nearest).abs
        if !blr.contains(c.at) then
          if dist >= c.r then None
          else
            val depth = c.r - dist
            val mtv = ((nearest - c.at)/dist) * depth
            Some(mtv/2d, -mtv/2d)
        else
          val dists = Seq(
            ((-1d, 0d), c.at.x - blr.x), 
            ((1d, 0d),  blr.xmax - c.at.x), 
            ((0d, -1d), c.at.y - blr.y), 
            ((0d, 1d),  blr.ymax - c.at.y), 
          )
          val (n, dist) = dists.minBy((_, d) => d)
          val mtv = n * (dist + c.r)
          Some(-mtv/2d, mtv/2d)
        end if

  end minTrVecs

  def intersectsLine(from: Vec[Double], to: Vec[Double]): Option[(near: Vec[Double], far: Vec[Double])] =
    this match
      case r: Rect => r.toBlRect.intersection(from.x, from.y, to.x, to.y)
      case c: Circle => Shape.circleLineIntersection(c, from, to)
    
  def contains(pt: Vec[Double]): Boolean =
    this match
      case r: Rect => r.toBlRect.contains(pt)
      case Circle(at, r) =>
        (at - pt).abs <= r

  def relativeTo(pos: Vec[Double]): Shape =
    this match
      case Rect(dx, dy, w, h) =>
        val (x, y) = (dx, dy) + pos
        Rect(x, y, w, h)
      case Circle(at, r) =>
        Circle(at+pos, r)

  def containedWithin(region: BlRect[Double]): Boolean =
    val (min, max) = extents
    region.x <= min.x
    && region.y <= min.y
    && region.xmax >= max.x
    && region.ymax >= max.y

  lazy val extents: (min: Vec[Double], max: Vec[Double]) =
    this match
      case Circle(at, radius) =>
        val topLeft = at - (radius, radius)
        val bottomRight = at + (radius, radius)
        (topLeft, bottomRight)
      case rect: Rect =>
        val blr = rect.toBlRect
        (blr.pos, blr.pos + blr.dim)

  def draw()(using drawing: Camera.Drawing): Unit =
    this match
      case r: Rect =>
        val blr = r.toBlRect
        val (x, y) = drawing.screenPosOf(blr.pos)
        drawing.drawRect(blr.copy(x,y))
      case Circle(at, r) =>
        val pos = drawing.screenPosOf(at)
        val corners = Shape.tabulateCircle(r).map(_ + pos)
        drawing.drawLines(corners :+ corners.head)

end Shape

object Shape:
  private val numCorners = 35 
  def tabulateCircle(radius: Double): Seq[Vec[Double]] =
    val angle = math.Pi*2 / numCorners
    Seq.tabulate(numCorners)(i => (radius, 0.0).rotate(angle*i))

  extension (r: Shape.Rect)
    def toBlRect: BlRect[Double] =
      val Shape.Rect(x,y,w,h) = r
      BlRect(x,y,w,h)

  private def circleLineIntersection(circle: Shape.Circle, from: Vec[Double], to: Vec[Double]): Option[(near: Vec[Double], far: Vec[Double])] =
    boundary:
      // check if entire line segment is inside the circle
      val fromInside = (from - circle.at).absSqrd <= circle.r*circle.r
      val toInside = (to - circle.at).absSqrd <= circle.r*circle.r
      if fromInside && toInside then boundary.break(Some(from, to))

      // for a point on the line |P - c.at|² = c.r²
      // introduce
      //    V = to - from
      //    L = from - c.at
      //
      // putting these into the original equation gives:
      //    |(from + t*V) - c.at|² = c.r²
      // => |L + t*V|² = c.r²
      //
      // expanding with the dot product gives:
      //    L² + 2*V*L*t + V²*t² = r²
      // => V²*t² + 2*V*L*t + (L²-r²) = 0
      // => a*t² + b*t + c = 0
      // where
      //    a = V dot V
      //    b = 2*(V dot L)
      //    c = L dot L - r*r
      //
      // => t = (-b ± √discr) / (2*a)
      val V = to - from
      val L = from - circle.at
      val a = V dot V
      val b = (V dot L)*2
      val c = (L dot L) - (circle.r*circle.r)
      val discr = b*b - 4*a*c
      lazy val t1 = (-b + math.sqrt(discr)) / (2*a)
      lazy val t2 = (-b - math.sqrt(discr)) / (2*a)

      if discr < 0.0 then boundary.break(None)
      
      val (tNear, tFar) = if t1 <= t2 then (t1, t2) else (t2, t1)
      val tNearOk = 0.0 <= tNear && tNear <= 1.0
      val tFarOk = 0.0 <= tFar && tFar <= 1.0
      lazy val near = from + V*tNear
      lazy val far = from + V*tFar
      
      (tNearOk, tFarOk) match
        case (true, true) => Some(near, far)
        case (true, false) => Some(near, to)
        case (false, true) => Some(from, far)
        case (false, false) => None
      
  end circleLineIntersection
