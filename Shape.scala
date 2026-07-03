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
  case Convex(corners: Vector[Vec[Double]])

  def overlaps(other: Shape): Boolean =
    (this, other) match
      case (a: Rect, b: Rect) =>
        a.toBlRect.hasIntersection(b.toBlRect)
      case (Circle(p1, r1), Circle(p2, r2)) =>
        (p1-p2).abs < (r1+r2)
      case (c1: Convex, c2: Convex) =>
        val axes = c1.axes ++ c2.axes
        !axes.exists(ax =>
          val (p1, p2) = (c1, c2).map(_.project(ax))
          !p1.isOverlapping(p2)
        )
      case (rect: Rect, circle: Circle) =>
        val blr = rect.toBlRect
        val nearest = circle.at.clamp(blr)
        (circle.at-nearest).abs < circle.r
      case (circle: Circle, rect: Rect) =>
        rect.overlaps(circle)
      case (convex: Convex, rect: Rect) =>
        val axes = convex.axes ++ rect.axes
        !axes.exists(ax =>
          val p1 = convex.project(ax)
          val p2 = rect.project(ax)
          !p1.isOverlapping(p2)
        )
      case (rect: Rect, convex: Convex) =>
        convex.overlaps(rect)
      case (convex@Convex(cs1), circle@Circle(p2, r2)) =>
        val axes = convex.axes ++ Vector(cs1.map(_ - p2).minBy(_.abs).normalize)
        !axes.exists(ax =>
          val p1 = convex.project(ax)
          val p2 = circle.project(ax)
          !p1.isOverlapping(p2)
        )
      case (circle: Circle, convex: Convex) =>
        convex.overlaps(circle)

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
      case (c1: Convex, c2: Convex) =>
        val axes = c1.axes ++ c2.axes
        Shape.minTrVecsByAxes(c1, c2, axes)
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
      case (r: Rect, c: Convex) => c.minTrVecs(r).map(_.toTuple.reverse)
      case (c: Convex, r: Rect) =>
        ???
      case (circ: Circle, conv: Convex) => conv.minTrVecs(circ).map(_.toTuple.reverse)
      case (conv: Convex, circ: Circle) => ???
  end minTrVecs

  def intersectsLine(from: Vec[Double], to: Vec[Double]): Option[(near: Vec[Double], far: Vec[Double])] =
    this match
      case r: Rect => r.toBlRect.intersection(from.x, from.y, to.x, to.y)
      case c: Circle => Shape.circleLineIntersection(c, from, to)
      case c: Convex => Shape.lineConvexIntersection(c, from, to)
    
  def contains(pt: Vec[Double]): Boolean =
    this match
      case r: Rect => r.toBlRect.contains(pt)
      case Circle(at, r) =>
        (at - pt).abs <= r
      case c: Convex =>
        !c.axes.exists(ax =>
          val ppt = pt dot ax
          val pconvex = c.project(ax)
          pconvex.min <= ppt && pconvex.max >= ppt
        )

  def relativeTo(pos: Vec[Double]): Shape =
    this match
      case Rect(dx, dy, w, h) =>
        val (x, y) = (dx, dy) + pos
        Rect(x, y, w, h)
      case Circle(at, r) =>
        Circle(at+pos, r)
      case Convex(corners) =>
        Convex(corners.map(_+pos))

  def project(axis: Vec[Double]): (min: Double, max: Double) =
    this match
      case Rect(x, y, w, h) =>
        var min = (x, y) dot axis
        var max = min
        for c <- Vector((x+w, y), (x, y+h), (x+w, y+h)) do
          val p = c dot axis
          if p < min then
            min = p
          else if p > max then
            max = p
        (min, max)
      case Circle(at, r) =>
        val p = at dot axis
        (p-r, p+r)
      case Convex(corners) =>
        var min = axis dot corners.head
        var max = min
        for c <- corners.tail do
          val p = axis dot c
          if p < min then min = p
          else if p > max then max = p
        (min, max)
    

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
      case Convex(corners) =>
        var (minx, miny) = corners.head
        var (maxx, maxy) = (minx, miny)
        for (x, y) <- corners do
          if x < minx then minx = x
          else if x > maxx then maxx = x
          
          if y < miny then miny = y
          else if y > maxy then maxy = y

        ((minx, miny), (maxx, maxy))

  lazy val center: Vec[Double] = this match
    case Rect(x, y, w, h) => (x + w/2, y + h/2)
    case Circle(at, r) => at
    case Convex(corners) => corners.reduce(_+_) / corners.length
  

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
      case Convex(corners) =>
        val screenCorners = corners.map(drawing.screenPosOf)
        drawing.drawLines(screenCorners :+ screenCorners.head)

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
    def axes: Vector[Vec[Double]] =
      Vector((1d, 0d), (0d, 1d))

  extension (convex: Shape.Convex)
    def edges: Iterator[(from: Vec[Double], to: Vec[Double])] =
      val corners = convex.corners :+ convex.corners.head
      for
        pair <- corners.sliding(2)
      yield
        (pair(0), pair(1))

    def axes: Iterator[Vec[Double]] =
      val corners = convex.corners :+ convex.corners.head
      for
        pair <- corners.sliding(2)
        edge = pair(1) - pair(0)
        normal = edge.perp.normalize
      yield
        normal

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

  def lineLineIntersectionTs(from1: Vec[Double], to1: Vec[Double], from2: Vec[Double], to2: Vec[Double]): Option[(t1: Double, t2: Double)] =
    // ft1 = to1 - from1
    // ft2 = to2 - from2
    // when
    //    from1 + ft1 * t1 == from2 + ft2 * t2
    // then the intersection has been found
    // 
    // rearrange to get
    // from1 - from2 == -ft1 * t1 + ft2 * t2
    //
    // => [from1.x - from2.x] == [-ft1.x * t1 + ft2.x * t2]
    //    [from1.y - from2.y]    [-ft1.y * t1 + ft2.y * t2]
    //
    // => [from1.x - from2.x] == [ft1.x, ft2.x] [-t1]
    //    [from1.y - from2.y]    [ft1.y, ft2.y] [ t2]
    //
    // => (1 / (ft1.x * ft2.y - ft2.x * ft1.y)) * [ ft2.y, -ft2.x] [from1.x - from2.x] == [-t1]
    //                                            [-ft1.y,  ft1.x] [from1.y - from2.y]    [ t2]
    //
    // => (1 / (ft1.x * ft2.y - ft2.x * ft1.y)) * [ ft2.y, -ft2.x] [from1.x - from2.x] == [-t1]
    //                                            [-ft1.y,  ft1.x] [from1.y - from2.y]    [ t2]
    //
    // => (1 / (ft1.x * ft2.y - ft2.x * ft1.y)) * [ ft2.y * (from1.x - from2.x) - ft2.x * (from1.y - from2.y)] == [-t1]
    //                                            [-ft1.y * (from1.x - from2.x) + ft1.x * (from1.y - from2.y)] == [ t2]
    //
    // => t2 = (1 / (ft1.x * ft2.y - ft2.x * ft1.y)) * (-ft1.y * (from1.x - from2.x) + ft1.x * (from1.y - from2.y)

    val ft1 = to1 - from1
    val ft2 = to2 - from2
    val inv = (1d / (ft1.x * ft2.y - ft2.x * ft1.y))
    val dx = from1.x - from2.x
    val dy = from1.y - from2.y
    val t1 = inv * (-ft2.y * dx + ft2.x * dy)
    val t2 = inv * (-ft1.y * dx + ft1.x * dy)

    if 0d <= t1 && t1 <= 1d  && 0d <= t2 && t2 <= 1d then
      Some(t1, t2)
    else
      None

  def lineLineIntersection(from1: Vec[Double], to1: Vec[Double], from2: Vec[Double], to2: Vec[Double]): Option[Vec[Double]] =
    lineLineIntersectionTs(from1, to1, from2, to2) match
      case Some((t1, _)) => Some(from1 + (to1-from1)*t1)
      case None => None

  private def lineConvexIntersection(convex: Shape.Convex, from: Vec[Double], to: Vec[Double]): Option[(near: Vec[Double], far: Vec[Double])] =
    var (tMin, tMax) = (Double.PositiveInfinity, Double.NegativeInfinity)
    for
      (eFrom, eTo) <- convex.edges
      (tl, _) <- lineLineIntersectionTs(from, to, eFrom, eTo)
    do
      if tl < tMin then tMin = tl
      if tl > tMax then tMax = tl

    (tMin, tMax) match
      case (Double.PositiveInfinity, Double.NegativeInfinity) => None
      case (tMin, tMax) => Some(from + (to-from)*tMin, from + (to-from)*tMax)

  private def minTrVecsByAxes(s1: Shape, s2: Shape, axes: Iterator[Vec[Double]]): Option[(`this`: Vec[Double], other: Vec[Double])] =
    boundary:
      val sepAx = s1.center - s2.center
      var overlap = Double.PositiveInfinity
      var smallest: Vec[Double] = (0d, 0d)
      for ax <- axes do
        val p1 = s1.project(ax)
        val p2 = s2.project(ax)
        p1.overlap(p2) match
          case Some(o) =>
            if o < overlap then
              overlap = o
              smallest = if (sepAx dot ax) < 0 then -ax else ax
          case None => boundary.break(None)
      val mtv = overlap * smallest
      Some(mtv/2d, -mtv/2d)
      

