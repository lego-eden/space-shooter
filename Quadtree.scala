package spacegame

import bearlyb.Rect
import bearlyb.Renderer

enum Quadtree:
  
  def r: Rect[Double]

  case Leaf(r: Rect[Double], nodes: Vector[Quadtree.Node])
  case Branch(r: Rect[Double], nodes: Vector[Quadtree.Node], nw: Quadtree, ne: Quadtree, sw: Quadtree, se: Quadtree)

  def +(collider: Collider[?]): Quadtree =
    this + (collider.absCollider, collider)

  private def +(node: Quadtree.Node): Quadtree =
    val shape = node.shape
    val collider = node.collider
    this match
      case Leaf(r, nodes) if nodes.length < Quadtree.N =>
        Leaf(r, nodes :+ node)
      case b@Branch(r, nodes, nw, ne, sw, se) =>
        if shape.containedWithin(nw.r) then b.copy(nw = nw + node)
        else if shape.containedWithin(ne.r) then b.copy(ne = ne + node)
        else if shape.containedWithin(sw.r) then b.copy(sw = sw + node)
        else if shape.containedWithin(se.r) then b.copy(se = se + node)
        else b.copy(nodes = nodes :+ node)
      case Leaf(Rect(x, y, w, h), nodes) =>
        val dim = (w/2, h/2)
        val nw = Rect((x, y), dim)
        val ne = Rect((x+w/2, y), dim)
        val sw = Rect((x, y+h/2), dim)
        val se = Rect((x+w/2, y+h/2), dim)
        val vs = Vector.empty
        Branch(
          r,
          vs,
          Leaf(nw, vs),
          Leaf(ne, vs),
          Leaf(sw, vs),
          Leaf(se, vs),
        ) ++ nodes + node

  private def ++(nodes: IterableOnce[Quadtree.Node]): Quadtree =
    nodes.iterator.foldLeft(this)(_ + _)

  lazy val size: Int =
    this match
      case Leaf(r, nodes) => nodes.size
      case Branch(r, nodes, nw, ne, sw, se) =>
        nodes.size + nw.size + ne.size + sw.size + se.size
    
  def draw()(using r: Camera.Drawing): Unit =
    this match
      case Leaf(boundary, nodes) =>
        r.drawRect(boundary)
        nodes.foreach(node => node.shape.draw())
      case Branch(boundary, nodes, nw, ne, sw, se) =>
        nodes.foreach(node => node.shape.draw())
        Vector(nw, ne, sw, se).foreach(_.draw())

  private def shapesIn(bounds: Shape): Vector[Quadtree.Node] =
    this match
      case Leaf(r, nodes) =>
        nodes
      case Branch(r, nodes, nw, ne, sw, se) =>
        nodes :++ Vector(nw, ne, sw, se)
          .filter(region => Shape.Rect(region.r.x, region.r.y, region.r.w, region.r.h).overlaps(bounds))
          .flatMap(_.shapesIn(bounds))

  // def collidersOf(collider: Collider[?]): Vector[Collider[?]] =
  //   shapesIn(collider.absCollider).map(_.collider)

  def foreachCollision(collider: Collider[?])(body: (Collider[?], Vec[Double], Vec[Double]) => Unit): Unit =
    val absCollider = collider.absCollider
    shapesIn(absCollider).iterator
      .filter(!_.collider.eq(collider))
      .map((shape, other) => (other, absCollider.minTrVecs(shape)))
      .collect:
        case (other, Some(colliderTrVec, otherTrVec)) =>
          (other, colliderTrVec, otherTrVec)
      .foreach(body.tupled)

object Quadtree:
  type Node = (shape: Shape, collider: Collider[?])

  val N = 1

  def apply(boundary: Rect[Double]): Quadtree =
    Quadtree.Leaf(boundary, Vector.empty)
