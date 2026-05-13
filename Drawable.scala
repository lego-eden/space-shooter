import bearlyb.render.Renderer

trait Drawable:
  def draw()(using r: Renderer): Unit
