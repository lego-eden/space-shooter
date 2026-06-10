package spacegame

opaque type Color <: (Float, Float, Float, Float) = (Float, Float, Float, Float)

object Color:
  extension (c: Color)
    inline def r = c(0)
    inline def g = c(1)
    inline def b = c(2)
    inline def a = c(3)

    inline def copy(r: Float = c.r, g: Float = c.g, b: Float = c.b, a: Float = c.a): Color =
      (r,g,b,a)

    inline def withAlpha(a: Float): Color = copy(a=a)

  end extension

  val white: Color = (1f,1f,1f,1f)
  val red: Color = (1f,0f,0f,1f)
  val green: Color = (0f,1f,0f,1f)
  val blue: Color = (0f,0f,1f,1f)
  val black: Color = (0f,0f,0f,1f)
  val yellow: Color = (1f,1f,0f,1f)
