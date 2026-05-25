package util

import bearlyb.Rect

extension (rect: Rect[Double])
  def expand(margin: Double): Rect[Double] =
    val Rect(x, y, w, h) = rect
    Rect(x-margin, y-margin, w+margin*2, h+margin*2)
