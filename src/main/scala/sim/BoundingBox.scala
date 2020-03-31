/********************************************************************
 * Bounding box around population
 *
 * Pepe Gallardo, 2020
 *
 * Partly based on Java Event-Driven Simulator by
 * Robert Sedgewick and Kevin Wayne
 *******************************************************************/

package sim

object BoundingBox {
  val width = 1000.0
  val height = 500.0

  val left = -width/2
  val right = width/2
  val top = -height/2
  val bottom = height/2

  def drawOn(g2D: Graphics2D): Unit = {
    g2D.lineWidth = 3
    g2D.strokeStyle = "#303030"
    g2D.strokeRect(left,top,width,height)
  }
}