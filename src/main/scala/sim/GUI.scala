/********************************************************************
 * Graphical User Interface
 *
 * Pepe Gallardo, 2020
 *
 * Partly based on Java Event-Driven Simulator by
 * Robert Sedgewick and Kevin Wayne
 *******************************************************************/

package sim

import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLInputElement, MouseEvent}
import org.scalajs.dom.{Element, html}


trait Drawable {
  def drawWith(procedure : Graphics2D => Unit): Unit
}

object Window {
  def apply(canvas: html.Canvas): Window =
    new Window(canvas)
}

class Window(canvas: html.Canvas) extends Drawable {
  private val g2D = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  private val scale = 0.75

  private val width = dom.window.innerWidth
  private val height = dom.window.innerHeight

  canvas.width = (BoundingBox.width*scale*1.1).toInt
  canvas.height = (BoundingBox.height*scale*1.4).toInt

  private val xOffset = canvas.width / 2.0
  private val yOffset = canvas.height / 2.0

  def drawWith(procedure : Graphics2D => Unit): Unit = {
    g2D.fillStyle = "white"
    g2D.setTransform(1, 0, 0, 1, 0, 0)
    g2D.fillRect(0, 0, width, height)
    g2D.setTransform(scale, 0, 0, scale, xOffset, yOffset)
    procedure(g2D)
  }
}

object Colors {
  val nonInfected = "#0000EE"
  val infected = "red"
  val recovered = "#00BB00"
  val dead = "#0A0A0A"
  val black = "#000000"
}

object Fonts {
  val mono = "14px Mono"
}

object Layout {
  def horizontal(left: Element, right: Element): Element = {
    val div = dom.document.createElement("div")
    div.setAttribute("style", "display:table;")
    val outputDiv = dom.document.createElement("div")
    outputDiv.appendChild(left)
    outputDiv.setAttribute("style", "display:table-cell; vertical-align:middle; width: 50px; text-align:right")
    val sliderDiv = dom.document.createElement("div")
    sliderDiv.appendChild(right)
    sliderDiv.setAttribute("style", "display:table-cell; vertical-align:middle; padding-left: 5px")
    div.appendChild(outputDiv)
    div.appendChild(sliderDiv)
    div
  }

  def row(xs: Element*): Element = {
    val row = dom.document.createElement("tr")
    for(x <- xs) {
      val cell = dom.document.createElement("td")
      cell.appendChild(x)
      row.appendChild(cell)
    }
    row
  }
}

trait CanDisable {
  def disabled: Boolean
  def disabled_=(d: Boolean): Unit
}

class Label(s: String) {
  protected val output = dom.document.createElement("output").asInstanceOf[HTMLInputElement]

  def tooltip: String = output.title
  def tooltip_=(s: String): Unit = {
    output.title = s
  }

  output.innerText = s
  val layout = output
}

class IntField() extends CanDisable {
  protected val input = dom.document.createElement("input").asInstanceOf[HTMLInputElement]
  protected val peers = Array(input)

  input.setAttribute("type", "number")
  input.setAttribute("size", "12")
  input.setAttribute("style", "text-align:right; font-size: 9pt")
  val layout = input

  def value: Int = input.value.toInt
  def value_=(v: Int): Unit = {
    input.value = v.toString
  }

  def tooltip: String = input.title
  def tooltip_=(s: String): Unit = {
    input.title = s
  }

  def disabled: Boolean = input.disabled
  def disabled_=(d: Boolean): Unit = {
    input.disabled = d
  }
}

class Button(s: String) extends CanDisable {
  protected val button = dom.document.createElement("button").asInstanceOf[html.Button]

  def title: String = button.title
  def title_=(s: String): Unit = {
    button.title = s
  }

  def text: String = button.innerText
  def text_=(s: String): Unit = {
    button.innerText = s
  }

  def disabled: Boolean = button.disabled
  def disabled_=(d: Boolean): Unit = {
    button.disabled = d
  }

  text_=(s)
  val layout = button

  def onClick(ev: MouseEvent): Unit = {}
  button.onclick = onClick(_)
}


abstract class NumericSlider[A] extends CanDisable {
  protected def formatText(n: A): String

  protected def fromInt(n: Int): A

  protected def toInt(v: A): Int

  protected val output = dom.document.createElement("output").asInstanceOf[HTMLInputElement]
  protected val slider = dom.document.createElement("input").asInstanceOf[HTMLInputElement]

  def value: A = fromInt(slider.value.toInt)
  def value_=(v: A): Unit = {
    output.value = formatText(v)
    slider.value = toInt(v).toString
  }

  slider.setAttribute("type", "range")
  slider.setAttribute("step", "1")
  slider.addEventListener("input", (_: Any) => output.value = formatText(value))

  output.setAttribute("style", "font-size: 9pt")

  val layout = Layout.horizontal(output, slider)

  def tooltip: String = slider.title
  def tooltip_=(s: String): Unit = {
    output.title = s
    slider.title = s
  }

  def disabled: Boolean = slider.disabled
  def disabled_=(d: Boolean): Unit = {
    output.disabled = d
    slider.disabled = d
  }
}

class DoubleSlider(v: Double, from: Int, to: Int) extends NumericSlider[Double] {
  def this(from: Int, to: Int) {
    this(0, from, to)
  }
  def formatText(v: Double): String = s"$v"
  def fromInt(n: Int): Double = n
  def toInt(v: Double): Int = v.toInt

  slider.setAttribute("min", from.toString)
  slider.setAttribute("max", to.toString)

  value = v
}

class ProbSlider(v: Double = 0) extends DoubleSlider(v, 0, 100) {
  override def formatText(v: Double): String = f"$v%.2f"
  override def fromInt(n: Int): Double = n/100.0
  override def toInt(v: Double): Int = (v*100).toInt
}

class IntSlider(v: Int, from: Int, to: Int) extends NumericSlider[Int] {
  def this(from: Int, to: Int) {
    this(0, from, to)
  }
  def formatText(v: Int): String = s"$v"
  def fromInt(n: Int): Int = n
  def toInt(v: Int): Int = v

  slider.setAttribute("min", from.toString)
  slider.setAttribute("max", to.toString)

  value = v
}

