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

  private val scale = 0.7

  private val width = dom.window.innerWidth
  private val height = dom.window.innerHeight

  canvas.width = (BoundingBox.width*scale*1.05).toInt
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

object Identifier {
  private var id = 0
  def unique(prefix: String): String = {
    id += 1
    s"$prefix$id"
  }
}

object Layout {
  def horizontal(minWidth: Int = 0, padding: Int = 1)(elements: Element*): Element = {
    val div = dom.document.createElement("div")
    div.setAttribute("style", "display:table;")
    for(element <- elements){
      val innerDiv = dom.document.createElement("div")
      innerDiv.appendChild(element)
      innerDiv.setAttribute("style", s"display:table-cell; vertical-align:middle; padding-left: ${padding}px; min-width: ${minWidth}px; text-align:right")
      div.appendChild(innerDiv)
    }
    div
  }

  def row(elements: Element*): Element = {
    val row = dom.document.createElement("tr")
    for(element <- elements) {
      val cell = dom.document.createElement("td")
      cell.appendChild(element)
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
  output.setAttribute("class", "Label")

  def tooltip: String = output.title
  def tooltip_=(s: String): Unit =
    output.title = s

  output.innerText = s
  val layout = output
}

class IntField() extends CanDisable {
  protected val input = dom.document.createElement("input").asInstanceOf[HTMLInputElement]

  input.setAttribute("type", "number")
  input.setAttribute("class", "IntField")
  val layout = input

  def columns: Int = input.getAttribute("size").toInt
  def columns_=(v: Int): Unit = {
    input.setAttribute("size", v.toString)
    input.setAttribute("min", "0")
    input.setAttribute("max", math.pow(10,v).toInt.toString)
  }

  def value: Int = input.value.toInt
  def value_=(v: Int): Unit =
    input.value = v.toString

  def tooltip: String = input.title
  def tooltip_=(s: String): Unit =
    input.title = s

  def disabled: Boolean = input.disabled
  def disabled_=(d: Boolean): Unit =
    input.disabled = d
}

class Button(s: String) extends CanDisable {
  protected val button = dom.document.createElement("button").asInstanceOf[html.Button]
  button.setAttribute("class", "Button")

  def title: String = button.title
  def title_=(s: String): Unit =
    button.title = s

  def text: String = button.innerText
  def text_=(s: String): Unit =
    button.innerText = s

  def disabled: Boolean = button.disabled
  def disabled_=(d: Boolean): Unit =
    button.disabled = d

  text_=(s)
  val layout = button

  def onClick(ev: MouseEvent): Unit = {}
  button.onclick = onClick(_)
}

abstract class NumericSlider[A] extends CanDisable {
  protected def formatText(n: A): String
  protected def fromInt(n: Int): A
  protected def toInt(v: A): Int

  protected val display = dom.document.createElement("div").asInstanceOf[html.Div]
  protected val slider = dom.document.createElement("input").asInstanceOf[HTMLInputElement]

  def value: A = fromInt(slider.value.toInt)
  def value_=(v: A): Unit = {
    display.innerText = formatText(v)
    slider.value = toInt(v).toString
  }

  slider.setAttribute("type", "range")
  slider.setAttribute("step", "1")
  slider.setAttribute("class", "Slider")
  slider.addEventListener("input", (_: Any) => display.innerText = formatText(value))

  display.setAttribute("class", "Slider_Display")

  val layout = Layout.horizontal(minWidth = 40)(display, slider)

  def tooltip: String = slider.title
  def tooltip_=(s: String): Unit = {
    display.title = s
    slider.title = s
  }

  def disabled: Boolean = slider.disabled
  def disabled_=(d: Boolean): Unit =
    slider.disabled = d

  protected def setRange(min: Int, max: Int, ticks: Int = 20):Unit = {
    slider.setAttribute("min", min.toString)
    slider.setAttribute("max", max.toString)
    val majorTickSpacing = (max - min + 1) / ticks

    val labels = List.range(min, max+1, majorTickSpacing)
    val datalist = dom.document.createElement("datalist")
    val id = Identifier.unique("datalist")
    datalist.setAttribute("id", id)
    for(l <- labels) {
      val option = dom.document.createElement("option")
      option.setAttribute("value", l.toString)
      datalist.appendChild(option)
    }
    dom.document.getElementById("controls").appendChild(datalist)
    slider.setAttribute("list", id)
  }
}

class DoubleSlider(v: Double, from: Int, to: Int) extends NumericSlider[Double] {
  def this(from: Int, to: Int) {
    this(0, from, to)
  }
  def formatText(v: Double): String = s"$v"
  def fromInt(n: Int): Double = n
  def toInt(v: Double): Int = v.toInt

  setRange(from, to)
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

  setRange(from, to)
  value = v
}

