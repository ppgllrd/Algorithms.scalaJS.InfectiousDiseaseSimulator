/********************************************************************
 * Main application
 *
 * Pepe Gallardo, 2020
 *
 * Partly based on Java Event-Driven Simulator by
 * Robert Sedgewick and Kevin Wayne
 *******************************************************************/

package sim

import org.scalajs.dom
import org.scalajs.dom.raw.MouseEvent
import org.scalajs.dom.{document, html}

object Main  {
  def main(args: Array[String]): Unit = {
  }

  private val seedTooltip = "Seed for this random simulation"
  private val seedLabel = new Label("Seed:") {
    tooltip = seedTooltip
  }
  private val seedIntField = new IntField() {
    columns = 12
    tooltip = seedTooltip
  }

  private val randomButton = new Button("Random") {
    title = "Generate a random seed"
    override def onClick(ev: MouseEvent): Unit = {
      seedIntField.value = Random.uniform(Int.MaxValue)
    }
  }

  private val populationSzTooltip = "Number of individuals in population"
  private val populationSzLabel = new Label("Population size:") {
    tooltip = populationSzTooltip
  }
  private val populationSzSlider = new IntSlider(0, 1500) {
    tooltip = populationSzTooltip
  }

  private val velocityTooltip = "Velocity of individuals in normal distributed with μ=0 and this value as σ"
  private val velocityLabel = new Label("Velocity variance:") {
    tooltip = velocityTooltip
  }
  private val velocitySlider = new IntSlider(0, 100) {
    tooltip = velocityTooltip
  }

  private val probInfectionTooltip = "Probability of getting infected after contacting another infected individual"
  private val probInfectionLabel = new Label("Infection rate:") {
    tooltip = probInfectionTooltip
  }
  private val probInfectionSlider = new ProbSlider() {
    tooltip = probInfectionTooltip
  }

  private val probDyingTooltip = "Probability of dying after getting infected"
  private val probDyingLabel = new Label("Death rate:") {
    tooltip = probDyingTooltip
  }
  private val probDyingSlider = new ProbSlider() {
    tooltip = probDyingTooltip
  }

  private val timeInfectiousTooltip = "Time an individual remains infectious to others is normal distributed around this value(μ) with σ=1"
  private val timeInfectiousLabel = new Label("Time infectious:") {
    tooltip = timeInfectiousTooltip
  }
  private val timeInfectiousSlider = new IntSlider(0, 100){
    tooltip = timeInfectiousTooltip
  }

  private val HzTootlTip = "Number of redraw events per clock tick"
  private val HzLabel = new Label("Redraw frequency:") {
    tooltip = HzTootlTip
  }
  private val HzSlider = new IntSlider(0, 60) {
    tooltip = HzTootlTip
  }

  def configuration: Configuration =
    Configuration(
      seed = seedIntField.value
      , Hz = HzSlider.value
      , populationSz = populationSzSlider.value
      , velocitySigma = velocitySlider.value
      , timeLimit = DefaultConfiguration.timeLimit
      , probInfection= probInfectionSlider.value
      , probDying = probDyingSlider.value
      , timeInfectious = timeInfectiousSlider.value
    )

  def configuration_=(conf: Configuration): Unit = {
    seedIntField.value = conf.seed
    HzSlider.value = conf.Hz
    populationSzSlider.value = conf.populationSz
    velocitySlider.value = conf.velocitySigma.toInt
    probInfectionSlider.value = conf.probInfection
    probDyingSlider.value = conf.probDying
    timeInfectiousSlider.value = conf.timeInfectious.toInt
  }

  private val canvas = document.getElementById("canvas").asInstanceOf[html.Canvas]
  private val window = Window(canvas)

  private val startButton = new Button("Start") {
    private val start = "Start"
    private var handleOpt: Option[Int] = None

    private def setText(s: String): Unit = {
      text = s
      title = s"$s simulation"
    }

    def init(): Unit = {
      setText(start)
      handleOpt = None
    }

    private val toDisable = Array(seedIntField, randomButton, populationSzSlider, velocitySlider
      , probInfectionSlider, probDyingSlider, timeInfectiousSlider, HzSlider)

    override def onClick (ev: MouseEvent): Unit = {
      handleOpt match {
        case None =>
          val conf = configuration
          val simulator = Simulator(window, conf)
          simulator.initialize()
          val handle = dom.window.setInterval(() => simulator.step(), 1000.0/conf.Hz)

          toDisable.foreach(_.disabled = true)
          setText("Stop")
          handleOpt = Some(handle)

        case Some(handle) =>
          dom.window.clearInterval(handle)

          toDisable.foreach(_.disabled = false)
          setText(start)
          handleOpt = None
      }
    }
  }

  private val table = dom.document.createElement("table")
  table.appendChild(Layout.row(seedLabel.layout, Layout.horizontal()(seedIntField.layout, randomButton.layout)))
  table.appendChild(Layout.row(populationSzLabel.layout, populationSzSlider.layout))
  table.appendChild(Layout.row(velocityLabel.layout, velocitySlider.layout))
  table.appendChild(Layout.row(probInfectionLabel.layout, probInfectionSlider.layout))
  table.appendChild(Layout.row(probDyingLabel.layout, probDyingSlider.layout))
  table.appendChild(Layout.row(timeInfectiousLabel.layout, timeInfectiousSlider.layout))
  table.appendChild(Layout.row(HzLabel.layout, HzSlider.layout))
  table.appendChild(Layout.row(startButton.layout, new Label("").layout))

  private val controls = dom.document.getElementById("controls")
  controls.appendChild(table)

  configuration_=(DefaultConfiguration)
  startButton.init()
  window.drawWith(BoundingBox.drawOn)
}
