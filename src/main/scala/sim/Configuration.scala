/********************************************************************
 * Configuration and main program
 *
 * Pepe Gallardo, 2020
 *
 * Partly based on Java Event-Driven Simulator by
 * Robert Sedgewick and Kevin Wayne
 *******************************************************************/

package sim

case class Configuration(seed: Int
                         , Hz: Int // Hz is redraw frequency: number of redraw events per clock tick
                         , populationSz: Int
                         , velocitySigma: Double
                         , timeLimit: Double // execution time limit
                         , probInfection: Double
                         , probDying: Double
                         , timeInfectious: Double
                        )

object DefaultConfiguration extends Configuration(
  seed = 0
  , Hz = 40
  , populationSz = 800
  , velocitySigma = 50
  , timeLimit = 1000
  , probInfection = 1.0 / 3
  , probDying = 0.1
  , timeInfectious = 10
  )


object DefaultConfiguration2 extends Configuration(
  seed = 0
  , Hz = 48
  , populationSz = 100
  , velocitySigma = 25
  , timeLimit = 1000
  , probInfection = 1.0 / 3
  , probDying = 0.1
  , timeInfectious = 15
)

