/********************************************************************
 * One individual in the population
 *
 * Pepe Gallardo, 2020
 *
 * Partly based on Java Event-Driven Simulator by
 * Robert Sedgewick and Kevin Wayne
 *******************************************************************/

package sim

import scala.math.sqrt

object Individual {
  object Status extends Enumeration {
    type Status = Value
    val Infected, NonInfected, Recovered, Dead = Value
  }

  private val Infinity = java.lang.Double.POSITIVE_INFINITY

  def apply( rx: Double
           , ry: Double      // position
           , vx: Double
           , vy: Double      // velocity
           , mass: Double    // mass
           , radius: Double  // radius
           , status: Status.Status
           ): Individual = new Individual(rx, ry, vx, vy, mass, radius, status)

  def random(rnd: Random, conf: Configuration): Individual = {
    val radius = rnd.uniform(2.0, 8.0)
    val mass = 100*radius
    val ind = new Individual(
      rnd.uniform(BoundingBox.left+radius, BoundingBox.right-radius)
      , rnd.uniform(BoundingBox.top+radius, BoundingBox.bottom-radius)
      , rnd.normal(mu = 0, sigma = conf.velocitySigma)
      , rnd.normal(mu = 0, sigma = conf.velocitySigma)
      , mass
      , radius
      , Status.NonInfected
    )
    return ind
  }
}

class Individual( private var rx: Double
                , private var ry: Double // position
                , private var vx: Double
                , private var vy: Double // velocity
                , private val mass: Double // mass
                , private val radius: Double // radius
                , private var status: Individual.Status.Status
                ) {
  import Individual._

  private var numCollisions = 0 // number of collisions for this individual so far

  def move(dt: Double): Unit =
    if(!isDead) {
      rx += vx * dt
      ry += vy * dt
    }

  private val pi2 = 2*math.Pi
  def drawOn(g2D: Graphics2D): Unit =
    if(!isDead) {
      g2D.beginPath()
      g2D.arc(rx, ry, radius, 0, pi2, false)
      g2D.fillStyle = status match {
        case Status.NonInfected => Colors.nonInfected
        case Status.Infected    => Colors.infected
        case Status.Recovered   => Colors.recovered
        case Status.Dead        => Colors.dead
      }
      g2D.fill()
    }

  def canGetInfected: Boolean = status == Status.NonInfected

  def infect(): Unit =
    if(canGetInfected)
      status = Status.Infected

  def isInfected: Boolean = status == Status.Infected

  def isDead: Boolean = status == Status.Dead

  def endInfection(die: Boolean): Unit =
    status =
      if(die) {
        vx = 0
        vy = 0
        Status.Dead
      } else
        Status.Recovered

  def collisions: Int = numCollisions

  def collidesWith(that: Individual): Boolean = {
    val dx = this.rx - that.rx
    val dy = this.ry - that.ry
    val d2 = dx*dx + dy*dy
    val sigma = this.radius + that.radius
    return d2 <= sigma*sigma
  }

  def timeToHit(that: Individual): Double = {
    if(this eq that)
      return Infinity
    if(that.isDead)
      return Infinity
    val dx = that.rx - this.rx
    val dy = that.ry - this.ry
    val dvx = that.vx - this.vx
    val dvy = that.vy - this.vy
    val dvdr = dx * dvx + dy * dvy
    if(dvdr > 0)
      return Infinity
    val dvdv = dvx * dvx + dvy * dvy
    if(dvdv == 0)
      return Infinity
    val drdr = dx*dx + dy*dy
    val sigma = this.radius + that.radius
    val d = (dvdr*dvdr) - dvdv*(drdr - sigma*sigma)
    if(d < 0)
      return Infinity
    return -(dvdr + sqrt(d)) / dvdv
  }

  def timeToHitVerticalWall: Double =
    if(vx > 0)
      (BoundingBox.right - rx - radius) / vx
    else if(vx < 0)
      (radius - rx + BoundingBox.left) / vx
    else
      Infinity

  def timeToHitHorizontalWall: Double =
    if(vy > 0)
      (BoundingBox.bottom - ry - radius) / vy
    else if(vy < 0)
      (radius - ry + BoundingBox.top) / vy
    else
      Infinity


  val wallX = -50
  val wallYTop = -125
  val wallYBottom = 125

  def timeToHitVerticalWallll: Double =
    if(vx > 0) {
      if(rx > wallX)
        Infinity
      else {
        val dx = wallX - rx - radius
        val t = dx / vx
        val y = ry + vy*t
        if(y >= wallYTop && y <= wallYBottom)
          t
        else
          Infinity
      }
    } else if(vx < 0) {
      if(rx < wallX)
        Infinity
      else {
        val dx = wallX - rx + radius
        val t = dx / vx
        val y = ry + vy*t
        if(y >= wallYTop && y <= wallYBottom)
          t
        else
          Infinity
      }
    } else
      Infinity

  def bounceOff(that: Individual): Unit = {
    val dx = that.rx - this.rx
    val dy = that.ry - this.ry
    val dvx = that.vx - this.vx
    val dvy = that.vy - this.vy
    val dvdr = dx * dvx + dy * dvy // dv dot dr
    val dist = this.radius + that.radius // distance between particle centers at collision
    // magnitude of normal force
    val magnitude = 2 * this.mass * that.mass * dvdr / ((this.mass + that.mass) * dist)
    // normal force, and in x and y directions
    val fx = magnitude * dx / dist
    val fy = magnitude * dy / dist

    if(!this.isDead) {
      // println(dx, dy, dvx, dvy, this.mass, that.mass, dx * dvx, dy * dvy, dvdr, magnitude, fx, fy)
      this.vx += fx / this.mass
      this.vy += fy / this.mass
    }
    if(!that.isDead) {
      that.vx -= fx / that.mass
      that.vy -= fy / that.mass
    }
    // update collision counts
    this.numCollisions += 1
    that.numCollisions += 1
  }

  def bounceOffVerticalWall(): Unit = {
    vx = -vx
    numCollisions += 1
  }

  def bounceOffHorizontalWall(): Unit = {
    vy = -vy
    numCollisions += 1
  }
}


