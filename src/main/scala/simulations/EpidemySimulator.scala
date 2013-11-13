package simulations

import scala.collection.mutable.{Set => MSet}
import scala.math.random
import scala.util.Random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val modeQuanta = 5

    val prevalenceRate = 0.01f // of population
    val transmissibilityRate = 0.4f
    val deathRate = 0.25f

    val hasAirTraffic = false
    val airTrafficRate = 0.01f

    val hasReducedMobility = false

    val hasChosenFew = false
    val chosenFewRate = 0.05f // of population
  }

  import SimConfig._

  // Neighbour lookup map
  val map = Array.fill(roomRows, roomColumns)(MSet.empty[Person])

  // Spawn people
  val persons: List[Person] = Range(0, population).map(new Person(_)).toList

  // Prep
  val p0 = persons
  // Infect people
  val (infected, p1) = p0.splitAt(math.round(prevalenceRate * p0.size))
  infected.foreach{ p => {
    p.infected = true
    p.scheduleBecomeSick()
  }}
  // Vaccinate chosen few
  if (hasChosenFew) {
    val (vaccinated, _) = p1.splitAt(math.round(chosenFewRate * p1.size))
    vaccinated.foreach{ _.immune = true }
  }
  // Kick-start the simulation in random order
  Random.shuffle(persons).foreach{ _.scheduleMode() }

  def wrapBy(by: Int)(v: Int) = {
    if (v < 0) by - 1
    else if (v > by - 1) 0
    else v
  }

  def wrapRows(v: Int) = wrapBy(roomRows)(v)
  def wrapColumns(v: Int) = wrapBy(roomColumns)(v)

  def randomRow() = randomBelow(roomRows)
  def randomColumn() = randomBelow(roomColumns)

  class Person(val id: Int) {

    // Status

    var infected = false
    var sick = false
    var immune = false
    var dead = false

    def isVisiblyInfectious = sick || dead
    def isInfectious = infected
    def canBeInfected = !infected && !sick && !immune && !dead

    // Actions

    def ifPossible()(a: => Unit) = if (!dead) a

    def scheduleBecomeInfected() = afterDelay(0) { becomeInfected() }
    def scheduleBecomeSick() = afterDelay(6) { becomeSick() }
    def scheduleBecomeDead() = afterDelay(8) { becomeDead() }
    def scheduleBecomeImmune() = afterDelay(2) { becomeImmune() }
    def scheduleBecomeHealthy() = afterDelay(2) { becomeHealthy() }

    def becomeInfected() = ifPossible() {
      if (canBeInfected && random < transmissibilityRate) {
          infected = true
          scheduleBecomeSick()
      }
    }

    def becomeSick() = ifPossible() {
      sick = true
      scheduleBecomeDead()
    }

    def becomeDead() = ifPossible() {
      if (random < deathRate) {
        dead = true
      } else {
        scheduleBecomeImmune()
      }
    }

    def becomeImmune() = ifPossible() {
      sick = false
      immune = true
      scheduleBecomeHealthy()
    }

    def becomeHealthy() = ifPossible() {
      immune = false
      infected = false
    }

    // Movements

    var row: Int = randomRow()
    var col: Int = randomColumn()
    map(row)(col) += this

    def updatePos(newRow: Int, newCol: Int) = {
      map(row)(col) -= this
      row = newRow; col = newCol
      map(row)(col) += this

      if (map(row)(col).exists{ _.isInfectious }) {
        scheduleBecomeInfected()
      }
    }

    def neighbours() = {
      (row, wrapColumns(col - 1)) ::
        (row, wrapColumns(col + 1)) ::
        (wrapRows(row - 1), col) ::
        (wrapRows(row + 1), col) ::
        Nil
    }

    def randomMove() = {
      if (hasAirTraffic && random < airTrafficRate) {
        Some(randomRow(), randomColumn())

      } else {
        val free = neighbours()
                   .filterNot{ e => map(e._1)(e._2).exists{ p => p.isVisiblyInfectious } }
                   .toList
        if (free.isEmpty) None
        else Some(free(randomBelow(free.size)))

      }
    }

    // Main logic

    def scheduleMode(): Unit = afterDelay(modeDelay()) { mode() }

    def modeDelay(): Int = {
      if (hasReducedMobility) {
        if (isVisiblyInfectious) {
          randomBelow(4 * modeQuanta)
        } else {
          randomBelow(2 * modeQuanta)
        }

      } else {
        randomBelow(modeQuanta)

      }
    }

    def mode(): Unit = ifPossible() {
      randomMove().foreach { pos =>
        updatePos(pos._1, pos._2)
      }
      scheduleMode()
    }

  }

}
