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

    val hasAirTraffic = true
    val airTrafficRate = 0.01f

    val hasReducedMobility = true

    val hasChosenFew = true
    val chosenFewRate = 0.05f

    val prevalenceRate = 0.01f
    val transmissibilityRate = 0.4f
    val deathRate = 0.25f
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
  infected.foreach(_.infected = true)
  // Vaccinate chosen few
  if (hasChosenFew) {
    val (vaccinated, _) = p1.splitAt(math.round(chosenFewRate * p1.size))
    vaccinated.foreach(_.immune = true)
  }
  // Kick-start the simulation in random order
  Random.shuffle(persons).foreach(_.mode())

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
    def isNotImmune = !immune
    def isHealthy = !infected

    // Actions

    def becomeInfected() {
      if (isHealthy && isNotImmune) {
        if (map(row)(col).exists(_.isInfectious)) {
          if (random < transmissibilityRate) {
            infected = true
            afterDelay(6) { becomeSick() }
          }
        }
      }
    }

    def becomeSick() {
      sick = true
      afterDelay(8) { becomeDead() }
    }

    def becomeDead() {
      if (random < deathRate) {
        dead = true
      } else {
        afterDelay(2) { becomeImmune() }
      }
    }

    def becomeImmune() {
      sick = false
      immune = true
      afterDelay(2) { becomeHealthy() }
    }

    def becomeHealthy() {
      immune = false
      infected = false
    }

    // Movements

    var row: Int = randomRow()
    var col: Int = randomColumn()
    map(row)(col) += this

    def updatePos(newRow: Int, newCol: Int) {
      if (dead) return
      if (row == newRow && col == newCol) return

      map(row)(col) -= this
      row = newRow
      col = newCol
      map(row)(col) += this

      becomeInfected()
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
        (randomRow(), randomColumn())

      } else {
        val free = neighbours()
                   .filterNot(e => map(e._1)(e._2).exists(p => p.isVisiblyInfectious))
                   .toList
        if (free.isEmpty) (row, col)
        else free(randomBelow(free.size))

      }
    }

    def modeDelay() = {
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

    def mode() {
      val next = randomMove()
      afterDelay(modeDelay()) {
        updatePos(next._1, next._2)
        mode()
      }
    }
  }

}
