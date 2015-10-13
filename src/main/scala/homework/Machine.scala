package homework

import homework.State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) { self =>
}

object Machine {
  def handle(i: Input): State[Machine, (Int, Int)] = State { (s: Machine) =>
    i match {
      case Coin => ((s.candies, s.coins+1), Machine(false, s.candies, s.coins+1))
      case Turn => s.locked match {
        case true => ((s.candies, s.coins), s)
        case false => ((s.candies-1, s.coins), Machine(true, s.candies-1, s.coins))
      }
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { (s: Machine) =>
    sequence(inputs.map(handle)).run(s) match {
      case (_, s) => ((s.coins, s.candies), s)
    }
  }
}
