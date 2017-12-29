package at.fhj.swengb.apps.battleship

import at.fhj.swengb.apps.battleship.BattleShipProtobuf.Direction
import at.fhj.swengb.apps.battleship.model._
import at.fhj.swengb.apps.battleship.BattleShipProtocol
import org.scalacheck.{Gen, Prop}
import org.scalatest.WordSpecLike
import org.scalatest.prop.Checkers
import scala.collection.JavaConverters._

class BattleShipGameProtocolSpec extends WordSpecLike {


  "BattleShipProtocol" should {
    /*"be deserializable" in {
      Checkers.check(Prop.forAll(battleShipGameGen) {
        expected: BattleShipGame => {
          val actual = BattleShipProtocol.convert(BattleShipProtocol.convert(expected))
          actual == expected
        }
      })
    }*/
    " battlePos Test" in {
      val expectedPos = BattlePos(2408, 207)
      val actualPos: BattleShipProtobuf.BattlePos = BattleShipProtocol.convert(expectedPos)

      assert(actualPos.getX == expectedPos.x)
      assert(actualPos.getY == expectedPos.y)
    }

    " vessel Test" in {
      val expectedVessel = Vessel(NonEmptyString("Long"), BattlePos(0,0), Vertical, 3)
      val actualVessel : BattleShipProtobuf.Vessel = BattleShipProtocol.convert(expectedVessel)

      val direction = {
        actualVessel.getDirection match {
          case Direction.Horizontal => Horizontal
          case Direction.Vertical => Vertical
        }
      }

      assert(actualVessel.getStartPos.getX == expectedVessel.startPos.x)
      assert(actualVessel.getStartPos.getY == expectedVessel.startPos.y)

      assert(direction == expectedVessel.direction)

      assert(actualVessel.getName == expectedVessel.name.value)

      assert(actualVessel.getSize == expectedVessel.size)
    }

    " fleet Test:" in {
      val expectedFleet = Fleet(Set(Vessel(NonEmptyString("Long"), BattlePos(0,0), Vertical, 3),
        Vessel(NonEmptyString("Ling"), BattlePos(10,10), Horizontal, 3)))

      val actualFleet : BattleShipProtobuf.Fleet = BattleShipProtocol.convert(expectedFleet)

      assert(expectedFleet == BattleShipProtocol.convert(actualFleet))
    }

    " battlefield Test:" in {
      val expectedBattlefield = BattleField(30, 30, Fleet(Set(Vessel(NonEmptyString("Long"), BattlePos(0,0), Vertical, 3),
        Vessel(NonEmptyString("Ling"), BattlePos(10,10), Horizontal, 3))))

      val actualBattlefield : BattleShipProtobuf.BattleField = BattleShipProtocol.convert(expectedBattlefield)

      assert(expectedBattlefield.width == actualBattlefield.getWith)
      assert(expectedBattlefield.height == actualBattlefield.getHeight)
      assert(expectedBattlefield.fleet == BattleShipProtocol.convert(actualBattlefield.getFleet))
    }

    " battleshipGame Test: " in {
      val battlefield = BattleField(30, 30, Fleet(FleetConfig.Standard))
      val expectedGame = BattleShipGame(battlefield, (x : Int) => x.toDouble, (x:Int) => x.toDouble, x => ())

      val actualGame : BattleShipProtobuf.BattleShipGame = BattleShipProtocol.convert(expectedGame)

      assert(expectedGame.battleField == BattleShipProtocol.convert(actualGame.getBattlefield))
    }
  }
}