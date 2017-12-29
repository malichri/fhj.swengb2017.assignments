package at.fhj.swengb.apps.battleship

import at.fhj.swengb.apps.battleship.BattleShipProtobuf.Direction

import scala.collection.JavaConverters._
import at.fhj.swengb.apps.battleship.model._

object BattleShipProtocol {

// ---------------------------
// convert to proto messages
// ---------------------------
  def convert(v: Vessel): BattleShipProtobuf.Vessel = {
    val direction = {
      v.direction match {
        case Horizontal => Direction.Horizontal
        case Vertical => Direction.Vertical
      }
    }

    BattleShipProtobuf.Vessel.newBuilder()
      .setName(v.name.value)
      .setStartPos(convert(v.startPos))
      .setDirection(direction)
      .setSize(v.size).build()
  }

  def convert(f: Fleet): BattleShipProtobuf.Fleet = {
    BattleShipProtobuf.Fleet.newBuilder().addAllVessel(f.vessels.map(convert).asJava).build()
  }

  def convert(bf: BattleField): BattleShipProtobuf.BattleField = {
    BattleShipProtobuf.BattleField.newBuilder()
      .setFleet(convert(bf.fleet))
      .setWith(bf.width)
      .setHeight(bf.height).build()
  }


  def convert(g: BattleShipGame): BattleShipProtobuf.BattleShipGame = {
    BattleShipProtobuf.BattleShipGame.newBuilder()
      .setBattlefield(convert(g.battleField))
      .setCellWidth(g.battleField.width)
      .setCellHeight(g.battleField.height)
      .setLog(g.log.toString()).build()

  }

  def convert(p: BattlePos): BattleShipProtobuf.BattlePos = {
    BattleShipProtobuf.BattlePos.newBuilder().setX(p.x).setY(p.y).build()
  }


  // ---------------------------
  // convert it back from proto
  // ---------------------------
  def convert(v: BattleShipProtobuf.Vessel): Vessel = {
    val direction = {
      v.getDirection match {
        case Direction.Horizontal => Horizontal
        case Direction.Vertical => Vertical
      }
    }
    Vessel(NonEmptyString(v.getName), BattlePos(v.getStartPos.getX, v.getStartPos.getY), direction, v.getSize)

  }

  def convert(f: BattleShipProtobuf.Fleet): Fleet = {
    val vessels: Set[Vessel] = f.getVesselList.asScala.map(x => convert(x)).toSet
    Fleet(vessels)
  }

  def convert(bf: BattleShipProtobuf.BattleField) : BattleField = {
    BattleField(bf.getWith, bf.getHeight, convert(bf.getFleet))
  }

  def convert(g: BattleShipProtobuf.BattleShipGame): BattleShipGame = {
    BattleShipGame(convert(g.getBattlefield), (x: Int) => x.toDouble, (x: Int) => x.toDouble, x => ())
  }

  def convert(p: BattleShipProtobuf.BattlePos): BattlePos = {
    BattlePos(p.getX, p.getY)
  }
}