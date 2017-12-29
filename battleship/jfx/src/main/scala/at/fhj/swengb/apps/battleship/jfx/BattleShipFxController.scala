package at.fhj.swengb.apps.battleship.jfx

import java.net.URL
import java.nio.file.{Files, Paths}
import java.util.ResourceBundle
import javafx.fxml.{FXML, Initializable}
import javafx.scene.control.TextArea
import javafx.scene.layout.GridPane

import at.fhj.swengb.apps.battleship.BattleShipProtobuf
import at.fhj.swengb.apps.battleship.BattleShipProtocol._
import at.fhj.swengb.apps.battleship.model.{BattleField, BattleShipGame, Fleet, FleetConfig}


class BattleShipFxController extends Initializable {


  @FXML private var battleGroundGridPane: GridPane = _

  var game: BattleShipGame = _
  /**
    * A text area box to place the history of the game
    */
  @FXML private var log: TextArea = _

  @FXML
  def newGame(): Unit = initGame()



  def saveGame(): Unit = saveGameStatus()

  def loadGame(): Unit = loadGameStatus()

  override def initialize(url: URL, rb: ResourceBundle): Unit = initGame()

  private def getCellHeight(y: Int): Double = battleGroundGridPane.getRowConstraints.get(y).getPrefHeight

  private def getCellWidth(x: Int): Double = battleGroundGridPane.getColumnConstraints.get(x).getPrefWidth

  def appendLog(message: String): Unit = log.appendText(message + "\n")

  /**
    * Create a new game.
    *
    * This means
    *
    * - resetting all cells to 'empty' state
    * - placing your ships at random on the battleground
    *
    */
  def init(game : BattleShipGame) : Unit = {
    battleGroundGridPane.getChildren.clear()
    for (c <- game.getCells) {
      battleGroundGridPane.add(c, c.pos.x, c.pos.y)
    }
    game.getCells().foreach(c => c.init)
  }


  private def initGame(): Unit = {
    val game: BattleShipGame = createGame()
    init(game)
    appendLog("New game started.")
  }

  private def createGame(): BattleShipGame = {
    val field = BattleField(10, 10, Fleet(FleetConfig.Standard))

    val battleField: BattleField = BattleField.placeRandomly(field)

    BattleShipGame(battleField, getCellWidth, getCellHeight, appendLog)
  }

  def saveGameStatus(): Unit = {
    convert(game).writeTo(Files.newOutputStream(Paths.get("battleship/SaveGame.bin")))
    appendLog("game saved")
  }


  def loadGameStatus(): Unit = {
    val reload = BattleShipProtobuf.BattleShipGame.parseFrom(Files.newInputStream(Paths.get("battleship/SaveGame.bin")))


    val gameWithOldValues = BattleShipGame(convert(reload).battleField, getCellWidth, getCellHeight, appendLog)
    gameWithOldValues.actualState = convert(reload).actualState
    init(gameWithOldValues)
    gameWithOldValues.updateState(game.actualState.length)
    appendLog("Successful loaded")
  }
}