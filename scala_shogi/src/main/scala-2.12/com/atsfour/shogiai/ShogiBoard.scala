package com.atsfour.shogiai

import scala.util.Random
import scalafx.application.JFXApp
import scalafx.geometry.Pos
import scalafx.scene.control.Label
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Polygon, Rectangle}
import scalafx.scene.text.Font
import scalafx.scene.{Group, Scene}

/** JFXApp { を使い、traitの設定をしつつ、*/
object ShogiBoard extends JFXApp {
  var toIndexStock: List[Int] = Nil
  var ouTookKomaStock: List[Int] = Nil
  var canNotTyuAi: List[Int] = Nil
  var canTyuAi: Boolean = true
  var notGetBackKoma: Boolean = true

  val initalKomas: List[Koma] = testBoard match { case Board(komas) => komas }
  var initalChoiceKoma = true

  var board: Board = Board(
    Koma(ClickedKomaState.A, 81, true, initalChoiceKoma) :: Koma(ClickedKomaState.B, 87, true, initalChoiceKoma) ::
      Koma(ClickedKomaState.C, 93, true, initalChoiceKoma) :: Koma(ClickedKomaState.D, 99, true, initalChoiceKoma) :: //初期化
      initalKomas)
  var pastBoard: Board = board

  //todo Stateを減らしたい
  /** StateとFlagを定義 */
  var selectedCellIndex: Option[Int] = None
  var optIsSenteKomaState: Option[Boolean] = None
  var optOnBoardKomaState: Option[Boolean] = None
  var isSenteTurnState: Boolean = true
  var stockNariIndex = -1

  var isCheckmate: Option[Boolean] = None

  var clickedKomaKind: ClickedKomaState = ClickedKomaState.None
  sealed abstract class ClickedKomaState(val name: String)
  object ClickedKomaState {
    case object None extends ClickedKomaState("？")
    case object Fu extends ClickedKomaState("歩")
    case object Kyo extends ClickedKomaState("香")
    case object Kei extends ClickedKomaState("桂")
    case object Gin extends ClickedKomaState("銀")
    case object Kin extends ClickedKomaState("金")
    case object Ou extends ClickedKomaState("王")
    case object Kaku extends ClickedKomaState("角")
    case object Hisha extends ClickedKomaState("飛")
    case object To extends ClickedKomaState("と")
    case object NariKyo extends ClickedKomaState("杏")
    case object NariKei extends ClickedKomaState("圭")
    case object NariGin extends ClickedKomaState("全")
    case object Uma extends ClickedKomaState("馬")
    case object Ryu extends ClickedKomaState("龍")

    case object Sen extends ClickedKomaState("先")
    case object Go extends ClickedKomaState("後")
    case object Te extends ClickedKomaState("手")
    case object No extends ClickedKomaState("の")
    case object Ka extends ClickedKomaState("勝")
    case object Chi extends ClickedKomaState("ち")

    case object Not extends ClickedKomaState("不")
    case object Na extends ClickedKomaState("成")
    case object Ri extends ClickedKomaState("り")
    case object Slash extends ClickedKomaState("/")

    case object Ban extends ClickedKomaState("番")
    case object Ni extends ClickedKomaState("二")
    case object De extends ClickedKomaState("で")
    case object Su extends ClickedKomaState("す")

    case object Ma extends ClickedKomaState("待")
    case object Ltu extends ClickedKomaState("っ")
    case object TaHira extends ClickedKomaState("た")

    case object A extends ClickedKomaState("A")
    case object B extends ClickedKomaState("B")
    case object C extends ClickedKomaState("C")
    case object D extends ClickedKomaState("D")

    case object Tumi extends ClickedKomaState("詰")
    case object Mi extends ClickedKomaState("み")
    case object Ahira extends ClickedKomaState("あ")
    case object Nahira extends ClickedKomaState("な")
    case object Si extends ClickedKomaState("し")

    case object One extends ClickedKomaState("1")
    case object Two extends ClickedKomaState("2")
    case object Three extends ClickedKomaState("3")
    case object Four extends ClickedKomaState("4")
    case object Five extends ClickedKomaState("5")
    case object Six extends ClickedKomaState("6")
    case object Seven extends ClickedKomaState("7")
    case object Eight extends ClickedKomaState("8")
    case object Nine extends ClickedKomaState("9")
    case object Ten extends ClickedKomaState("10")
    case object Eleven extends ClickedKomaState("11")
    case object Twelve extends ClickedKomaState("12")
    case object Thirteen extends ClickedKomaState("13")
    case object Fourteen extends ClickedKomaState("14")
    case object Fifteen extends ClickedKomaState("15")
    case object Sixteen extends ClickedKomaState("16")
    case object Seventeen extends ClickedKomaState("17")
    case object Eighteen extends ClickedKomaState("18")
    case object Blank extends ClickedKomaState("")

    lazy val values = Seq(None, Fu, Kyo, Kei, Gin, Kin, Ou, Kaku, Hisha, To, NariKyo, NariKei, NariGin, Uma, Ryu, Sen, Go, Te, No, Ka, Chi, Not, Na, Ri)
  }

  /** 将棋盤のテンプレートの切り替え */
  var (isWin, isCanNari, isNifu) = (false, false, false)
  var (isSenteHandOver,isGoteHandOver) = (false, false)

  def boardSwitch :Board = {
    /** 初期化と待ったのボタン更新 */
    val transitionKoma: Boolean = true
    val onBoardKomas: List[Koma] = board match { case Board(komas) => komas.takeRight(40) }
    val pastKomas: List[Koma] = pastBoard match { case Board(komas) => komas.takeRight(40) }

    board = if (onBoardKomas != pastKomas && !isCanNari) { //待ったを出していいとき
      val addBoard: Board = Board(
        Koma(ClickedKomaState.A, 81, true, transitionKoma) :: Koma(ClickedKomaState.B, 87, true, transitionKoma) ::
          Koma(ClickedKomaState.C, 93, true, transitionKoma) :: Koma(ClickedKomaState.D, 99, true, transitionKoma) :: //初期化
          Koma(ClickedKomaState.Tumi, 105, isSenteTurnState, transitionKoma) :: //詰み判定
          Koma(ClickedKomaState.Ma, 117, true, transitionKoma) :: Koma(ClickedKomaState.Ltu, 123, true, transitionKoma) :: Koma(ClickedKomaState.TaHira, 129, true, transitionKoma) :: //待った
          onBoardKomas)
      addBoard
    } else {
      val addInitializeBoard: Board = Board(
        Koma(ClickedKomaState.A, 81, true, transitionKoma) :: Koma(ClickedKomaState.B, 87, true, transitionKoma) ::
          Koma(ClickedKomaState.C, 93, true, transitionKoma) :: Koma(ClickedKomaState.D, 99, true, transitionKoma) :: //初期化
          Koma(ClickedKomaState.Tumi, 105, isSenteTurnState, transitionKoma) :: //詰み判定
          onBoardKomas)
      addInitializeBoard
    }

    /** その他テンプレートの更新 */
    val realKomas: List[Koma] = board match { case Board(komas) => komas }
    val displayKoma: Boolean = true
    board = (isWin, isCanNari, isNifu, isSenteTurnState) match { //1手指すと出てくる
      case (false, false, false, true) => {
        val normalBoard: Board = Board( //先手番
          Koma(ClickedKomaState.Sen, 107, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 108, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ban, 109, isSenteTurnState, displayKoma) ::
            realKomas)
        normalBoard
      }
      case (false, false, false, false) => {
        val normalBoard: Board = Board( //後手番
          Koma(ClickedKomaState.Go, 107, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 108, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ban, 109, isSenteTurnState, displayKoma) ::
            realKomas)
        normalBoard
      }
      case (true, _, _, false) => { //駒を取った時には後手の番に移っている
        val SenteWinBoard: Board = Board( //先手の勝ち
          Koma(ClickedKomaState.Sen, 106, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 107, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.No, 108, !isSenteTurnState, displayKoma) ::
            (Koma(ClickedKomaState.Ka, 109, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 110, !isSenteTurnState, displayKoma) ::
              realKomas))
        SenteWinBoard
      }
      case (true, _, _, true) => {
        val GoteWinboard: Board = Board( //後手の勝ち
          Koma(ClickedKomaState.Go, 106, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 107, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.No, 108, !isSenteTurnState, displayKoma) ::
            (Koma(ClickedKomaState.Ka, 109, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 110, !isSenteTurnState, displayKoma) ::
              realKomas))
        GoteWinboard
      }
      case (_, true, _, true) => {
        val SenteWinBoard: Board = Board( //成りor不成
          Koma(ClickedKomaState.Na, 106, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ri, 107, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Slash, 108, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Not, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Na, 110, isSenteTurnState, displayKoma) ::
            realKomas)
        SenteWinBoard
      }
      case (_, true, _, false) => {
        val GoteWinboard: Board = Board( //成りor不成
          Koma(ClickedKomaState.Na, 106, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ri, 107, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Slash, 108, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Not, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Na, 110, isSenteTurnState, displayKoma) :: realKomas)
        GoteWinboard
      }
      case (_, _, true, true) => {
        val SenteNifuBoard: Board = Board( //二歩です
          Koma(ClickedKomaState.Ni, 107, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Fu, 108, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.De, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Su, 110, isSenteTurnState, displayKoma) ::
            realKomas)
        SenteNifuBoard
      }
      case (_, _, true, false) => {
        val GoteNifuboard: Board = Board( //二歩です
          Koma(ClickedKomaState.Ni, 107, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Fu, 108, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.De, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Su, 110, isSenteTurnState, displayKoma) ::
            realKomas)
        GoteNifuboard
      }
      case _ => {
        val normalBoard: Board = Board(realKomas)
        normalBoard
      }
    }

    val secondKomas: List[Koma] = board match { case Board(komas) => komas }

    board = isCheckmate match {
      case Some(true) => isSenteTurnState match {
        case true => { //先手の手番 => 先手詰み(先手)
          val senteTumiBoard: Board = Board(
            Koma(ClickedKomaState.Sen, 127, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 128, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Tumi, 133, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Mi, 134, isSenteTurnState, displayKoma) ::
              secondKomas)
          senteTumiBoard
        }
        case false => { //後手の手番 => 後手詰み(後手)
          val goteTumiBoard: Board = Board(
            Koma(ClickedKomaState.Go, 85, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 86, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Tumi, 91, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Mi, 92, isSenteTurnState, displayKoma) ::
              secondKomas)
          goteTumiBoard
        }
      }
      case Some(false) => isSenteTurnState match {
        case true => { //先手の手番 => 詰みなし(先手表示)
        val senteTumiBoard: Board = Board(
            Koma(ClickedKomaState.Tumi, 127, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Mi, 128, isSenteTurnState, displayKoma) ::
              Koma(ClickedKomaState.Nahira, 133, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Si, 134, isSenteTurnState, displayKoma) ::
              secondKomas)
          senteTumiBoard
        }
        case false => { //後手の手番 => 詰みなし(後手表示)
        val goteTumiBoard: Board = Board(
            Koma(ClickedKomaState.Tumi, 85, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Mi, 86, isSenteTurnState, displayKoma) ::
              Koma(ClickedKomaState.Nahira, 91, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Si, 92, isSenteTurnState, displayKoma) ::
              secondKomas)
          goteTumiBoard
        }
      }
      case _ => board
    }

    val lastKomas: List[Koma] = board match { case Board(komas) => komas }

    def handOverlap(handIndex: Int): ClickedKomaState = {
      lastKomas.count(lastKomas => lastKomas.index == handIndex) match {
        case 2 => ClickedKomaState.Two
        case 3 => ClickedKomaState.Three
        case 4 => ClickedKomaState.Four
        case 5 => ClickedKomaState.Five
        case 6 => ClickedKomaState.Six
        case 7 => ClickedKomaState.Seven
        case 8 => ClickedKomaState.Eight
        case 9 => ClickedKomaState.Nine
        case 10 => ClickedKomaState.Ten
        case 11 => ClickedKomaState.Eleven
        case 12 => ClickedKomaState.Twelve
        case 13 => ClickedKomaState.Thirteen
        case 14 => ClickedKomaState.Fourteen
        case 15 => ClickedKomaState.Fifteen
        case 16 => ClickedKomaState.Sixteen
        case 17 => ClickedKomaState.Seventeen
        case 18 => ClickedKomaState.Eighteen
        case _ => ClickedKomaState.Blank
      }
    }

    val BlankKomas: List[Koma] = {
      (Koma(handOverlap(100), 94, false, displayKoma) :: Koma(handOverlap(101), 95, false, displayKoma) :: Koma(handOverlap(102), 96, false, displayKoma) ::
        Koma(handOverlap(103), 97, false, displayKoma) :: Koma(handOverlap(104), 98, false, displayKoma) ::
        Koma(handOverlap(88), 82, false, displayKoma) :: Koma(handOverlap(89), 83, false, displayKoma) :: Koma(handOverlap(90), 84, false, displayKoma) ::
        Koma(handOverlap(112), 118, true, displayKoma) :: Koma(handOverlap(113), 119, true, displayKoma) :: Koma(handOverlap(114), 120, true, displayKoma) ::
        Koma(handOverlap(115), 121, true, displayKoma) :: Koma(handOverlap(116), 122, true, displayKoma) ::
        Koma(handOverlap(124), 130, true, displayKoma) :: Koma(handOverlap(125), 131, true, displayKoma) :: Koma(handOverlap(126), 132, true, displayKoma) ::
        lastKomas)
    }
    val outputKomas = BlankKomas.filterNot(koma => koma.kind == ClickedKomaState.Blank)

    board = {
      val addHandNumberBoard: Board = Board(outputKomas)
      addHandNumberBoard
    }

    board
  }

  /** 描画定義 */
  def inBord(index: Int) = index <= 80
  def outOfBord(index: Int) = index >= 81 && index <= 134 //持ち駒,テンプレートの描画をする場所

  //Sceneクラスをインスタンス化したもの
  val boardScene = new Scene {
    fill = Transparent
    content = boardObjPane
  }

  /* `stage` は、trait JFXAppの中で、`null` で定義されてる */
  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Scala Shogi!"
    width = 1850
    height = 800
    scene = boardScene
  }

  def repaint: Unit = {
    boardScene.content = boardObjPane
  }

  def boardObjPane = {
    val pane = new GridPane
    board.cells.zipWithIndex.foreach { //cellsを繰り返し呼んでいる
      case (optKoma, index) => {
        if (inBord(index)) {
          val x = index % 9
          val y = index / 9
          pane.add(cellObjGroup(optKoma, index), x, y)
        }
        if (outOfBord(index))  {
          val x = (index - 81) % 6 + 10
          val y = (index - 81) / 6
          pane.add(cellObjGroup(optKoma, index), x, y) //負の座標は.addできない
        }
      }
    }
    pane
  }

  /** セルの描画処理, ゲーム内での駒の動きはここで定義している */
  def cellObjGroup(komaOpt: Option[Koma], clickedIndex: Int): Group = {

    /** 以下、駒をクリックした時に使う関数のまとまり */
    /* clickedIndexと一致した場合の駒情報を取得する (komaの中のClickedIndexと一致する、という条件が必要(本当は)) */
    val optClickedKomaKind: Option[ClickedKomaState] = komaOpt.map(koma => koma.kind)
    val optIsSenteKoma: Option[Boolean] = komaOpt.map(koma => koma.isSente)
    val optOnBoard: Option[Boolean] = komaOpt.map(koma => koma.onBoard)

    val existSelectedCellIndex = selectedCellIndex.getOrElse(-1)
    val absMoveDistance = Math.abs(existSelectedCellIndex - clickedIndex) //駒の移動距離の絶対値を定義
    val moveDistance = existSelectedCellIndex - clickedIndex //駒の移動距離を定義

    def toMoveBoard: Boolean = clickedIndex <= 80
    def handPlace: Boolean = (clickedIndex >= 81 && clickedIndex <= 134) && (clickedIndex - 81) % 6 != 0 && (clickedIndex-81) / 6 != 4

    def fromOnBoard(num: Int): Boolean = num <= 80
    def fromOutOfBoard(num: Int): Boolean = num >= 81 && num <= 134
    def switchTurn(nextTurn: Boolean): Boolean = if (nextTurn) false else true

    /** 盤面内を横切っていないか */
    def notCrossOnBoard(index: Int): Boolean = {
      if ((index % 9) + 1 == 9) { //9筋の時
        if (moveDistance == 8 || moveDistance == -1 || moveDistance == -10 || moveDistance == -19 || moveDistance == 17) false
        else true
      } else if ((index % 9) + 1 == 1) { //1筋の時
        if (moveDistance == -8 || moveDistance == 1 || moveDistance == 10 || moveDistance == -17 || moveDistance == 19) false
        else true
      } else true
    }

    /** 駒に応じて、動けるかどうかを判定する関数 */
    def canMove(koma: ClickedKomaState, num: Int): Boolean = {
      val canMove: Boolean = {
        if (isSenteTurnState) {
          koma match {
            case ClickedKomaState.Fu => moveDistance == 9 && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Kyo => moveDistance % 9 == 0 && moveDistance > 0 && board.upJumpCheck(num, clickedIndex) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Kei => (moveDistance == 17 || moveDistance == 19) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Gin => (absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == 9) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Kin => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Ou => (absMoveDistance == 1 || absMoveDistance == 9 || absMoveDistance == 8 || absMoveDistance == 10) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Kaku => (((board.leftUpRightDownMove(num, clickedIndex)&& board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex)) //左上から右下方向
                || (board.rightUpLeftDownMove(num, clickedIndex) && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex))) //右上から左下方向
                && board.fromToMoveBoard(num, clickedIndex))
            case ClickedKomaState.Hisha => (((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
                || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex))) //横方向
                && board.fromToMoveBoard(num, clickedIndex))
            case ClickedKomaState.To => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.NariKyo => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.NariKei => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.NariGin => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Uma => ((board.leftUpRightDownMove(num, clickedIndex) && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex))
              || (board.rightUpLeftDownMove(num, clickedIndex) && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex))
              || (absMoveDistance == 1 || absMoveDistance == 9)) && notCrossOnBoard(num) && board.fromToMoveBoard(num, clickedIndex)
            case ClickedKomaState.Ryu => ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
              || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex)) //横方向
              || (absMoveDistance == 8 || absMoveDistance == 10)) && notCrossOnBoard(num) && board.fromToMoveBoard(num, clickedIndex)
            case _ => false
          }
        } else {
          koma match {
            case ClickedKomaState.Fu => moveDistance == -9 && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Kyo => (moveDistance % 9 == 0 && moveDistance < 0 && board.downJumpCheck(num, clickedIndex)) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Kei => (moveDistance == -17 || moveDistance == -19) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Gin => (absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == -9) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Kin => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Ou => (absMoveDistance == 1 || absMoveDistance == 9 || absMoveDistance == 8 || absMoveDistance == 10) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Kaku => (((board.leftUpRightDownMove(num, clickedIndex)&& board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex)) //左上から右下方向
              || (board.rightUpLeftDownMove(num, clickedIndex) && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex))) //右上から左下方向
              && board.fromToMoveBoard(num, clickedIndex))
            case ClickedKomaState.Hisha => (((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
              || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex))) //横方向
              && board.fromToMoveBoard(num, clickedIndex))
            case ClickedKomaState.To => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.NariKyo => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.NariKei => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.NariGin => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && board.fromToMoveBoard(num, clickedIndex) && notCrossOnBoard(num)
            case ClickedKomaState.Uma => ((board.leftUpRightDownMove(num, clickedIndex) && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex))
              || (board.rightUpLeftDownMove(num, clickedIndex) && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex))
              || (absMoveDistance == 1 || absMoveDistance == 9)) && notCrossOnBoard(num) && board.fromToMoveBoard(num, clickedIndex)
            case ClickedKomaState.Ryu => ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
              || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex)) //横方向
              || (absMoveDistance == 8 || absMoveDistance == 10)) && notCrossOnBoard(num) && board.fromToMoveBoard(num, clickedIndex)
            case _ => false
          }
        }
      }
      canMove
    }

    def initializeTumiState = {
      isCheckmate = None
      toIndexStock = Nil
      ouTookKomaStock = Nil
      notGetBackKoma = true
      canNotTyuAi = Nil
      canTyuAi = true
    }

    def isThereKomaKind(index: Int) = board.findKoma(index) match {
      case Some((Koma(kind, index, isSente, onBoard), i)) => Some(kind)
      case None => None
    }
    def isThereSenteKoma(index: Int) = board.findKoma(index) match {
      case Some((Koma(kind, index, isSente, onBoard), i)) => Some(isSente)
      case None => None
    }
    def isNotFriendKoma(index: Int): Boolean = {
      isSenteTurnState match {
        case true => !isThereSenteKoma(index).contains(true)
        case false => !isThereSenteKoma(index).contains(false)
      }
    }
    def isNotEnemyKoma(index: Int): Boolean = {
      !isSenteTurnState match {
        case true => !isThereSenteKoma(index).contains(true)
        case false => !isThereSenteKoma(index).contains(false)
      }
    }

    /** 以下、王が詰んでいるかのチェック関数 */
    def isCheckmateCheck: Boolean = {

      /** すべての駒を調べた時、そこへ移動できるか調べる関数(from,to引数としたを場所に応じて) */
      def canMovePlace(koma: ClickedKomaState, fromIndex: Int, toIndex: Int, isTumasuKoma: Boolean, board: Board): Boolean = {
        val absPlaceMoveDistance = Math.abs(fromIndex - toIndex) //駒の移動距離の絶対値を定義
        val placeMoveDistance = fromIndex - toIndex //駒の移動距離を定義

        def goteKomaMove = koma match {
            case ClickedKomaState.Fu => placeMoveDistance == -9 && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Kyo => (placeMoveDistance % 9 == 0 && placeMoveDistance < 0 && board.downJumpCheck(fromIndex, toIndex)) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Kei => (placeMoveDistance == -17 || placeMoveDistance == -19) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Gin => (absPlaceMoveDistance == 8 || absPlaceMoveDistance == 10 || placeMoveDistance == -9) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Kin => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == -8 || placeMoveDistance == -10) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Ou => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || absPlaceMoveDistance == 8 || absPlaceMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Kaku => (((board.leftUpRightDownMove(fromIndex, toIndex) && board.leftUpJumpCheck(fromIndex, toIndex) && board.rightDownJumpCheck(fromIndex, toIndex)) //左上から右下方向
              || (board.rightUpLeftDownMove(fromIndex, toIndex) && board.rightUpJumpCheck(fromIndex, toIndex) && board.leftDownJumpCheck(fromIndex, toIndex))) //右上から左下方向
              && board.fromToMoveBoard(fromIndex, toIndex))
            case ClickedKomaState.Hisha => (((absPlaceMoveDistance % 9 == 0 && board.upJumpCheck(fromIndex, toIndex) && board.downJumpCheck(fromIndex, toIndex)) //縦(上下)方向
              || (fromIndex / 9 == toIndex / 9 && board.rightJumpCheck(fromIndex, toIndex) && board.leftJumpCheck(fromIndex, toIndex))) //横方向
              && board.fromToMoveBoard(fromIndex, toIndex))
            case ClickedKomaState.To => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == -8 || placeMoveDistance == -10) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.NariKyo => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == -8 || placeMoveDistance == -10) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.NariKei => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == -8 || placeMoveDistance == -10) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.NariGin => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == -8 || placeMoveDistance == -10) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Uma => ((board.leftUpRightDownMove(fromIndex, toIndex) && board.leftUpJumpCheck(fromIndex, toIndex) && board.rightDownJumpCheck(fromIndex, toIndex))
              || (board.rightUpLeftDownMove(fromIndex, toIndex) && board.rightUpJumpCheck(fromIndex, toIndex) && board.leftDownJumpCheck(fromIndex, toIndex))
              || (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9)) && notCrossOnBoard(fromIndex) && board.fromToMoveBoard(fromIndex, toIndex)
            case ClickedKomaState.Ryu => ((absPlaceMoveDistance % 9 == 0 && board.upJumpCheck(fromIndex, toIndex) && board.downJumpCheck(fromIndex, toIndex)) //縦(上下)方向
              || (fromIndex / 9 == toIndex / 9 && board.rightJumpCheck(fromIndex, toIndex) && board.leftJumpCheck(fromIndex, toIndex)) //横方向
              || (absPlaceMoveDistance == 8 || absPlaceMoveDistance == 10)) && notCrossOnBoard(fromIndex) && board.fromToMoveBoard(fromIndex, toIndex)
            case _ => false
        }

        def senteKomaMove = koma match {
            case ClickedKomaState.Fu => placeMoveDistance == 9 && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Kyo => placeMoveDistance % 9 == 0 && placeMoveDistance > 0 && board.upJumpCheck(fromIndex, toIndex) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Kei => (placeMoveDistance == 17 || placeMoveDistance == 19) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Gin => (absPlaceMoveDistance == 8 || absPlaceMoveDistance == 10 || placeMoveDistance == 9) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Kin => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == 8 || placeMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Ou => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || absPlaceMoveDistance == 8 || absPlaceMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Kaku => (((board.leftUpRightDownMove(fromIndex, toIndex) && board.leftUpJumpCheck(fromIndex, toIndex) && board.rightDownJumpCheck(fromIndex, toIndex)) //左上から右下方向
              || (board.rightUpLeftDownMove(fromIndex, toIndex) && board.rightUpJumpCheck(fromIndex, toIndex) && board.leftDownJumpCheck(fromIndex, toIndex))) //右上から左下方向
              && board.fromToMoveBoard(fromIndex, toIndex))
            case ClickedKomaState.Hisha => (((absPlaceMoveDistance % 9 == 0 && board.upJumpCheck(fromIndex, toIndex) && board.downJumpCheck(fromIndex, toIndex)) //縦(上下)方向
              || (fromIndex / 9 == toIndex / 9 && board.rightJumpCheck(fromIndex, toIndex) && board.leftJumpCheck(fromIndex, toIndex))) //横方向
              && board.fromToMoveBoard(fromIndex, toIndex))
            case ClickedKomaState.To => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == 8 || placeMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.NariKyo => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == 8 || placeMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.NariKei => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == 8 || placeMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.NariGin => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == 8 || placeMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && notCrossOnBoard(fromIndex)
            case ClickedKomaState.Uma => ((board.leftUpRightDownMove(fromIndex, toIndex) && board.leftUpJumpCheck(fromIndex, toIndex) && board.rightDownJumpCheck(fromIndex, toIndex))
              || (board.rightUpLeftDownMove(fromIndex, toIndex) && board.rightUpJumpCheck(fromIndex, toIndex) && board.leftDownJumpCheck(fromIndex, toIndex))
              || (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9)) && notCrossOnBoard(fromIndex) && board.fromToMoveBoard(fromIndex, toIndex)
            case ClickedKomaState.Ryu => ((absPlaceMoveDistance % 9 == 0 && board.upJumpCheck(fromIndex, toIndex) && board.downJumpCheck(fromIndex, toIndex)) //縦(上下)方向
              || (fromIndex / 9 == toIndex / 9 && board.rightJumpCheck(fromIndex, toIndex) && board.leftJumpCheck(fromIndex, toIndex)) //横方向
              || (absPlaceMoveDistance == 8 || absPlaceMoveDistance == 10)) && notCrossOnBoard(fromIndex) && board.fromToMoveBoard(fromIndex, toIndex)
            case _ => false
        }

        val canMovePlace: Boolean = isTumasuKoma match {
          case true => {
            if (isSenteTurnState && isThereSenteKoma(fromIndex).contains(!isSenteTurnState)) goteKomaMove //先手のときは後手の駒を調べる
            else if (!isSenteTurnState && (isThereSenteKoma(fromIndex).contains(!isSenteTurnState))) senteKomaMove //後手のときは先手の駒を調べる
            else false
          }
          case false => {
            if (isSenteTurnState && isThereSenteKoma(fromIndex).contains(isSenteTurnState)) senteKomaMove //先手のときは先手の駒を調べる
            else if (!isSenteTurnState && (isThereSenteKoma(fromIndex).contains(isSenteTurnState))) goteKomaMove //後手のときは後手の駒を調べる
            else false
          }
        }
        canMovePlace
      }

      def canTakePlace(fromIndex: Int, toIndex: Int, isTumasuKoma: Boolean, board: Board): Boolean = {
        val isSenteKoma = isThereSenteKoma(fromIndex)
          isSenteKoma match {
            case Some(isSenteTurnState) => {
              isThereKomaKind(fromIndex) match {
                case Some(kind) => canMovePlace(kind, fromIndex, toIndex, isTumasuKoma, board) //詰ます側と詰まされる側のどちらの駒を調べたいか
                case None => false
              }
            }
            case _ => false
          }
      }

      val enemyOuIndex: Int = board.findKomaKind(ClickedKomaState.Ou, !isSenteTurnState) //相手の王の位置
      val nineColumnOuMove: List[Int] = List(enemyOuIndex - 9, enemyOuIndex - 8, enemyOuIndex, enemyOuIndex + 1, enemyOuIndex + 9, enemyOuIndex + 10)
      val oneColumnOuMove: List[Int] = List(enemyOuIndex - 10, enemyOuIndex - 9, enemyOuIndex - 1, enemyOuIndex, enemyOuIndex + 8, enemyOuIndex + 9)
      val normalOuMove: List[Int] = List(enemyOuIndex - 10, enemyOuIndex - 9, enemyOuIndex - 8, enemyOuIndex - 1, enemyOuIndex, enemyOuIndex + 1, enemyOuIndex + 8, enemyOuIndex + 9, enemyOuIndex + 10)

      val ouMove: List[Int] = {
        if ((enemyOuIndex % 9) + 1 == 9) nineColumnOuMove //9筋の時
        else if ((enemyOuIndex % 9) + 1 == 1) oneColumnOuMove //1筋の時
        else normalOuMove
      }
      val OucanMove: List[Int] = ouMove.filter(index => index >= 0 && index <= 80)

      for (fromIndex <- 0 to 80) {
        //王の場所にtoIndexができる駒を調べる => 王を狙う駒を取り返すパターン
        if (canTakePlace(fromIndex, enemyOuIndex, true, board)) {
          ouTookKomaStock = fromIndex :: ouTookKomaStock
        }
        //すべての駒が効きのある場所 => 逃げるパターン
        for (toIndex <- 0 to 80) {
          if (!toIndexStock.contains(toIndex)) {
            if (canTakePlace(fromIndex, toIndex, true, board)) {
              toIndexStock = toIndex :: toIndexStock
            }
          }
        }
      }

      //王を狙う駒を取り返すパターン
      if (ouTookKomaStock.length == 0) notGetBackKoma = false //王に対する駒の効きが無いなら詰みではない
      else if (ouTookKomaStock.length == 1) { //一つなら、王以外でその駒を取り返せるか、中合いが効くか調べる

        //取り返せる場合
        for (fromIndex <- 0 to 80) {
          if (canTakePlace(fromIndex, ouTookKomaStock(0), false, board) && //詰まさ無い側の駒で、相手の王を取ろうとしている駒を取れるか
            !isThereKomaKind(fromIndex).contains(ClickedKomaState.Ou)) { //王以外
              notGetBackKoma = false
          }
        }

        /** todo
          * 1. "canTyuai(index)"関数を定義(そこに自分の駒を置けるか、という関数)
          * 2. その関数がtrueになる場所だけを中合いしたい
          * BlankKomasと同様の追加方法ができないか？
          */
        //中合いが効くパターン
        val tyuAiKoma = true
        val tumiCheckKomas: List[Koma] = board match { case Board(komas) => komas }
        val tyuAiBorad = Board(
          Koma(ClickedKomaState.Fu, enemyOuIndex - 10, isSenteTurnState, tyuAiKoma) :: Koma(ClickedKomaState.Fu, enemyOuIndex - 9, isSenteTurnState, tyuAiKoma) ::
            Koma(ClickedKomaState.Fu, enemyOuIndex - 8, isSenteTurnState, tyuAiKoma) :: Koma(ClickedKomaState.Fu, enemyOuIndex - 1, isSenteTurnState, tyuAiKoma) ::
            Koma(ClickedKomaState.Fu, enemyOuIndex + 1, isSenteTurnState, tyuAiKoma) :: Koma(ClickedKomaState.Fu, enemyOuIndex + 8, isSenteTurnState, tyuAiKoma) ::
            Koma(ClickedKomaState.Fu, enemyOuIndex + 9, isSenteTurnState, tyuAiKoma) :: Koma(ClickedKomaState.Fu, enemyOuIndex + 10, isSenteTurnState, tyuAiKoma) ::
            tumiCheckKomas)
        for (fromIndex <- 0 to 80) {
          if (canTakePlace(fromIndex, enemyOuIndex, true, tyuAiBorad)) { //入らなければOK
            canNotTyuAi = fromIndex :: canNotTyuAi
          }
        }
        if (canNotTyuAi.isEmpty) canTyuAi = false //長さが1から、中合いすると0になる場合は、詰みでは無い
      }

      //逃げるパターン
      //味方の駒がいる場合、そこには逃げられない(最初からいる場合は除く)
      val OucanLive: List[Int] = (OucanMove diff toIndexStock).filter(index => isNotFriendKoma(index) || index == enemyOuIndex)
      val isCheckmateFlag = OucanLive match {
        case Nil => true
        case _ => false
      }

      //(王を狙う駒を取り返す || 逃げるパターン) <= 両方ができない(両方ともtrueの)場合は、詰みになる
      println(isCheckmateFlag, notGetBackKoma, canTyuAi)
      isCheckmateFlag && notGetBackKoma && canTyuAi
    }
    /** ここまで、王が詰んでいるかチェックする関数 */


    /** Cellの描画を定義 */
    val isFillLightBulue: Boolean = selectedCellIndex.contains(clickedIndex) && clickedKomaKind != ClickedKomaState.None

    val fillColor = if (isFillLightBulue) LightBlue
    else if (canMove(clickedKomaKind, selectedCellIndex.getOrElse(200)) && isNotFriendKoma(clickedIndex)
      && optOnBoardKomaState.contains(true)
    ) AliceBlue
    else if (toMoveBoard || handPlace) Burlywood
    else White

    val grid = {
      val rect = Rectangle(80, 80, fillColor)
      if (toMoveBoard || handPlace) rect.setStroke(Black)
      rect
    }

    val group =  new Group { children = List(Some(grid), komaOpt.map(komaObjGroup)).flatten }

    /** クリック時にどの判定を行うべきか分岐 */
    def useHandKomaBranch: Boolean = {
      isSenteTurnState match {
        case true => ((optOnBoard.contains(false) && clickedKomaKind == ClickedKomaState.None) || optOnBoardKomaState.contains(false)) &&
          (optIsSenteKoma.contains(true) || optIsSenteKomaState.contains(true)) && isSenteTurnState && !optOnBoardKomaState.contains(true) && !isCanNari && !isWin
        case false => ((optOnBoard.contains(false) && clickedKomaKind == ClickedKomaState.None) || optOnBoardKomaState == Option(false)) &&
          (optIsSenteKoma.contains(false) || optIsSenteKomaState.contains(false)) && !isSenteTurnState && !optOnBoardKomaState.contains(true) && !isCanNari && !isWin
      }
    }

    def inBoardKomaBranch(koma: ClickedKomaState): Boolean = {
      isSenteTurnState match {
        case true => ((optClickedKomaKind.contains(koma) && clickedKomaKind == ClickedKomaState.None) || clickedKomaKind == koma) &&
          (optIsSenteKoma.contains(true) || optIsSenteKomaState.contains(true)) && isSenteTurnState && !optOnBoardKomaState.contains(false) && !isCanNari && !isWin
        case false => ((optClickedKomaKind.contains(koma) && clickedKomaKind == ClickedKomaState.None) || clickedKomaKind == koma) &&
          (optIsSenteKoma.contains(false) || optIsSenteKomaState.contains(false)) && !isSenteTurnState && !optOnBoardKomaState.contains(false) && !isCanNari && !isWin
      }
    }

    def nariChoiceBranch: Boolean = {
      (optClickedKomaKind.contains(ClickedKomaState.Na) && selectedCellIndex.contains(106)) || optClickedKomaKind.contains(ClickedKomaState.Ri)
    }
    def funariChoiceBranch: Boolean = {
      (optClickedKomaKind.contains(ClickedKomaState.Na) && selectedCellIndex.contains(110)) || optClickedKomaKind.contains(ClickedKomaState.Not)
    }

    /** 初期化, 待った */
    def initializationBranch = optClickedKomaKind.contains(ClickedKomaState.A) || optClickedKomaKind.contains(ClickedKomaState.B) ||
      optClickedKomaKind.contains(ClickedKomaState.C) || optClickedKomaKind.contains(ClickedKomaState.D)
    def waitBranch = optClickedKomaKind.contains(ClickedKomaState.Ma) || optClickedKomaKind.contains(ClickedKomaState.Ltu) || optClickedKomaKind.contains(ClickedKomaState.TaHira)
    def tumiBranch: Boolean = {
      optClickedKomaKind.contains(ClickedKomaState.Tumi) && selectedCellIndex.contains(105)
    }

    /** 複数回クリックした時に、駒の情報を保存したり、条件を外したり、条件制御を行う */
    def addState = {
      clickedKomaKind = optClickedKomaKind.getOrElse(ClickedKomaState.None)
      optIsSenteKomaState = optIsSenteKoma
      optOnBoardKomaState = optOnBoard
    }
    def fromToBoradAddState(koma: ClickedKomaState) = {
      if (optClickedKomaKind.contains(koma) && clickedKomaKind == ClickedKomaState.None) addState
    }
    def fromHandToBoradAddState = {
      if (optOnBoard.contains(false) && clickedKomaKind == ClickedKomaState.None) addState
    }

    def clickCancel = {
      if (moveDistance != 0) {
        clickedKomaKind = ClickedKomaState.None
        optIsSenteKomaState = None
        optOnBoardKomaState = None
        selectedCellIndex = None
      }
    }

    /** 駒を取った時の処理と王様が取られた場合の判定 */
    def takeKoma(clickedIndex: Int) = { //駒を取った時に行う処理の集まり
      takeOuCheck(clickedIndex)
      pastBoard = board //設定の書き換えをする前に前の状態を保存しておく必要がある
      board = board.ownerChangeKoma(clickedIndex, optIsSenteKoma.contains(true)) //相手の駒が自分の駒になる
      board = board.spaceChangeKoma(clickedIndex, optOnBoard.contains(true)) //盤上の取られた駒が持ち駒になる
      board = board.returnNariKoma(clickedIndex)
      board = board.moveKoma(clickedIndex, handMove(clickedIndex)) //取られた駒の情報を書き換えて、最後に持ち駒に移動する
    }

    //駒に応じて持ち駒をどこに置くかを決める
    def handMove(clickedIndex: Int): Int = {
      val tookKomaOpt: Option[ClickedKomaState]  = board.findKoma(clickedIndex) match {
        case Some((Koma(kind, index, isSente, onBoard), i)) => Some(kind)
        case None => None
      }

      val movePlace: Int  = isSenteTurnState match {  //switchTurn(isSenteTurnState)前にコマを取っている
        case true => tookKomaOpt match {
          case Some(ClickedKomaState.Fu) => 112
          case Some(ClickedKomaState.Kyo) => 113
          case Some(ClickedKomaState.Kei) => 114
          case Some(ClickedKomaState.Gin) => 115
          case Some(ClickedKomaState.Kin) => 116
          case Some(ClickedKomaState.Kaku) => 124
          case Some(ClickedKomaState.Hisha) => 125
          case Some(ClickedKomaState.Ou) => 126
          case _ => 126
        }
        case false => tookKomaOpt match {
          case Some(ClickedKomaState.Fu) => 100
          case Some(ClickedKomaState.Kyo) => 101
          case Some(ClickedKomaState.Kei) => 102
          case Some(ClickedKomaState.Gin) => 103
          case Some(ClickedKomaState.Kin) => 104
          case Some(ClickedKomaState.Kaku) => 88
          case Some(ClickedKomaState.Hisha) => 89
          case Some(ClickedKomaState.Ou) => 90
          case _ => 90
        }
      }
      movePlace
    }

    def takeOuCheck(clickedIndex: Int) = {
      optClickedKomaKind.getOrElse(ClickedKomaState.None) match {
        case ClickedKomaState.Ou => isWin = true
        case _ =>
      }
    }

    /** 成れるかどうかの条件判定 */
    def mustNari(num: Int): Boolean = isSenteTurnState match {
      case true => {
        ((clickedKomaKind == ClickedKomaState.Fu || clickedKomaKind == ClickedKomaState.Kyo) && (clickedIndex / 9) + 1 == 1) || //先手の歩と香車が1段目
          (clickedKomaKind == ClickedKomaState.Kei && (clickedIndex / 9 + 1) <= 2) //先手の桂馬が1段目と2段目
      }
      case false => {
        ((clickedKomaKind == ClickedKomaState.Fu || clickedKomaKind == ClickedKomaState.Kyo) && (clickedIndex / 9) + 1 == 9) ||
          (clickedKomaKind == ClickedKomaState.Kei && (clickedIndex / 9 + 1) >= 8)
      }
    }

    def canNari(num: Int): Boolean = isSenteTurnState match {
      case true => {
        ((clickedIndex / 9) + 1 <= 3 || (num / 9) + 1 <= 3) && isNariKoma && fromOnBoard(num)
      } //成れる駒が、3段目以内にいた、もしくは3段目以内に入った
      case false => {
        (((clickedIndex / 9) + 1 >= 7) || (num / 9) + 1 >= 7) && isNariKoma && fromOnBoard(num)
      }
    }

    def isNariKoma: Boolean = {
      clickedKomaKind == ClickedKomaState.Fu || clickedKomaKind == ClickedKomaState.Kyo ||
        clickedKomaKind == ClickedKomaState.Kei || clickedKomaKind == ClickedKomaState.Gin ||
        clickedKomaKind == ClickedKomaState.Hisha || clickedKomaKind == ClickedKomaState.Kaku
    }

    /** 成り駒フラグの処理 */
    def addNariGomaState = {
      isCanNari = true //成れる場合は成り不成選択のフラグを立てる
      stockNariIndex = clickedIndex
    }
    def initializeNariGomaState = {
      isCanNari = false
      isSenteTurnState = switchTurn(isSenteTurnState)
      stockNariIndex = -1
    }

    /** 実際に手を指し、今までの条件を初期化する */
    def playAndInitialize(num: Int) = {

      //takeKomaでを保存していない場合に, moveKomaする前に保存しておく
      if ((isSenteTurnState && !optIsSenteKoma.contains(false)) || (!isSenteTurnState && !optIsSenteKoma.contains(true))) {
        pastBoard = board
      }

      if (mustNari(num)) {
        board = board.nariKoma(num) //強制的に成り、相手の手番へ
        isSenteTurnState = switchTurn(isSenteTurnState)
      }
      else if (canNari(num)) {  //どこにいる駒が成れる状態、という状態を付与
        addNariGomaState
        if (isWin) isSenteTurnState = switchTurn(isSenteTurnState)
      }
      else isSenteTurnState = switchTurn(isSenteTurnState) //成れない場合は相手の手番へ

      board = board.moveKoma(num, clickedIndex)
      if (optOnBoardKomaState.contains(false)){
        board = board.spaceChangeKoma(clickedIndex, optOnBoard.contains(false)) //打ち終わった駒は盤上の駒になる
      }

      selectedCellIndex = None
      optIsSenteKomaState = None
      clickedKomaKind = ClickedKomaState.None
      optOnBoardKomaState = None
    }

    /** 盤上の駒を動かす時のtakeKomaとplayAndInitializeの条件分岐処理をまとめた */
    def takeKomaAndplayAndInitialize(num: Int) = {
      isSenteTurnState match {
        case true => {
          if (optIsSenteKoma.contains(false)) takeKoma(clickedIndex)
          if (!optIsSenteKoma.contains(true)) playAndInitialize(num)
          else clickCancel
        }
        case false => {
          if (optIsSenteKoma.contains(true)) takeKoma(clickedIndex)
          if (!optIsSenteKoma.contains(false)) playAndInitialize(num)
          else clickCancel
        }
      }
    }

    /** 手持ちの駒を盤面に打てるかどうかの判定 */
    def canSetFromHand: Boolean = {
      if (isSenteTurnState) {
        toMoveBoard && optIsSenteKoma.isEmpty &&
          (!((clickedKomaKind == ClickedKomaState.Fu || clickedKomaKind == ClickedKomaState.Kyo) && (clickedIndex / 9) + 1 == 1)) && //先手の歩と香車は、1段目に打てない
          !(clickedKomaKind == ClickedKomaState.Kei && (clickedIndex / 9 + 1) <= 2) && //先手の桂馬は、1段目と2段目に打てない
          (clickedKomaKind != ClickedKomaState.Fu || board.nifuCheck(clickedIndex, optIsSenteKomaState.contains(true)))
      } else { toMoveBoard && optIsSenteKoma.isEmpty &&
        (!((clickedKomaKind == ClickedKomaState.Fu || clickedKomaKind == ClickedKomaState.Kyo) && (clickedIndex / 9) + 1 == 9)) &&
        !(clickedKomaKind == ClickedKomaState.Kei && (clickedIndex / 9 + 1) >= 8) &&
        (clickedKomaKind != ClickedKomaState.Fu || board.nifuCheck(clickedIndex, optIsSenteKomaState.contains(true)))
      }
    }


    /** 手持ちの駒を盤面に打つ時に行う処理 */
    def useHandKomaFlow(num: Int) = {
      fromHandToBoradAddState
      if (toMoveBoard && optIsSenteKoma.isEmpty &&
        !(clickedKomaKind != ClickedKomaState.Fu || board.nifuCheck(clickedIndex, optIsSenteKomaState.contains(true)))) isNifu = true
      if (canSetFromHand) playAndInitialize(num)
      else clickCancel
    }

    /** 盤上から盤上へ移動する駒が行う処理 */
    def inBordKomaMoveFlow(koma: ClickedKomaState, num: Int) = {
      fromToBoradAddState(koma)
      if (canMove(koma, num)) takeKomaAndplayAndInitialize(num)
      else clickCancel
    }

    def tumiCheckFlow = {
      if (isCheckmateCheck) isCheckmate = Some(true)
      else isCheckmate = Some(false)
    }

    def initializationOrWaitFlow = {
      if (initializationBranch) {
        optClickedKomaKind match { //初期化時の駒配置の選択が可能
          case Some(ClickedKomaState.A) => board = initalBoard
          case Some(ClickedKomaState.B) => board = newUnderFuInitalBoard
          case Some(ClickedKomaState.C) => board = halfRandomBoard
          case Some(ClickedKomaState.D) => board = allRandomBoard
          case _ =>
        }
        isSenteHandOver = false //初期化した場合の超過フラグは削除
        isGoteHandOver = false //初期化した場合の超過フラグは削除
        pastBoard = board //待ったはなし
        isSenteTurnState = true
      }
      else if (waitBranch) {
        board = pastBoard
        isSenteTurnState = !isSenteTurnState
      }
      clickCancel
      isCanNari = false
      isWin = false
      isNifu = false
      stockNariIndex = -1
    }

    /** ここまで駒をクリックした時に使われる関数群 */

    group.setOnMouseClicked(e => {
      selectedCellIndex match {
        case Some(num) => {

          /** 初期化、待ったをクリックした場合の処理 */
          if (initializationBranch) initializationOrWaitFlow
          else if (waitBranch) initializationOrWaitFlow
          else if (tumiBranch) tumiCheckFlow

          /** 駒が成るかどうかの判定をクリックした場合の処理 */
          else if (nariChoiceBranch) {
            board = board.nariKoma(stockNariIndex)
            initializeNariGomaState //状態を元に戻す
          } else if (funariChoiceBranch) initializeNariGomaState

          /** 持ち駒をクリックして盤面に打つ場合の処理 */
          else if (useHandKomaBranch) useHandKomaFlow(num)
          /** 盤面の歩を移動させる場合の処理 */
          else if (inBoardKomaBranch(ClickedKomaState.Fu)) {
            inBordKomaMoveFlow(ClickedKomaState.Fu, num)
          }
          /** 香車の場合 */
          else if (inBoardKomaBranch(ClickedKomaState.Kyo)) inBordKomaMoveFlow(ClickedKomaState.Kyo, num)
          /** 桂馬の場合 */
          else if (inBoardKomaBranch(ClickedKomaState.Kei)) inBordKomaMoveFlow(ClickedKomaState.Kei, num)
          /** 銀の場合 */
          else if (inBoardKomaBranch(ClickedKomaState.Gin)) inBordKomaMoveFlow(ClickedKomaState.Gin, num)
          /** 金の場合 */
          else if (inBoardKomaBranch(ClickedKomaState.Kin)) inBordKomaMoveFlow(ClickedKomaState.Kin, num)
          /** 王の場合 */
          else if (inBoardKomaBranch(ClickedKomaState.Ou)) inBordKomaMoveFlow(ClickedKomaState.Ou, num)
          /** 角の場合 */
          else if (inBoardKomaBranch(ClickedKomaState.Kaku)) inBordKomaMoveFlow(ClickedKomaState.Kaku, num)
          /** 飛車の場合 */
          else if (inBoardKomaBranch(ClickedKomaState.Hisha)) inBordKomaMoveFlow(ClickedKomaState.Hisha, num)
          /** との場合 */
          else if (inBoardKomaBranch(ClickedKomaState.To)) inBordKomaMoveFlow(ClickedKomaState.To, num)
          /** 成香の場合 */
          else if (inBoardKomaBranch(ClickedKomaState.NariKyo)) inBordKomaMoveFlow(ClickedKomaState.NariKyo, num)
          /** 成桂の場合 */
          else if (inBoardKomaBranch(ClickedKomaState.NariKei)) inBordKomaMoveFlow(ClickedKomaState.NariKei, num)
          /** 成銀の場合 */
          else if (inBoardKomaBranch(ClickedKomaState.NariGin)) inBordKomaMoveFlow(ClickedKomaState.NariGin, num)
          /** 馬の場合 */
          else if (inBoardKomaBranch(ClickedKomaState.Uma)) inBordKomaMoveFlow(ClickedKomaState.Uma, num)
          /** 龍の場合 */
          else if (inBoardKomaBranch(ClickedKomaState.Ryu)) inBordKomaMoveFlow(ClickedKomaState.Ryu, num)

          /* デバッグ用
          println("existSelectedCellIndex:" + existSelectedCellIndex, "selectedCellIndex:" + selectedCellIndex, "stockNariIndex:" + stockNariIndex)
          println("optOnBoard:" + optOnBoard, "optOnBoardKomaState:" + optOnBoardKomaState)
          println("optIsSenteKoma:" + optIsSenteKoma, "optIsSenteKomaState:" + optIsSenteKomaState)
          println("optClickedKomaKind:" + optClickedKomaKind, "clickedKomaKind:" + clickedKomaKind)
          println("isSenteTurnState:" + isSenteTurnState)
          println("num:" + num,"clickedIndex:"+clickedIndex,"selectedCellIndex:"+selectedCellIndex)
          println("")
          */
        }

        case None => selectedCellIndex = Some(clickedIndex)
      }
      boardSwitch
      isNifu = false
      initializeTumiState
      repaint
    })
    group
  }

  def komaObjGroup(koma: Koma): Group = {
    val senteKomaShape = { //駒の形を定義している
    val poly = koma.isSente match {
        case true => Polygon(40, 10, 60, 20, 70, 70, 10, 70, 20, 20)
        case false => Polygon(40, 70, 20, 60, 10, 10, 70, 10, 60, 60)
      }
      poly.setFill(Sienna)
      poly.setStroke(Black)
      poly
    }

    val komaLabel = { //升ないの駒の置き場所を定義してる
    val label = new Label
      label.setText(koma.kind.name)
      if (!koma.isSente && !(koma.index >= 81 && koma.onBoard)) label.setRotate(180)
      label.setFont(Font(30))
      label.setMaxSize(30, 30)
      label.setLayoutX(25)
      label.setLayoutY(25)
      label.setAlignment(Pos.Center)
      label
    }
    val obj = new Group(senteKomaShape, komaLabel) //駒の形、置き場所のセット
    obj
  }

  def allRandomBoard: Board = { //D
    def randomRow: List[Int] = scala.util.Random.shuffle((1 to 7).toList)
    var stockFu: List[Int] = List()

    for (i <- 0 to 8) { //0筋〜8筋
      val twoRandomRow = randomRow
      val senteFuIndex = 9 * twoRandomRow(0) + i
      val goteFuIndex = 9 * twoRandomRow(1) + i
      stockFu = senteFuIndex :: goteFuIndex :: stockFu
    }

    val possibleSenteKeiKyoIndex = scala.util.Random.shuffle((18 to 80).toList diff stockFu)
    val senteKeiKyoIndex = List(possibleSenteKeiKyoIndex(0),possibleSenteKeiKyoIndex(1),possibleSenteKeiKyoIndex(2),possibleSenteKeiKyoIndex(3)) //香車もあえてここに入れとく

    val possibleGoteKeiKyoIndex = scala.util.Random.shuffle((0 to 62).toList diff stockFu diff senteKeiKyoIndex)
    val goteKeiKyoIndex = List(possibleGoteKeiKyoIndex(0),possibleGoteKeiKyoIndex(1),possibleGoteKeiKyoIndex(2),possibleGoteKeiKyoIndex(3))

    val possibleKomaIndex = scala.util.Random.shuffle((0 to 80).toList diff stockFu diff senteKeiKyoIndex diff goteKeiKyoIndex) //残りの駒はここから取る

    val allRandomBoard: Board = Board(List(
      Koma(ClickedKomaState.Fu, stockFu(0), false, true), Koma(ClickedKomaState.Fu, stockFu(2), false, true), Koma(ClickedKomaState.Fu, stockFu(4), false, true),
      Koma(ClickedKomaState.Fu, stockFu(6), false, true), Koma(ClickedKomaState.Fu, stockFu(8), false, true), Koma(ClickedKomaState.Fu, stockFu(10), false, true),
      Koma(ClickedKomaState.Fu, stockFu(12), false, true), Koma(ClickedKomaState.Fu, stockFu(14), false, true), Koma(ClickedKomaState.Fu, stockFu(16), false, true),

      Koma(ClickedKomaState.Kyo, goteKeiKyoIndex(0), false, true), Koma(ClickedKomaState.Kei, goteKeiKyoIndex(1), false, true),
      Koma(ClickedKomaState.Gin, possibleKomaIndex(0), false, true), Koma(ClickedKomaState.Kin, possibleKomaIndex(1), false, true), Koma(ClickedKomaState.Ou, possibleKomaIndex(2), false, true),
      Koma(ClickedKomaState.Kyo, goteKeiKyoIndex(2), false, true), Koma(ClickedKomaState.Kei, goteKeiKyoIndex(3), false, true), Koma(ClickedKomaState.Gin, possibleKomaIndex(4), false, true), Koma(ClickedKomaState.Kin, possibleKomaIndex(3), false, true),
      Koma(ClickedKomaState.Hisha, possibleKomaIndex(5), false, true), Koma(ClickedKomaState.Kaku, possibleKomaIndex(6), false, true),

      Koma(ClickedKomaState.Hisha, possibleKomaIndex(8), true, true), Koma(ClickedKomaState.Kaku, possibleKomaIndex(9), true, true),
      Koma(ClickedKomaState.Kyo, senteKeiKyoIndex(0), true, true), Koma(ClickedKomaState.Kei, senteKeiKyoIndex(1), true, true),
      Koma(ClickedKomaState.Gin, possibleKomaIndex(10), true, true), Koma(ClickedKomaState.Kin, possibleKomaIndex(11), true, true), Koma(ClickedKomaState.Ou, possibleKomaIndex(12), true, true),
      Koma(ClickedKomaState.Kyo, senteKeiKyoIndex(2), true, true), Koma(ClickedKomaState.Kei, senteKeiKyoIndex(3), true, true), Koma(ClickedKomaState.Gin, possibleKomaIndex(7), true, true), Koma(ClickedKomaState.Kin, possibleKomaIndex(13), true, true),

      Koma(ClickedKomaState.Fu, stockFu(1), true, true), Koma(ClickedKomaState.Fu, stockFu(3), true, true), Koma(ClickedKomaState.Fu, stockFu(5), true, true),
      Koma(ClickedKomaState.Fu, stockFu(7), true, true), Koma(ClickedKomaState.Fu, stockFu(9), true, true), Koma(ClickedKomaState.Fu, stockFu(11), true, true),
      Koma(ClickedKomaState.Fu, stockFu(13), true, true), Koma(ClickedKomaState.Fu, stockFu(15), true, true), Koma(ClickedKomaState.Fu, stockFu(17), true, true)
    ))
    allRandomBoard
  }

  def halfRandomBoard: Board = { //C
    val (senteZone, goteZone) = ((0 to 35).toList, (45 to 80).toList)
    val r = new Random

    val senteFuPlace = List(r.nextInt(3) * 9 + 0, r.nextInt(3) * 9 + 1, r.nextInt(3) * 9 + 2, r.nextInt(3) * 9 + 3, r.nextInt(3) * 9 + 4,
      r.nextInt(3) * 9 + 5, r.nextInt(3) * 9 + 6, r.nextInt(3) * 9 + 7, r.nextInt(3) * 9 + 8)
    val senteWithoutFuPlace: List[Int] = scala.util.Random.shuffle(senteZone diff senteFuPlace)

    val goteFuPlace = List( (r.nextInt(3) + 6) * 9 + 0, (r.nextInt(3) + 6) * 9 + 1, (r.nextInt(3) + 6) * 9 + 2, (r.nextInt(3) + 6) * 9 + 3, (r.nextInt(3) + 6) * 9 + 4,
        (r.nextInt(3) + 6) * 9 + 5, (r.nextInt(3) + 6) * 9 + 6, (r.nextInt(3) + 6) * 9 + 7, (r.nextInt(3) + 6) * 9 + 8)
    val goteWithoutFuPlace: List[Int] = scala.util.Random.shuffle(goteZone diff goteFuPlace)

    val newInitalBoard: Board = Board(List(
      Koma(ClickedKomaState.Fu, senteFuPlace(0), false, true), Koma(ClickedKomaState.Fu, senteFuPlace(1), false, true), Koma(ClickedKomaState.Fu, senteFuPlace(2), false, true),
      Koma(ClickedKomaState.Fu, senteFuPlace(3), false, true), Koma(ClickedKomaState.Fu, senteFuPlace(4), false, true), Koma(ClickedKomaState.Fu, senteFuPlace(5), false, true),
      Koma(ClickedKomaState.Fu, senteFuPlace(6), false, true), Koma(ClickedKomaState.Fu, senteFuPlace(7), false, true), Koma(ClickedKomaState.Fu, senteFuPlace(8), false, true),
      Koma(ClickedKomaState.Kyo, senteWithoutFuPlace(0), false, true), Koma(ClickedKomaState.Kei, senteWithoutFuPlace(1), false, true),
      Koma(ClickedKomaState.Gin, senteWithoutFuPlace(2), false, true), Koma(ClickedKomaState.Kin, senteWithoutFuPlace(3), false, true), Koma(ClickedKomaState.Ou, senteWithoutFuPlace(4), false, true),
      Koma(ClickedKomaState.Kyo, senteWithoutFuPlace(5), false, true), Koma(ClickedKomaState.Kei, senteWithoutFuPlace(6), false, true), Koma(ClickedKomaState.Gin, senteWithoutFuPlace(7), false, true), Koma(ClickedKomaState.Kin, senteWithoutFuPlace(8), false, true),
      Koma(ClickedKomaState.Hisha, senteWithoutFuPlace(9), false, true), Koma(ClickedKomaState.Kaku, senteWithoutFuPlace(10), false, true),

      Koma(ClickedKomaState.Hisha, goteWithoutFuPlace(0), true, true), Koma(ClickedKomaState.Kaku, goteWithoutFuPlace(1), true, true),
      Koma(ClickedKomaState.Kyo, goteWithoutFuPlace(2), true, true), Koma(ClickedKomaState.Kei, goteWithoutFuPlace(3), true, true),
      Koma(ClickedKomaState.Gin, goteWithoutFuPlace(4), true, true), Koma(ClickedKomaState.Kin, goteWithoutFuPlace(5), true, true), Koma(ClickedKomaState.Ou, goteWithoutFuPlace(6), true, true),
      Koma(ClickedKomaState.Kyo, goteWithoutFuPlace(7), true, true), Koma(ClickedKomaState.Kei, goteWithoutFuPlace(8), true, true), Koma(ClickedKomaState.Gin, goteWithoutFuPlace(9), true, true), Koma(ClickedKomaState.Kin, goteWithoutFuPlace(10), true, true),
      Koma(ClickedKomaState.Fu, goteFuPlace(0), true, true), Koma(ClickedKomaState.Fu, goteFuPlace(1), true, true), Koma(ClickedKomaState.Fu, goteFuPlace(2), true, true),
      Koma(ClickedKomaState.Fu, goteFuPlace(3), true, true), Koma(ClickedKomaState.Fu, goteFuPlace(4), true, true), Koma(ClickedKomaState.Fu, goteFuPlace(5), true, true),
      Koma(ClickedKomaState.Fu, goteFuPlace(6), true, true), Koma(ClickedKomaState.Fu, goteFuPlace(7), true, true), Koma(ClickedKomaState.Fu, goteFuPlace(8), true, true)
    ))
    newInitalBoard
  }
  def newUnderFuInitalBoard: Board = { //B
    val (senteZone, goteZone) = ((0 to 17).toList, (63 to 80).toList)
    val (senteKomaPlace: List[Int], goteKomaPlace: List[Int]) = (scala.util.Random.shuffle(senteZone), scala.util.Random.shuffle(goteZone))
    val newInitalBoard: Board = Board(List( //歩の下でランダムな初期の駒配置
      Koma(ClickedKomaState.Fu, 18, false, true), Koma(ClickedKomaState.Fu, 19, false, true), Koma(ClickedKomaState.Fu, 20, false, true),
      Koma(ClickedKomaState.Fu, 21, false, true), Koma(ClickedKomaState.Fu, 22, false, true), Koma(ClickedKomaState.Fu, 23, false, true),
      Koma(ClickedKomaState.Fu, 24, false, true), Koma(ClickedKomaState.Fu, 25, false, true), Koma(ClickedKomaState.Fu, 26, false, true),
      Koma(ClickedKomaState.Kyo, senteKomaPlace(0), false, true), Koma(ClickedKomaState.Kei, senteKomaPlace(1), false, true),
      Koma(ClickedKomaState.Gin, senteKomaPlace(2), false, true), Koma(ClickedKomaState.Kin, senteKomaPlace(3), false, true), Koma(ClickedKomaState.Ou, senteKomaPlace(4), false, true),
      Koma(ClickedKomaState.Kyo, senteKomaPlace(5), false, true), Koma(ClickedKomaState.Kei, senteKomaPlace(6), false, true), Koma(ClickedKomaState.Gin, senteKomaPlace(7), false, true), Koma(ClickedKomaState.Kin, senteKomaPlace(8), false, true),
      Koma(ClickedKomaState.Hisha, senteKomaPlace(9), false, true), Koma(ClickedKomaState.Kaku, senteKomaPlace(10), false, true),
      Koma(ClickedKomaState.Hisha, goteKomaPlace(9), true, true), Koma(ClickedKomaState.Kaku, goteKomaPlace(10), true, true),
      Koma(ClickedKomaState.Kyo, goteKomaPlace(0), true, true), Koma(ClickedKomaState.Kei, goteKomaPlace(1), true, true),
      Koma(ClickedKomaState.Gin, goteKomaPlace(2), true, true), Koma(ClickedKomaState.Kin, goteKomaPlace(3), true, true), Koma(ClickedKomaState.Ou, goteKomaPlace(4), true, true),
      Koma(ClickedKomaState.Kyo, goteKomaPlace(5), true, true), Koma(ClickedKomaState.Kei, goteKomaPlace(6), true, true), Koma(ClickedKomaState.Gin, goteKomaPlace(7), true, true), Koma(ClickedKomaState.Kin, goteKomaPlace(8), true, true),
      Koma(ClickedKomaState.Fu, 62, true, true), Koma(ClickedKomaState.Fu, 61, true, true), Koma(ClickedKomaState.Fu, 60, true, true),
      Koma(ClickedKomaState.Fu, 59, true, true), Koma(ClickedKomaState.Fu, 58, true, true), Koma(ClickedKomaState.Fu, 57, true, true),
      Koma(ClickedKomaState.Fu, 56, true, true), Koma(ClickedKomaState.Fu, 55, true, true), Koma(ClickedKomaState.Fu, 54, true, true)
    ))
    newInitalBoard
  }
  def initalBoard: Board = Board(List( //A
    Koma(ClickedKomaState.Fu, 18, false, true), Koma(ClickedKomaState.Fu, 19, false, true), Koma(ClickedKomaState.Fu, 20, false, true),
    Koma(ClickedKomaState.Fu, 21, false, true), Koma(ClickedKomaState.Fu, 22, false, true), Koma(ClickedKomaState.Fu, 23, false, true),
    Koma(ClickedKomaState.Fu, 24, false, true), Koma(ClickedKomaState.Fu, 25, false, true), Koma(ClickedKomaState.Fu, 26, false, true),
    Koma(ClickedKomaState.Kyo, 0, false, true), Koma(ClickedKomaState.Kei, 1, false, true),
    Koma(ClickedKomaState.Gin, 2, false, true), Koma(ClickedKomaState.Kin, 3, false, true), Koma(ClickedKomaState.Ou, 4, false, true),
    Koma(ClickedKomaState.Kyo, 8, false, true), Koma(ClickedKomaState.Kei, 7, false, true), Koma(ClickedKomaState.Gin, 6, false, true), Koma(ClickedKomaState.Kin, 5, false, true),
    Koma(ClickedKomaState.Hisha, 10, false, true), Koma(ClickedKomaState.Kaku, 16, false, true), Koma(ClickedKomaState.Hisha, 70, true, true), Koma(ClickedKomaState.Kaku, 64, true, true),
    Koma(ClickedKomaState.Kyo, 80, true, true), Koma(ClickedKomaState.Kei, 79, true, true),
    Koma(ClickedKomaState.Gin, 78, true, true), Koma(ClickedKomaState.Kin, 77, true, true), Koma(ClickedKomaState.Ou, 76, true, true),
    Koma(ClickedKomaState.Kyo, 72, true, true), Koma(ClickedKomaState.Kei, 73, true, true), Koma(ClickedKomaState.Gin, 74, true, true), Koma(ClickedKomaState.Kin, 75, true, true),
    Koma(ClickedKomaState.Fu, 62, true, true), Koma(ClickedKomaState.Fu, 61, true, true), Koma(ClickedKomaState.Fu, 60, true, true),
    Koma(ClickedKomaState.Fu, 59, true, true), Koma(ClickedKomaState.Fu, 58, true, true), Koma(ClickedKomaState.Fu, 57, true, true),
    Koma(ClickedKomaState.Fu, 56, true, true), Koma(ClickedKomaState.Fu, 55, true, true), Koma(ClickedKomaState.Fu, 54, true, true)
  ))

  //仮装条件での検証用のBoard
  def testBoard: Board = Board(List(
    Koma(ClickedKomaState.Ou, 4, false, true), Koma(ClickedKomaState.Ou, 2, true, true), //Koma(ClickedKomaState.Gin, 5, false, true),
    Koma(ClickedKomaState.Kin, 6, true, true),
    Koma(ClickedKomaState.Kin, 20, true, true), Koma(ClickedKomaState.Kyo, 31, true, true), Koma(ClickedKomaState.Kin, 24, true, true)
  ))

}
