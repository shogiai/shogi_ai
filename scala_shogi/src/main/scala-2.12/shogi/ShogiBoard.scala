package shogi

import java.io._
import scala.collection.immutable.::
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

  val initalKomas: List[Koma] = testBoard match { case Board(komas) => komas }
  val initalChoiceKoma = true

  var board: Board = Board(
    Koma(ClickedKomaState.A, 81, true, initalChoiceKoma) :: Koma(ClickedKomaState.B, 87, true, initalChoiceKoma) ::
      Koma(ClickedKomaState.C, 93, true, initalChoiceKoma) :: Koma(ClickedKomaState.D, 99, true, initalChoiceKoma) :: //初期化
      initalKomas)
  var pastBoard: Board = board

  /** 将棋盤のテンプレートの切り替え */

  /** StateとFlagを定義 */
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

  var isSenteTurnState: Boolean = true
  var (isWin, isCanNari, isNifu) = (false, false, false)
  var isCheckmate: Option[Boolean] = None
  var isOuCatch: Option[Boolean] = None
  var enemyOuTakeKomaStock: List[Int] = Nil

  def boardSwitch :Board = {
    /** 初期化と待ったのボタン更新 */
    val transitionKoma: Boolean = true
    val onBoardKomas: List[Koma] = board match { case Board(komas) => komas.takeRight(40) }
    val pastKomas: List[Koma] = pastBoard match { case Board(komas) => komas.takeRight(40) }

    board = if (onBoardKomas != pastKomas && !isCanNari) { //待ったを出していいとき
      val addBoard: Board = Board(
        Koma(ClickedKomaState.A, 81, true, transitionKoma) :: Koma(ClickedKomaState.B, 87, true, transitionKoma) ::
          Koma(ClickedKomaState.C, 93, true, transitionKoma) :: Koma(ClickedKomaState.D, 99, true, transitionKoma) :: //初期化
          Koma(ClickedKomaState.Ma, 117, true, transitionKoma) :: Koma(ClickedKomaState.Ltu, 123, true, transitionKoma) :: Koma(ClickedKomaState.TaHira, 129, true, transitionKoma) :: //待った
          onBoardKomas)
      addBoard
    }
    else { //待ったを出してはいけないとき
      val addInitializeBoard: Board = Board(
        Koma(ClickedKomaState.A, 81, true, transitionKoma) :: Koma(ClickedKomaState.B, 87, true, transitionKoma) ::
          Koma(ClickedKomaState.C, 93, true, transitionKoma) :: Koma(ClickedKomaState.D, 99, true, transitionKoma) :: //初期化
          onBoardKomas)
      addInitializeBoard
    }

    /** その他テンプレートの更新 */
    val realKomas: List[Koma] = board match { case Board(komas) => komas }
    val displayKoma: Boolean = true
    board = (isCanNari, isNifu, isSenteTurnState) match { //1手指すと出てくる
      case (false, false, true) => {
        val normalBoard: Board = Board( //先手番
          Koma(ClickedKomaState.Sen, 107, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 108, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ban, 109, isSenteTurnState, displayKoma) ::
            realKomas)
        normalBoard
      }
      case (false, false, false) => {
        val normalBoard: Board = Board( //後手番
          Koma(ClickedKomaState.Go, 107, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 108, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ban, 109, isSenteTurnState, displayKoma) ::
            realKomas)
        normalBoard
      }
      case (true, _, true) => {
        val SenteNariFunariBoard: Board = Board( //成りor不成
          Koma(ClickedKomaState.Na, 106, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ri, 107, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Slash, 108, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Not, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Na, 110, isSenteTurnState, displayKoma) ::
            realKomas)
        SenteNariFunariBoard
      }
      case (true, _, false) => {
        val GoteNariFunariboard: Board = Board( //成りor不成
          Koma(ClickedKomaState.Na, 106, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ri, 107, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Slash, 108, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Not, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Na, 110, isSenteTurnState, displayKoma) :: realKomas)
        GoteNariFunariboard
      }
      case (_, true, true) => {
        val SenteNifuBoard: Board = Board( //二歩です
          Koma(ClickedKomaState.Ni, 107, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Fu, 108, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.De, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Su, 110, isSenteTurnState, displayKoma) ::
            realKomas)
        SenteNifuBoard
      }
      case (_, true, false) => {
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

    //todo 王手です、も表示する
    board = (isCheckmate, isOuCatch) match {
      case (_, Some(true)) => isSenteTurnState match { //isOuCatch
        case true => { //先手の手番 => 先手の勝ち
        val senteOuCatchBoard: Board = Board(
            Koma(ClickedKomaState.Sen, 106, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 107, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.No, 108, isSenteTurnState, displayKoma) ::
              Koma(ClickedKomaState.Ka, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 110, isSenteTurnState, displayKoma) :: //先手の勝ち
              Koma(ClickedKomaState.Ka, 127, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 128, isSenteTurnState, displayKoma) ::
              Koma(ClickedKomaState.Ahira, 133, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ri, 134, isSenteTurnState, displayKoma) ::
              secondKomas)
          senteOuCatchBoard
        }
        case false => { //後手の手番 => 後手の勝ち
        val goteOuCatchBoard: Board = Board(
            Koma(ClickedKomaState.Go, 106, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 107, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.No, 108, isSenteTurnState, displayKoma) ::
              Koma(ClickedKomaState.Ka, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 110, isSenteTurnState, displayKoma) :: //後手の勝ち
              Koma(ClickedKomaState.Ka, 85, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 86, isSenteTurnState, displayKoma) ::
              Koma(ClickedKomaState.Ahira, 91, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ri, 92, isSenteTurnState, displayKoma) ::
              secondKomas)
          goteOuCatchBoard
        }
      }
      case (Some(true), _) => isSenteTurnState match { //isCheckmate
        case true => { //先手の手番 => 後手の勝ち
          val senteTumiBoard: Board = Board(
            Koma(ClickedKomaState.Go, 106, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 107, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.No, 108, !isSenteTurnState, displayKoma) ::
              Koma(ClickedKomaState.Ka, 109, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 110, !isSenteTurnState, displayKoma) :: //後手の勝ち
            Koma(ClickedKomaState.Sen, 127, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 128, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Tumi, 133, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Mi, 134, isSenteTurnState, displayKoma) ::
              secondKomas)
          senteTumiBoard
        }
        case false => { //後手の手番 => 先手の勝ち
          val goteTumiBoard: Board = Board(
            Koma(ClickedKomaState.Sen, 106, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 107, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.No, 108, !isSenteTurnState, displayKoma) ::
              Koma(ClickedKomaState.Ka, 109, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 110, !isSenteTurnState, displayKoma) :: //先手の勝ち
            Koma(ClickedKomaState.Go, 85, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 86, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Tumi, 91, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Mi, 92, isSenteTurnState, displayKoma) ::
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
    board = Board(outputKomas)

    board
  }

  /** 棋譜の出力 */
  var kifu: List[String] = List("まで","手で","勝ち")
  val LOG_FILE_PATH = "kifu.txt" //ログ出力先
  def logOutPut {
    val tesu = ((kifu.length + 1)/3 - 1).toString
    val winPlayer: String = enemyOuTakeKomaStock.nonEmpty match {
      case true => {
        (kifu.length + 1) % 2 match {
          case 1 => "後手"
          case 0 => "先手"
          case _ => "??"
        }
      }
      case false => {
        (kifu.length + 1) % 2 match {
          case 1 => "先手"
          case 0 => "後手"
          case _ => "??"
        }
      }
    }

    var outPutKifu: List[String] = kifu.takeRight(3)
    outPutKifu = outPutKifu match {
      case List(a,b,c) => List(a + tesu + b + winPlayer + c)
      case  _ => List("")
    }

    kifu = kifu.dropRight(3)
    while(kifu.length >= 3) {
      val itteList = kifu.take(3)
      val itte: String = itteList match {
        case List(yoko:String,tate:String,koma:String) => yoko+tate+koma
        case _ => ""
      }

      outPutKifu = itte :: outPutKifu
      kifu = kifu.drop(3)
    }

    val in = new File(LOG_FILE_PATH)
    val out = new PrintWriter(new FileWriter(LOG_FILE_PATH, true))
    outPutKifu.foreach(itte => out.print(itte+" "))
    out.println("")
    out.close
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

  /** cellObjGroup内で使われるStateを定義 */
  //todo 変数をOption型にする
  var selectedCellIndex: Int = -100
  var stockNariIndex: Int = -1
  var optIsSenteKomaState: Option[Boolean] = None
  var optOnBoardKomaState: Option[Boolean] = None

  def cellObjGroup(komaOpt: Option[Koma], clickedIndex: Int): Group = {

    /** 以下、駒をクリックした時に使う関数のまとまり */
    /* clickedIndexと一致した場合の駒情報を取得する (komaの中のClickedIndexと一致する、という条件が必要(本当は)) */
    val optClickedKomaKind: Option[ClickedKomaState] = komaOpt.map(koma => koma.kind)
    val optIsSenteKoma: Option[Boolean] = komaOpt.map(koma => koma.isSente)
    val optOnBoard: Option[Boolean] = komaOpt.map(koma => koma.onBoard)

    val absMoveDistance = Math.abs(selectedCellIndex - clickedIndex) //駒の移動距離の絶対値を定義
    val moveDistance = selectedCellIndex - clickedIndex //駒の移動距離を定義

    def toMoveBoard: Boolean = clickedIndex <= 80
    def handPlace: Boolean = (clickedIndex >= 81 && clickedIndex <= 134) && (clickedIndex - 81) % 6 != 0 && (clickedIndex-81) / 6 != 4

    def fromOnBoard: Boolean = selectedCellIndex <= 80
    def fromOutOfBoard: Boolean = selectedCellIndex >= 81 && selectedCellIndex <= 134
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
    def canMove(koma: ClickedKomaState): Boolean = {
      val canMove: Boolean = {
        if (isSenteTurnState) {
          koma match {
            case ClickedKomaState.Fu => moveDistance == 9 && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Kyo => moveDistance % 9 == 0 && moveDistance > 0 && board.upJumpCheck(selectedCellIndex, clickedIndex) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Kei => (moveDistance == 17 || moveDistance == 19) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Gin => (absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == 9) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Kin => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Ou => (absMoveDistance == 1 || absMoveDistance == 9 || absMoveDistance == 8 || absMoveDistance == 10) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Kaku => (((board.leftUpRightDownMove(selectedCellIndex, clickedIndex)&& board.leftUpJumpCheck(selectedCellIndex, clickedIndex) && board.rightDownJumpCheck(selectedCellIndex, clickedIndex)) //左上から右下方向
                || (board.rightUpLeftDownMove(selectedCellIndex, clickedIndex) && board.rightUpJumpCheck(selectedCellIndex, clickedIndex) && board.leftDownJumpCheck(selectedCellIndex, clickedIndex))) //右上から左下方向
                && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && moveDistance != 0)
            case ClickedKomaState.Hisha => (((absMoveDistance % 9 == 0 && board.upJumpCheck(selectedCellIndex, clickedIndex) && board.downJumpCheck(selectedCellIndex, clickedIndex)) //縦(上下)方向
                || (selectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(selectedCellIndex, clickedIndex) && board.leftJumpCheck(selectedCellIndex, clickedIndex))) //横方向
                && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && moveDistance != 0)
            case ClickedKomaState.To => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.NariKyo => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.NariKei => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.NariGin => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Uma => ((board.leftUpRightDownMove(selectedCellIndex, clickedIndex) && board.leftUpJumpCheck(selectedCellIndex, clickedIndex) && board.rightDownJumpCheck(selectedCellIndex, clickedIndex))
              || (board.rightUpLeftDownMove(selectedCellIndex, clickedIndex) && board.rightUpJumpCheck(selectedCellIndex, clickedIndex) && board.leftDownJumpCheck(selectedCellIndex, clickedIndex))
              || (absMoveDistance == 1 || absMoveDistance == 9)) && notCrossOnBoard(selectedCellIndex) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && moveDistance != 0
            case ClickedKomaState.Ryu => ((absMoveDistance % 9 == 0 && board.upJumpCheck(selectedCellIndex, clickedIndex) && board.downJumpCheck(selectedCellIndex, clickedIndex)) //縦(上下)方向
              || (selectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(selectedCellIndex, clickedIndex) && board.leftJumpCheck(selectedCellIndex, clickedIndex)) //横方向
              || (absMoveDistance == 8 || absMoveDistance == 10)) && notCrossOnBoard(selectedCellIndex) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && moveDistance != 0
            case _ => false
          }
        } else {
          koma match {
            case ClickedKomaState.Fu => moveDistance == -9 && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Kyo => (moveDistance % 9 == 0 && moveDistance < 0 && board.downJumpCheck(selectedCellIndex, clickedIndex)) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Kei => (moveDistance == -17 || moveDistance == -19) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Gin => (absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == -9) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Kin => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Ou => (absMoveDistance == 1 || absMoveDistance == 9 || absMoveDistance == 8 || absMoveDistance == 10) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Kaku => (((board.leftUpRightDownMove(selectedCellIndex, clickedIndex)&& board.leftUpJumpCheck(selectedCellIndex, clickedIndex) && board.rightDownJumpCheck(selectedCellIndex, clickedIndex)) //左上から右下方向
              || (board.rightUpLeftDownMove(selectedCellIndex, clickedIndex) && board.rightUpJumpCheck(selectedCellIndex, clickedIndex) && board.leftDownJumpCheck(selectedCellIndex, clickedIndex))) //右上から左下方向
              && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && moveDistance != 0)
            case ClickedKomaState.Hisha => (((absMoveDistance % 9 == 0 && board.upJumpCheck(selectedCellIndex, clickedIndex) && board.downJumpCheck(selectedCellIndex, clickedIndex)) //縦(上下)方向
              || (selectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(selectedCellIndex, clickedIndex) && board.leftJumpCheck(selectedCellIndex, clickedIndex))) //横方向
              && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && moveDistance != 0)
            case ClickedKomaState.To => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.NariKyo => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.NariKei => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.NariGin => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && notCrossOnBoard(selectedCellIndex)
            case ClickedKomaState.Uma => ((board.leftUpRightDownMove(selectedCellIndex, clickedIndex) && board.leftUpJumpCheck(selectedCellIndex, clickedIndex) && board.rightDownJumpCheck(selectedCellIndex, clickedIndex))
              || (board.rightUpLeftDownMove(selectedCellIndex, clickedIndex) && board.rightUpJumpCheck(selectedCellIndex, clickedIndex) && board.leftDownJumpCheck(selectedCellIndex, clickedIndex))
              || (absMoveDistance == 1 || absMoveDistance == 9)) && notCrossOnBoard(selectedCellIndex) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && moveDistance != 0
            case ClickedKomaState.Ryu => ((absMoveDistance % 9 == 0 && board.upJumpCheck(selectedCellIndex, clickedIndex) && board.downJumpCheck(selectedCellIndex, clickedIndex)) //縦(上下)方向
              || (selectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(selectedCellIndex, clickedIndex) && board.leftJumpCheck(selectedCellIndex, clickedIndex)) //横方向
              || (absMoveDistance == 8 || absMoveDistance == 10)) && notCrossOnBoard(selectedCellIndex) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && moveDistance != 0
            case _ => false
          }
        }
      }
      canMove
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

    /** 王が詰んでいるかチェックするためのState */
    var tyuAiFalseList: List[Int] = Nil
    var toIndexStock: List[Int] = Nil
    var ouTookKomaStock: List[Int] = Nil
    var canNotTyuAi: List[Int] = Nil
    var (canTyuAiFlag: Boolean, notGetBackKoma: Boolean, notGetBackTyuaiKoma: Boolean) = (true, true, true)

    /** def isCheckmateCheck内で使う汎用関数 */
    /** すべての駒を調べた時、そこへ移動できるか調べる関数(from,to引数としたを場所に応じて) */
    /** 盤面内を横切っていないか */

    /** 以下、王が詰んでいるかのチェック関数 */
    def isCheckmateCheck: Boolean = {

      def canMovePlace(koma: ClickedKomaState, fromIndex: Int, toIndex: Int, isTumasuKoma: Boolean, board: Board): Boolean = {
        val absPlaceMoveDistance = Math.abs(fromIndex - toIndex) //駒の移動距離の絶対値を定義
        val placeMoveDistance = fromIndex - toIndex //駒の移動距離を定義

        def tumiNotCrossOnBoard(index: Int): Boolean = {
          if ((index % 9) + 1 == 9) { //9筋の時
            if (placeMoveDistance == 8 || placeMoveDistance == -1 || placeMoveDistance == -10 || placeMoveDistance == -19 || placeMoveDistance == 17) false
            else true
          } else if ((index % 9) + 1 == 1) { //1筋の時
            if (placeMoveDistance == -8 || placeMoveDistance == 1 || placeMoveDistance == 10 || placeMoveDistance == -17 || placeMoveDistance == 19) false
            else true
          } else true
        }

        //飛び駒では、王の効きがある場合には飛び越えた効きになる
        def goteKomaMove = koma match {
          case ClickedKomaState.Fu => placeMoveDistance == -9 && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Kyo => (placeMoveDistance % 9 == 0 && placeMoveDistance < 0 && board.checkMateDownJumpCheck(fromIndex, toIndex)) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Kei => (placeMoveDistance == -17 || placeMoveDistance == -19) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Gin => (absPlaceMoveDistance == 8 || absPlaceMoveDistance == 10 || placeMoveDistance == -9) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Kin => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == -8 || placeMoveDistance == -10) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Ou => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || absPlaceMoveDistance == 8 || absPlaceMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Kaku => (((board.leftUpRightDownMove(fromIndex, toIndex) && board.checkMateLeftUpJumpCheck(fromIndex, toIndex) && board.checkMateRightDownJumpCheck(fromIndex, toIndex)) //左上から右下方向
            || (board.rightUpLeftDownMove(fromIndex, toIndex) && board.checkMateRightUpJumpCheck(fromIndex, toIndex) && board.checkMateLeftDownJumpCheck(fromIndex, toIndex))) //右上から左下方向
            && board.fromToMoveBoard(fromIndex, toIndex) && placeMoveDistance != 0)
          case ClickedKomaState.Hisha => (((absPlaceMoveDistance % 9 == 0 && board.checkMateUpJumpCheck(fromIndex, toIndex) && board.checkMateDownJumpCheck(fromIndex, toIndex)) //縦(上下)方向
            || (fromIndex / 9 == toIndex / 9 && board.checkMateRightJumpCheck(fromIndex, toIndex) && board.checkMateLeftJumpCheck(fromIndex, toIndex))) //横方向
            && board.fromToMoveBoard(fromIndex, toIndex) && placeMoveDistance != 0)
          case ClickedKomaState.To => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == -8 || placeMoveDistance == -10) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.NariKyo => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == -8 || placeMoveDistance == -10) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.NariKei => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == -8 || placeMoveDistance == -10) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.NariGin => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == -8 || placeMoveDistance == -10) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Uma => ((board.leftUpRightDownMove(fromIndex, toIndex) && board.checkMateLeftUpJumpCheck(fromIndex, toIndex) && board.checkMateRightDownJumpCheck(fromIndex, toIndex))
            || (board.rightUpLeftDownMove(fromIndex, toIndex) && board.checkMateRightUpJumpCheck(fromIndex, toIndex) && board.checkMateLeftDownJumpCheck(fromIndex, toIndex))
            || (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9)) && tumiNotCrossOnBoard(fromIndex) && board.fromToMoveBoard(fromIndex, toIndex) && placeMoveDistance != 0
          case ClickedKomaState.Ryu => ((absPlaceMoveDistance % 9 == 0 && board.checkMateUpJumpCheck(fromIndex, toIndex) && board.checkMateDownJumpCheck(fromIndex, toIndex)) //縦(上下)方向
            || (fromIndex / 9 == toIndex / 9 && board.checkMateRightJumpCheck(fromIndex, toIndex) && board.checkMateLeftJumpCheck(fromIndex, toIndex)) //横方向
            || (absPlaceMoveDistance == 8 || absPlaceMoveDistance == 10)) && tumiNotCrossOnBoard(fromIndex) && board.fromToMoveBoard(fromIndex, toIndex) && placeMoveDistance != 0
          case _ => false
        }

        def senteKomaMove = koma match {
          case ClickedKomaState.Fu => placeMoveDistance == 9 && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Kyo => placeMoveDistance % 9 == 0 && placeMoveDistance > 0 && board.checkMateUpJumpCheck(fromIndex, toIndex) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Kei => (placeMoveDistance == 17 || placeMoveDistance == 19) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Gin => (absPlaceMoveDistance == 8 || absPlaceMoveDistance == 10 || placeMoveDistance == 9) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Kin => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == 8 || placeMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Ou => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || absPlaceMoveDistance == 8 || absPlaceMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Kaku => (((board.leftUpRightDownMove(fromIndex, toIndex) && board.checkMateLeftUpJumpCheck(fromIndex, toIndex) && board.checkMateRightDownJumpCheck(fromIndex, toIndex)) //左上から右下方向
            || (board.rightUpLeftDownMove(fromIndex, toIndex) && board.checkMateRightUpJumpCheck(fromIndex, toIndex) && board.checkMateLeftDownJumpCheck(fromIndex, toIndex))) //右上から左下方向
            && board.fromToMoveBoard(fromIndex, toIndex) && placeMoveDistance != 0)
          case ClickedKomaState.Hisha => (((absPlaceMoveDistance % 9 == 0 && board.checkMateUpJumpCheck(fromIndex, toIndex) && board.checkMateDownJumpCheck(fromIndex, toIndex)) //縦(上下)方向
            || (fromIndex / 9 == toIndex / 9 && board.checkMateRightJumpCheck(fromIndex, toIndex) && board.checkMateLeftJumpCheck(fromIndex, toIndex))) //横方向
            && board.fromToMoveBoard(fromIndex, toIndex) && placeMoveDistance != 0)
          case ClickedKomaState.To => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == 8 || placeMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.NariKyo => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == 8 || placeMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.NariKei => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == 8 || placeMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.NariGin => (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9 || placeMoveDistance == 8 || placeMoveDistance == 10) && board.fromToMoveBoard(fromIndex, toIndex) && tumiNotCrossOnBoard(fromIndex)
          case ClickedKomaState.Uma => ((board.leftUpRightDownMove(fromIndex, toIndex) && board.checkMateLeftUpJumpCheck(fromIndex, toIndex) && board.checkMateRightDownJumpCheck(fromIndex, toIndex))
            || (board.rightUpLeftDownMove(fromIndex, toIndex) && board.checkMateRightUpJumpCheck(fromIndex, toIndex) && board.checkMateLeftDownJumpCheck(fromIndex, toIndex))
            || (absPlaceMoveDistance == 1 || absPlaceMoveDistance == 9)) && tumiNotCrossOnBoard(fromIndex) && board.fromToMoveBoard(fromIndex, toIndex) && placeMoveDistance != 0
          case ClickedKomaState.Ryu => ((absPlaceMoveDistance % 9 == 0 && board.checkMateUpJumpCheck(fromIndex, toIndex) && board.checkMateDownJumpCheck(fromIndex, toIndex)) //縦(上下)方向
            || (fromIndex / 9 == toIndex / 9 && board.checkMateRightJumpCheck(fromIndex, toIndex) && board.checkMateLeftJumpCheck(fromIndex, toIndex)) //横方向
            || (absPlaceMoveDistance == 8 || absPlaceMoveDistance == 10)) && tumiNotCrossOnBoard(fromIndex) && board.fromToMoveBoard(fromIndex, toIndex) && placeMoveDistance != 0
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
      /** def isCheckmateCheck内で使う汎用関数 */

      val ownOuIndex: Int = board.findKomaKind(ClickedKomaState.Ou, isSenteTurnState) //自分の王の位置
      val enemyOuIndex: Int = board.findKomaKind(ClickedKomaState.Ou, !isSenteTurnState) //自分の王の位置
      /** getBackWithoutOuPattern */
      //toIndexに王以外の駒が動けるかどうかを調べることで、相手の駒を取り返せるかどうかを調べるための関数
      def canNotGetBack(toIndex: Int): Boolean = {
        for (fromIndex <- 0 to 80) {
          if (canTakePlace(fromIndex, toIndex, false, board)) { //詰まされる側の駒で、王を取ろうとしている駒を取れるか
            //王以外の駒で取る場合、取れる駒を動かしたboard上で効きがない場合に、falseとなる
            if (!isThereKomaKind(fromIndex).contains(ClickedKomaState.Ou)) {
              var afterMoveStock: List[Int] = Nil
              for (reCheckfromIndex <- 0 to 80) {
                if (canTakePlace(reCheckfromIndex, ownOuIndex, true, board.moveKoma(fromIndex, toIndex))) {
                  //詰ます側が詰まされる王の場所に効きがある(つまり取れる)駒がどこにいるのか
                  afterMoveStock = reCheckfromIndex :: afterMoveStock
                }
              }
              if (afterMoveStock.length <= 1) notGetBackKoma = false //駒の効きの数が変わらなければ詰みを回避している(実際には駒を取っていない)
            }
            //王で取る場合、移動先で王を取ることができない(取った駒に紐が付いていない)場合、falseになる
            if (isThereKomaKind(fromIndex).contains(ClickedKomaState.Ou)) {
              var ouAfterMoveStock: List[Int] = Nil
              for (reCheckfromIndex <- 0 to 80) {
                if (canTakePlace(reCheckfromIndex, toIndex, true, board.moveKoma(ownOuIndex, toIndex))) { //王はownOuIndexからtoIndexに移動
                  ouAfterMoveStock = reCheckfromIndex :: ouAfterMoveStock
                }
              }
              if (ouAfterMoveStock.isEmpty) notGetBackKoma = false //駒の効きの数が変わらない場所が存在すれば詰みを回避している(true)
            }
          }
        }
        notGetBackKoma
      }

      /** TyuaiPattern */
      //自分の駒がtoIndexの場所に動くことができる駒について、fromIndex(どこから動いて来れるのか)をチェックしている
      def canMoveWithoutOuTyuAi(toIndex: Int): List[Int] = {
        tyuAiFalseList = Nil
        for (fromIndex <- 0 to 80) {
          if (canTakePlace(fromIndex, toIndex, false, board) &&
            !isThereKomaKind(fromIndex).contains(ClickedKomaState.Ou)) {
            tyuAiFalseList = fromIndex :: tyuAiFalseList
          }
        }
        tyuAiFalseList
      }

      val senteHandKomaSet: List[Int] = List(112,113,114,115,116,124,125).filter(index => isThereKomaKind(index).isDefined)
      val goteHandKomaSet: List[Int] = List(100,101,102,103,104,88,89).filter(index => isThereKomaKind(index).isDefined)

      //持ち駒で中合いができるかを判別する関数
      def handTyuai(index:Int): Boolean = {
        isSenteTurnState match {
          case true => {
            if (senteHandKomaSet.contains(115) || senteHandKomaSet.contains(116) || senteHandKomaSet.contains(124) || senteHandKomaSet.contains(125)) true
            else if ((senteHandKomaSet.contains(113) && (index / 9 + 1) != 1) || //香車を持っていて1段目ではない
              (senteHandKomaSet.contains(114) && ((index / 9 + 1) != 1 && (index / 9 + 1) != 2)) || //桂馬を持っていて1段目,または2段目ではない
              (senteHandKomaSet.contains(112) && (index / 9 + 1) != 1 && board.nifuCheck(index, isSenteTurnState)) //歩を持っていて、二歩で無いかつ1段目でない
            ) true
            else false
          }
          case false => {
            if (goteHandKomaSet.contains(103) || goteHandKomaSet.contains(104) || goteHandKomaSet.contains(88) || goteHandKomaSet.contains(89)) true
            else if ((goteHandKomaSet.contains(101) && (index / 9 + 1) != 9) || //香車を持っていて9段目ではない
              (goteHandKomaSet.contains(102) && ((index / 9 + 1) != 8 && (index / 9 + 1) != 9)) || //桂馬を持っていて8段目,または9段目ではない
              (goteHandKomaSet.contains(100) && (index / 9 + 1) != 9 && board.nifuCheck(index, isSenteTurnState)) //歩を持っていて、二歩で無いかつ9段目でない
            ) true
            else false
          }
        }
      }

      /* 盤上の駒で中合いができるかを判別する関数(その場所への駒が置けなくなるというデメリットがあるので、最終手段)
      すべてのindexの場所に動ける駒について、「動いたことによって駒の効きが増えない場合」が存在しないならFalseを返す */
      def boardTyuai(index:Int): Boolean = {
        if (canMoveWithoutOuTyuAi(index).nonEmpty) { //その場所に駒が原理的には移動できる場合
          var boardTyuai = false
          canMoveWithoutOuTyuAi(index).foreach(fromIndex => {
            var afterMoveStock: List[Int] = Nil
            for (reCheckfromIndex <- 0 to 80) {
              if (canTakePlace(reCheckfromIndex, ownOuIndex, true, board.moveKoma(fromIndex, index))) { //詰ます側が詰まされる王の場所に効きがある(つまり取れる)駒がどこにいるのか
                afterMoveStock = reCheckfromIndex :: afterMoveStock
              }
            }
            if (afterMoveStock.length <= 1) boardTyuai = true //駒の効きの数が変わらない場所が存在すれば詰みを回避している(true)
          })
          boardTyuai //一度でもtrueがあればtrueである
        }
        else false
      }

      /** ouEscapePattern */
      //王が動ける範囲を定義
      val nineColumnOuMove: List[Int] = List(ownOuIndex - 10, ownOuIndex - 9, ownOuIndex - 1, ownOuIndex, ownOuIndex + 8, ownOuIndex + 9)
      val oneColumnOuMove: List[Int] = List(ownOuIndex - 9, ownOuIndex - 8, ownOuIndex, ownOuIndex + 1, ownOuIndex + 9, ownOuIndex + 10)
      val normalOuMove: List[Int] = List(ownOuIndex - 10, ownOuIndex - 9, ownOuIndex - 8, ownOuIndex - 1, ownOuIndex, ownOuIndex + 1, ownOuIndex + 8, ownOuIndex + 9, ownOuIndex + 10)

      val ouCanMove: List[Int] = {
        if ((ownOuIndex % 9) + 1 == 9) nineColumnOuMove //9筋の時
        else if ((ownOuIndex % 9) + 1 == 1) oneColumnOuMove //1筋の時
        else normalOuMove
      }.filter(index => index >= 0 && index <= 80)

      /** 詰み、勝ちの条件判定 */
      //王の場所に効きがある(つまり取れる)駒がどこにいるのかをStockしている
      for (fromIndex <- 0 to 80) {
        //詰ます側が詰まされる王の場所に効きがある(つまり取れる)駒がどこにいるのか
        if (canTakePlace(fromIndex, ownOuIndex, true, board)) {
          ouTookKomaStock = fromIndex :: ouTookKomaStock
        }
        //詰まされる側が敵の王を取れるかどうか
        if (canTakePlace(fromIndex, enemyOuIndex, false, board)) {
          enemyOuTakeKomaStock = fromIndex :: enemyOuTakeKomaStock
        }

        //すべての駒が効きのある場所を調べる => 逃げるパターンで使う
        for (toIndex <- 0 to 80) {
          if (!toIndexStock.contains(toIndex)) {
            if (canTakePlace(fromIndex, toIndex, true, board)) {
              toIndexStock = toIndex :: toIndexStock
            }
          }
        }
      }

      /** 王手に対する対応策の定義(できない場合にtrueになる点注意)
        * 1. 逃げるパターン
        * 2. 取り返せるパターン
        * 3. 中合いが効くパターン
        */
      //王が逃げるパターン(王で相手の駒を取る場合を含む, 味方の駒がいる場合そこには逃げられない)
      def ouEscapePattern: Boolean = {
        val OucanLive: List[Int] = (ouCanMove diff toIndexStock).filter(index => isNotFriendKoma(index) || index == ownOuIndex)
        val ouCanEscape = OucanLive match {
          case Nil => true
          case _ => false
        }
        ouCanEscape
      }

      //取り返せるパターン(ouTookKomaStock.length==1の場合に使う)
      def getBackWithoutOuPattern: Boolean = ouTookKomaStock.length match {
        case 1 => canNotGetBack(ouTookKomaStock.head)
        case 0 => false
        case _ => true
      }

      //中合いが効くパターン(ouTookKomaStock.length==1の場合に使う)
      def TyuaiPattern: Boolean = ouTookKomaStock.length match {
        case 1 => {
          val tyuAiKoma = true
          val tumiCheckKomas: List[Koma] = board match { case Board(komas) => komas }

          /* 持ち駒で中合いできるか判別
          できる => Fuをおく,
          できない => 2.盤上の駒で中合いできるか判別
          できる => Fuをおく, できない => Blankをおく */
          def tyuAiAdd(index: Int): ClickedKomaState = {
            handTyuai(index) match {
              case true => ClickedKomaState.Fu
              case false => {
                boardTyuai(index) match {
                  case true => ClickedKomaState.Fu
                  case false => ClickedKomaState.Blank
                }
              }
            }
          }

          //true => Fu,False => Blankの駒をつけて、Blankの駒をはがしてから処理する
          val tyuAiKomas =
            Koma(tyuAiAdd(ownOuIndex - 10), ownOuIndex - 10, isSenteTurnState, tyuAiKoma) :: Koma(tyuAiAdd(ownOuIndex - 9), ownOuIndex - 9, isSenteTurnState, tyuAiKoma) ::
              Koma(tyuAiAdd(ownOuIndex - 8), ownOuIndex - 8, isSenteTurnState, tyuAiKoma) :: Koma(tyuAiAdd(ownOuIndex - 1), ownOuIndex - 1, isSenteTurnState, tyuAiKoma) ::
              Koma(tyuAiAdd(ownOuIndex + 1), ownOuIndex + 1, isSenteTurnState, tyuAiKoma) :: Koma(tyuAiAdd(ownOuIndex + 8), ownOuIndex + 8, isSenteTurnState, tyuAiKoma) ::
              Koma(tyuAiAdd(ownOuIndex + 9), ownOuIndex + 9, isSenteTurnState, tyuAiKoma) :: Koma(tyuAiAdd(ownOuIndex + 10), ownOuIndex + 10, isSenteTurnState, tyuAiKoma) ::
              tumiCheckKomas
          val tyuAiBoard: Board = Board(tyuAiKomas.filterNot(koma => koma.kind == ClickedKomaState.Blank))

          for (fromIndex <- 0 to 80) {
            if (canTakePlace(fromIndex, ownOuIndex, true, tyuAiBoard)) { //再度王が取られることがないかチェック
              canNotTyuAi = fromIndex :: canNotTyuAi
            }
          }
          if (canNotTyuAi.isEmpty) canTyuAiFlag = false //王を狙う駒が一つの場合で、中合いすると0になる場合は、詰みを回避できる
          canTyuAiFlag
        }

        case 0 => false
        case _ => true
      }

      /** 詰みチェックをする関数(詰みである場合にtrue、詰みでない場合にfalseを返す)
        * - 王に対する駒の効きが無い => 詰みではない
        * - 王を取る駒が一つ => 王以外でその駒を取り返す、及び、中合いでの対応が可能
        * - 王に2つ以上の効きがある => 効きから逃げる以外の選択肢がない
        */
      def isCheckmateCheckLogic: Boolean = {
        if (ouTookKomaStock.isEmpty) false
        else if (ouTookKomaStock.length == 1) {
          ouEscapePattern && TyuaiPattern && getBackWithoutOuPattern && enemyOuTakeKomaStock.isEmpty
        }
        else {
          ouEscapePattern && enemyOuTakeKomaStock.isEmpty
        }
      }
      //デバッグ用
      println(ouEscapePattern, TyuaiPattern, getBackWithoutOuPattern, enemyOuTakeKomaStock.isEmpty, enemyOuTakeKomaStock.nonEmpty)
      println("enemyOuTakeKomaStock", enemyOuTakeKomaStock, "ouTookKomaStock", ouTookKomaStock)
      //println("enemy",enemyOuIndex,"own",ownOuIndex)

      isCheckmateCheckLogic
    }

    def initializeTumiState = {
      isCheckmate = None
      isOuCatch = None
      tyuAiFalseList = Nil
      toIndexStock = Nil
      ouTookKomaStock = Nil
      enemyOuTakeKomaStock = Nil
      canNotTyuAi = Nil
      canTyuAiFlag = true
      notGetBackKoma = true
      notGetBackTyuaiKoma = true
    }
    /** ここまで、王が詰んでいるかチェックする関数 */


    /** Cellの描画を定義 */
    val isFillLightBulue: Boolean = selectedCellIndex == clickedIndex && clickedKomaKind != ClickedKomaState.None

    val fillColor = if (isFillLightBulue) LightBlue
    else if (canMove(clickedKomaKind) && isNotFriendKoma(clickedIndex)
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
      (optClickedKomaKind.contains(ClickedKomaState.Na) && selectedCellIndex == 106) || optClickedKomaKind.contains(ClickedKomaState.Ri)
    }
    def funariChoiceBranch: Boolean = {
      (optClickedKomaKind.contains(ClickedKomaState.Na) && selectedCellIndex == 110) || optClickedKomaKind.contains(ClickedKomaState.Not)
    }

    /** 初期化, 待った */
    def initializationBranch = optClickedKomaKind.contains(ClickedKomaState.A) || optClickedKomaKind.contains(ClickedKomaState.B) ||
      optClickedKomaKind.contains(ClickedKomaState.C) || optClickedKomaKind.contains(ClickedKomaState.D)
    def waitBranch = optClickedKomaKind.contains(ClickedKomaState.Ma) || optClickedKomaKind.contains(ClickedKomaState.Ltu) || optClickedKomaKind.contains(ClickedKomaState.TaHira)

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

    var firstClickFlag: Boolean = false
    def clickCancel = {
      if (moveDistance != 0 && !firstClickFlag) {
        clickedKomaKind = ClickedKomaState.None
        optIsSenteKomaState = None
        optOnBoardKomaState = None
        selectedCellIndex = -100
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
        case ClickedKomaState.Ou =>  {
          isWin = true
        }
        case _ =>
      }
    }

    /** 成れるかどうかの条件判定 */
    def mustNari: Boolean = isSenteTurnState match {
      case true => {
        ((clickedKomaKind == ClickedKomaState.Fu || clickedKomaKind == ClickedKomaState.Kyo) && (clickedIndex / 9) + 1 == 1) || //先手の歩と香車が1段目
          (clickedKomaKind == ClickedKomaState.Kei && (clickedIndex / 9 + 1) <= 2) //先手の桂馬が1段目と2段目
      }
      case false => {
        ((clickedKomaKind == ClickedKomaState.Fu || clickedKomaKind == ClickedKomaState.Kyo) && (clickedIndex / 9) + 1 == 9) ||
          (clickedKomaKind == ClickedKomaState.Kei && (clickedIndex / 9 + 1) >= 8)
      }
    }

    def canNari: Boolean = isSenteTurnState match {
      case true => {
        ((clickedIndex / 9) + 1 <= 3 || (selectedCellIndex / 9) + 1 <= 3) && isNariKoma && fromOnBoard
      } //成れる駒が、3段目以内にいた、もしくは3段目以内に入った
      case false => {
        (((clickedIndex / 9) + 1 >= 7) || (selectedCellIndex / 9) + 1 >= 7) && isNariKoma && fromOnBoard
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
      selectedCellIndex = -100
    }

    /** 実際に手を指し、今までの条件を初期化する */
    def playAndInitialize = {

      //takeKomaでを保存していない場合に, moveKomaする前に保存しておく
      if ((isSenteTurnState && !optIsSenteKoma.contains(false)) || (!isSenteTurnState && !optIsSenteKoma.contains(true))) {
        pastBoard = board
      }

      /** 成り不成の処理 */
      if (mustNari) {
        board = board.nariKoma(selectedCellIndex) //強制的に成り、相手の手番へ
        isSenteTurnState = switchTurn(isSenteTurnState)
      }
      else if (canNari) addNariGomaState //どこにいる駒が成れる状態、という状態を付与
      else isSenteTurnState = switchTurn(isSenteTurnState) //成れない場合は相手の手番へ

      board = board.moveKoma(selectedCellIndex, clickedIndex)

      /** 棋譜の出力 */
      val place = clickedIndex
      val movedKoma = {
        //mustNariの場合はここで棋譜に追加、canNariの場合はボタンを選択した時に追加する方式に
        if (mustNari) board.findPlaceKomaKind(clickedIndex).name + "成り"
        else if (canSetFromHand) board.findPlaceKomaKind(clickedIndex).name + "打"
        else board.findPlaceKomaKind(clickedIndex).name
      }

      val tate = (clickedIndex / 9 + 1).toString
      val yoko = (9 - (clickedIndex % 9)).toString
      kifu = yoko :: tate :: movedKoma :: kifu

      if (optOnBoardKomaState.contains(false)){
        board = board.spaceChangeKoma(clickedIndex, optOnBoard.contains(false)) //打ち終わった駒は盤上の駒になる
      }

      /** 初期化 */
      selectedCellIndex = -100
      clickedKomaKind = ClickedKomaState.None
      optIsSenteKomaState = None
      optOnBoardKomaState = None
    }

    /** 盤上の駒を動かす時のtakeKomaとplayAndInitializeの条件分岐処理をまとめた */
    def takeKomaAndplayAndInitialize = {
      isSenteTurnState match {
        case true => {
          if (optIsSenteKoma.contains(false)) takeKoma(clickedIndex)
          if (!optIsSenteKoma.contains(true)) playAndInitialize
          else clickCancel
        }
        case false => {
          if (optIsSenteKoma.contains(true)) takeKoma(clickedIndex)
          if (!optIsSenteKoma.contains(false)) playAndInitialize
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
    def useHandKomaFlow = {
      fromHandToBoradAddState
      if (toMoveBoard && optIsSenteKoma.isEmpty &&
        !(clickedKomaKind != ClickedKomaState.Fu || board.nifuCheck(clickedIndex, optIsSenteKomaState.contains(true)))) isNifu = true
      if (canSetFromHand) {
        playAndInitialize
        tumiCheckFlow
      }
      else clickCancel
    }

    /** 盤上から盤上へ移動する駒が行う処理 */
    def inBordKomaMoveFlow(koma: ClickedKomaState) = {
      fromToBoradAddState(koma)
      if (canMove(koma)) {
        takeKomaAndplayAndInitialize
        if (!isCanNari) tumiCheckFlow
      }
      else clickCancel
    }

    def tumiCheckFlow = {
      if (!isCanNari) { //isCanNari状態の場合は、成り不成が確定した時に行う
        if (isCheckmateCheck) {
          isWin = true
          isCheckmate = Some(true)
          logOutPut
        }
        if (enemyOuTakeKomaStock.nonEmpty) { //手番で、手側側の駒が成ろうとしてる状態を除く
          isWin = true
          isOuCatch = Some(true)
          logOutPut
        }
      }
    }

    def initializationOrWaitFlow = {
      if (initializationBranch) {
        board = optClickedKomaKind match { //初期化時の駒配置の選択が可能
          case Some(ClickedKomaState.A) => initalBoard
          case Some(ClickedKomaState.B) => newUnderFuInitalBoard
          case Some(ClickedKomaState.C) => halfRandomBoard
          case Some(ClickedKomaState.D) => allRandomBoard
          case _ => board
        }
        pastBoard = board //待ったはなし
        kifu = List("まで","手で","勝ち")
        isSenteTurnState = true
      }
      else if (waitBranch) {
        board = pastBoard
        kifu = kifu.drop(3) //待ったをした場合を取り除く
        isSenteTurnState = !isSenteTurnState
      }
      firstClickFlag = false
      clickCancel
      isCanNari = false
      isWin = false
      isNifu = false
      selectedCellIndex = -100
      stockNariIndex = -1
      initializeTumiState
      tumiCheckFlow
    }

    def nariChoiceFlow = {
      board = board.nariKoma(stockNariIndex)
      kifu = kifu match { //kifuListの3番目に不成を追加
        case a :: b :: c :: past => a :: b :: c+"成り" :: past
        case _ => List("")
      }
      initializeNariGomaState //状態を元に戻す
      tumiCheckFlow //成りの状態を加えたらチェック
    }

    def fuNariChoiceFlow = {
      kifu = kifu match { //kifuListの3番目に不成を追加
        case a :: b :: c :: past => a :: b :: c+"不成" :: past
        case _ => List("")
      }
      initializeNariGomaState //状態を元に戻す
      tumiCheckFlow //不成の状態を加えたらチェック
    }
    /** ここまで駒をクリックした時に使われる関数群 */

    /** 実際に駒がクリックがされた場合の処理 */
    group.setOnMouseClicked(e => {

      /** 初期化されている場合は、クリックされている座標を代入する */
      firstClickFlag = false
      if (selectedCellIndex == -100) {
        selectedCellIndex = clickedIndex
        firstClickFlag = true
      }

      /** 初期化、待ったをクリックした場合の処理 */
      if (initializationBranch) initializationOrWaitFlow
      else if (waitBranch) initializationOrWaitFlow

      /** 駒が成るかどうかの判定をクリックした場合の処理 */
      else if (nariChoiceBranch) nariChoiceFlow
      else if (funariChoiceBranch) fuNariChoiceFlow

      /** 持ち駒をクリックして盤面に打つ場合の処理 */
      else if (useHandKomaBranch) useHandKomaFlow
      /** 盤面の歩を移動させる場合の処理 */
      else if (inBoardKomaBranch(ClickedKomaState.Fu)) inBordKomaMoveFlow(ClickedKomaState.Fu)
      /** 香車の場合 */
      else if (inBoardKomaBranch(ClickedKomaState.Kyo)) inBordKomaMoveFlow(ClickedKomaState.Kyo)
      /** 桂馬の場合 */
      else if (inBoardKomaBranch(ClickedKomaState.Kei)) inBordKomaMoveFlow(ClickedKomaState.Kei)
      /** 銀の場合 */
      else if (inBoardKomaBranch(ClickedKomaState.Gin)) inBordKomaMoveFlow(ClickedKomaState.Gin)
      /** 金の場合 */
      else if (inBoardKomaBranch(ClickedKomaState.Kin)) inBordKomaMoveFlow(ClickedKomaState.Kin)
      /** 王の場合 */
      else if (inBoardKomaBranch(ClickedKomaState.Ou)) inBordKomaMoveFlow(ClickedKomaState.Ou)
      /** 角の場合 */
      else if (inBoardKomaBranch(ClickedKomaState.Kaku)) inBordKomaMoveFlow(ClickedKomaState.Kaku)
      /** 飛車の場合 */
      else if (inBoardKomaBranch(ClickedKomaState.Hisha)) inBordKomaMoveFlow(ClickedKomaState.Hisha)
      /** との場合 */
      else if (inBoardKomaBranch(ClickedKomaState.To)) inBordKomaMoveFlow(ClickedKomaState.To)
      /** 成香の場合 */
      else if (inBoardKomaBranch(ClickedKomaState.NariKyo)) inBordKomaMoveFlow(ClickedKomaState.NariKyo)
      /** 成桂の場合 */
      else if (inBoardKomaBranch(ClickedKomaState.NariKei)) inBordKomaMoveFlow(ClickedKomaState.NariKei)
      /** 成銀の場合 */
      else if (inBoardKomaBranch(ClickedKomaState.NariGin)) inBordKomaMoveFlow(ClickedKomaState.NariGin)
      /** 馬の場合 */
      else if (inBoardKomaBranch(ClickedKomaState.Uma)) inBordKomaMoveFlow(ClickedKomaState.Uma)
      /** 龍の場合 */
      else if (inBoardKomaBranch(ClickedKomaState.Ryu)) inBordKomaMoveFlow(ClickedKomaState.Ryu)

      else {
        firstClickFlag = false
        clickCancel
      }

      /*デバッグ用
      println("selectedCellIndex:" + selectedCellIndex,"clickedIndex:"+clickedIndex,"stockNariIndex:" + stockNariIndex)
      println("optOnBoard:" + optOnBoard, "optOnBoardKomaState:" + optOnBoardKomaState)
      println("optIsSenteKoma:" + optIsSenteKoma, "optIsSenteKomaState:" + optIsSenteKomaState)
      println("optClickedKomaKind:" + optClickedKomaKind, "clickedKomaKind:" + clickedKomaKind)
      println("isSenteTurnState:" + isSenteTurnState)
      println("")
      */

      boardSwitch
      isNifu = false
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

    val komaLabel = { //升内の駒の置き場所を定義してる
    val label = new Label
      label.setText(koma.kind.name)
      if (!koma.isSente && !(koma.index >= 81 && koma.onBoard)) label.setRotate(180)
      if (koma.kind == ClickedKomaState.Ten || koma.kind == ClickedKomaState.Eleven || koma.kind == ClickedKomaState.Twelve || koma.kind == ClickedKomaState.Thirteen
        || koma.kind == ClickedKomaState.Fourteen || koma.kind == ClickedKomaState.Fifteen || koma.kind == ClickedKomaState.Sixteen
        || koma.kind == ClickedKomaState.Seventeen || koma.kind == ClickedKomaState.Eighteen) label.setFont(Font(25))
      else label.setFont(Font(30))
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
  def testBoard2: Board = Board(List(
    /*    Koma(ClickedKomaState.Ou, 28, true, true), Koma(ClickedKomaState.Gin, 10, false, true), Koma(ClickedKomaState.Ou, 0, false, true), Koma(ClickedKomaState.Kyo, 9, false, true),
    Koma(ClickedKomaState.Kei, 1, false, true), Koma(ClickedKomaState.Kin, 24, true, true), Koma(ClickedKomaState.Kyo, 113, true, false),
    Koma(ClickedKomaState.Fu, 100, false, false), Koma(ClickedKomaState.Hisha, 125, true, false), Koma(ClickedKomaState.Fu, 49, false, true),
    Koma(ClickedKomaState.Kaku, 40, true, true), Koma(ClickedKomaState.Kei, 114, true, false)    */
    Koma(ClickedKomaState.Ou, 2, true, true), Koma(ClickedKomaState.Gin, 3, false, true), Koma(ClickedKomaState.Ou, 4, false, true), Koma(ClickedKomaState.Kin, 6, true, true),
    Koma(ClickedKomaState.Kin, 20, true, true), Koma(ClickedKomaState.Kin, 24, true, true), Koma(ClickedKomaState.Kyo, 113, true, false),
    Koma(ClickedKomaState.Fu, 100, false, false), Koma(ClickedKomaState.Hisha, 125, true, false), Koma(ClickedKomaState.Fu, 40, false, true)
  ))

  //仮装条件での検証用のBoard
  def testBoard: Board = Board(List(
    Koma(ClickedKomaState.Fu, 112, true, false), Koma(ClickedKomaState.Fu, 112, true, false), Koma(ClickedKomaState.Fu, 112, true, false),
    Koma(ClickedKomaState.Fu, 112, true, false), Koma(ClickedKomaState.Fu, 112, true, false), Koma(ClickedKomaState.Fu, 112, true, false),
    Koma(ClickedKomaState.Fu, 112, true, false), Koma(ClickedKomaState.Fu, 112, true, false), Koma(ClickedKomaState.Fu, 112, true, false),
    Koma(ClickedKomaState.Kyo, 113, true, false), Koma(ClickedKomaState.Kei, 114, true, false),
    Koma(ClickedKomaState.Gin, 115, true, false), Koma(ClickedKomaState.Kin, 116, true, false), Koma(ClickedKomaState.Ou, 76, true, true),
    Koma(ClickedKomaState.Kyo, 113, true, false), Koma(ClickedKomaState.Kei, 114, true, false), Koma(ClickedKomaState.Gin, 115, true, false), Koma(ClickedKomaState.Kin, 116, true, false),
    Koma(ClickedKomaState.Hisha, 125, true, false), Koma(ClickedKomaState.Kaku, 124, true, false),
    Koma(ClickedKomaState.Hisha, 89, false, false), Koma(ClickedKomaState.Kaku, 88, false, false),
    Koma(ClickedKomaState.Kyo, 101, false, false), Koma(ClickedKomaState.Kei, 102, false, false),
    Koma(ClickedKomaState.Gin, 103, false, false), Koma(ClickedKomaState.Kin, 104, false, false), Koma(ClickedKomaState.Ou, 4, false, false),
    Koma(ClickedKomaState.Kyo, 101, false, false), Koma(ClickedKomaState.Kei, 102, false, false), Koma(ClickedKomaState.Gin, 103, false, false), Koma(ClickedKomaState.Kin, 104, false, false),
    Koma(ClickedKomaState.Fu, 100, false, false), Koma(ClickedKomaState.Fu, 100, false, false), Koma(ClickedKomaState.Fu, 100, false, false),
    Koma(ClickedKomaState.Fu, 100, false, false), Koma(ClickedKomaState.Fu, 100, false, false), Koma(ClickedKomaState.Fu, 100, false, false),
    Koma(ClickedKomaState.Fu, 100, false, false), Koma(ClickedKomaState.Fu, 100, false, false), Koma(ClickedKomaState.Fu, 100, false, false)
  ))

}
