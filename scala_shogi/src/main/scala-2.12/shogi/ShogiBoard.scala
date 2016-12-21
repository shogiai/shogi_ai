package shogi

import java.io._

import scala.collection.immutable.::
import scala.util.Random
import scalafx.application.JFXApp
import scalafx.geometry.Pos
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ButtonType, Label}
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Polygon, Rectangle}
import scalafx.scene.text.Font
import scalafx.scene.{Cursor, Group, Scene}

/** JFXApp { を使い、traitの設定をしつつ、*/
object ShogiBoard extends JFXApp {
  val (senteSideKoma, goteSideKoma) = (true, false)
  val (onBoardStartKoma, handStartKoma) = (true, false)
  var (isToryo, originalPushed) = (false, false)

  val initialKomas: List[Koma] = initialBoard match { case Board(komas) => komas }
  val initialLength: Int = initialKomas.length
  val displayKoma: Boolean = true
  val (displaySenteKoma: Boolean, displayGoteKoma: Boolean) = (true, false)

  var board: Board = Board(
    Koma(ClickedKomaState.Sen, 87, displaySenteKoma, displayKoma) :: Koma(ClickedKomaState.Te, 93, displaySenteKoma, displayKoma) :: Koma(ClickedKomaState.Ban, 99, displaySenteKoma, displayKoma) ::
      Koma(ClickedKomaState.GoKakuGo, 117, displaySenteKoma, displayKoma) :: Koma(ClickedKomaState.GoKaKuKaKu, 123, displaySenteKoma, displayKoma) ::
      Koma(ClickedKomaState.Original, 108, displaySenteKoma, displayKoma) :: initialKomas)
  var pastBoard: Board = board

  /** 棋譜の出力 */
  var kifu: List[String] = List("まで","手で","勝ち")

  var evaluationLog: List[Int] = Nil
  val LOG_FILE_PATH = "kifu.txt" //ログ出力先

  def logOutPut {

    val tesu = ((kifu.length + 1) - 3) / 4
    val winPlayer: String = (isOuCatch, isCheckmate, toryoPushed) match {
      case (Some(true), _, _) => { //勝ちの場合
        tesu % 2 match {
          case 1 => "後手"
          case 0 => "先手"
          case _ => "??"
        }
      }
      case _ => { //詰み、投了の場合
        tesu % 2 match {
          case 1 => "先手"
          case 0 => "後手"
          case _ => "??"
        }
      }
    }

    var outPutKifu: List[String] = kifu.takeRight(3)
    outPutKifu = outPutKifu match {
      case List(a,b,c) => List(a + tesu.toString + b + winPlayer + c)
      case  _ => List("")
    }

    kifu = kifu.dropRight(3)
    while(kifu.length >= 4) {
      val itteList = kifu.take(4)
      val itte: String = itteList match {
        case List(yoko: String, tate: String, koma: String, evaluate: String) => yoko + tate + koma + evaluate
        case _ => ""
      }

      outPutKifu = itte :: outPutKifu
      kifu = kifu.drop(4)
    }

    val in = new File(LOG_FILE_PATH)
    val out = new PrintWriter(new FileWriter(LOG_FILE_PATH, true))
    var kaigyoCount = 0

    //棋譜の出力
    if (outPutKifu.length >= 2) { //0手で勝利した場合は出力しない
      outPutKifu.foreach(itte => {
        out.print(itte + " ")

        kaigyoCount = kaigyoCount + 1
        if (kaigyoCount >= 10) { //10手に到達したら、改行してcountを0に戻す
          out.println("")
          kaigyoCount = 0
        }
      })

      if (kaigyoCount != 0) out.println("")
      out.println("")
    }

    out.close
    kaigyoCount = 0
  }

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
    case object Gyoku extends ClickedKomaState("玉")
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
    case object Yuu extends ClickedKomaState("有")
    case object Yuri extends ClickedKomaState("利")
    case object YuSeiYu extends ClickedKomaState("優")
    case object Sei extends ClickedKomaState("勢")
    case object GoKakuGo extends ClickedKomaState("互")
    case object GoKaKuKaKu extends ClickedKomaState("角")

    case object Ban extends ClickedKomaState("番")
    case object Ni extends ClickedKomaState("二")
    case object DisplayOu extends ClickedKomaState("王")
    case object De extends ClickedKomaState("で")
    case object Su extends ClickedKomaState("す")

    case object Matta extends ClickedKomaState("待った")

    case object Normal extends ClickedKomaState("通常"+ "\n" +"配置")
    case object Original extends ClickedKomaState("独自"+ "\n" +"配置")
    case object KifuOutPut extends ClickedKomaState("棋譜"+ "\n" +"出力")

    case object Nari extends ClickedKomaState("成り")
    case object FuNari extends ClickedKomaState("不成")

    case object Tumi extends ClickedKomaState("詰")
    case object Mi extends ClickedKomaState("み")

    case object TouRyo extends ClickedKomaState("投了")
    case object Tou extends ClickedKomaState("投")
    case object Ryo extends ClickedKomaState("了")

    case object One extends ClickedKomaState("１枚")
    case object Two extends ClickedKomaState("２枚")
    case object Three extends ClickedKomaState("３枚")
    case object Four extends ClickedKomaState("４枚")
    case object Five extends ClickedKomaState("５枚")
    case object Six extends ClickedKomaState("６枚")
    case object Seven extends ClickedKomaState("７枚")
    case object Eight extends ClickedKomaState("８枚")
    case object Nine extends ClickedKomaState("９枚")
    case object Ten extends ClickedKomaState("１０枚")
    case object Eleven extends ClickedKomaState("１１枚")
    case object Twelve extends ClickedKomaState("１２枚")
    case object Thirteen extends ClickedKomaState("１３枚")
    case object Fourteen extends ClickedKomaState("１４枚")
    case object Fifteen extends ClickedKomaState("１５枚")
    case object Sixteen extends ClickedKomaState("１６枚")
    case object Seventeen extends ClickedKomaState("１７枚")
    case object Eighteen extends ClickedKomaState("１８枚")
    case object Blank extends ClickedKomaState("")

    lazy val values = Seq(None, Fu, Kyo, Kei, Gin, Kin, Ou, Kaku, Hisha, To, NariKyo, NariKei, NariGin, Uma, Ryu, Sen, Go, Te, No, Ka, Chi)
  }

  /** 描画定義 */
  def inBord(index: Int) = index <= 80 && index >= 0
  def outOfBord(index: Int) = index >= 81 && index <= 134 //持ち駒,テンプレート描画の場所
  def inHand(index: Int) = index >= 81 && index <= 134 && !displayPlace(index)

  def displayPlace(index: Int) = index == 81 || index == 87 || index == 93 || index == 99 ||
    index == 105 || index == 106 || index == 107 || index == 108 || index == 109 || index == 110 ||
    index == 111 || index == 117 || index == 123 || index == 129
  def buttomPlace(index: Int) = index == 106 || index == 107 || index == 108 || index == 109 || index == 110
  def statusPlace(index: Int) = index == 81 || index == 87 || index == 93 || index == 99
  def evaluationPlace(index: Int) = index == 111 || index == 117 || index == 123 || index == 129
  def handDisplayPlace(index: Int) = index == 94 || index == 95 || index == 96 || index == 97 || index == 98 || index == 82 || index == 83 || index == 84 ||
    index == 118 || index == 119 || index == 120 || index == 121 || index == 122 || index == 130 || index == 131 || index == 132

  def isNariKoma(index: Int) = {
    board.findPlaceKomaKind(index) == ClickedKomaState.To || board.findPlaceKomaKind(index) == ClickedKomaState.NariKyo ||
    board.findPlaceKomaKind(index) == ClickedKomaState.NariKei || board.findPlaceKomaKind(index) == ClickedKomaState.NariGin ||
    board.findPlaceKomaKind(index) == ClickedKomaState.Uma || board.findPlaceKomaKind(index) == ClickedKomaState.Ryu
  }

  //Sceneクラスをインスタンス化したもの
  val boardScene = new Scene {
    fill = Transparent
    content = boardObjPane
  }

  /* `stage` は、trait JFXAppの中で、`null` で定義されてる */
  stage = new JFXApp.PrimaryStage {
    title.value = "kato-shogi-app!"
    scene = boardScene
    maximized = true
  }

  var isCheckmate: Option[Boolean] = None
  var isOuCatch: Option[Boolean] = None
  var selectHand: Cursor = Cursor.Default
  var (firstKifuMention, firstWinMention) = (true, true)

  def repaint: Unit = {
    boardScene.content = boardObjPane
  }

  def boardObjPane = {
    val pane = new GridPane

    //盤面の表示
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

    //カーソルの選択
    pane.cursor = (selectHand)

    //投了確認ボタン
    if (toryoPushed) {
      val alert = new Alert(AlertType.Confirmation) {
        initOwner(stage) //scene, modality
        title = "投了の確認です"
        headerText = "投了しますか?"
        contentText = "投了した場合、この対局を再開することはできません"
      }
      val result = alert.showAndWait()

      result match {
        case Some(ButtonType.OK) => {
          toryoPushed = false
          isToryo = true
          isWin = true
          if (isCanNari) kifu = kifu.drop(4)
          isCanNari = false
        }
        case _ => toryoPushed = false
      }
      boardSwitch
    }

    pane
  }

  def komaObjGroup(koma: Koma): Group = {
    val buttomKomas: Boolean = koma.kind == ClickedKomaState.Matta || koma.kind == ClickedKomaState.TouRyo ||
      koma.kind == ClickedKomaState.Normal || koma.kind == ClickedKomaState.Original || koma.kind == ClickedKomaState.KifuOutPut ||
      koma.kind == ClickedKomaState.Nari || koma.kind == ClickedKomaState.FuNari

    val komaShape = { //駒の形を定義している
    val poly = {
      if (buttomKomas) {
        Polygon(76, 6, 76, 76, 6, 76, 6, 6) //Polygon(20, 5, 60, 5, 75, 20, 75, 60, 60, 75, 20, 75, 5, 60, 5, 20)
      } else if (handDisplayPlace(koma.index)) {
        Polygon()
      } else {
        koma.isSente match {
          case true => Polygon(41, 11, 61, 21, 71, 71, 11, 71, 21, 21)
          case false => Polygon(41, 71, 21, 61, 11, 11, 71, 11, 61, 61)
        }
      }
    }
      if (buttomPlace(koma.index)) {
        poly.setFill(Cyan)
        poly.setStroke(LightBlue)
      } else if (statusPlace(koma.index)) {
        poly.setFill(LawnGreen) //GreenYellow,LawnGreen,SpringGreen
        poly.setStroke(LightGreen) //LightGreen
      } else if (evaluationPlace(koma.index)) {
        poly.setFill(HotPink) //Salmon
        poly.setStroke(Red)
      } else {
        poly.setFill(Sienna)
        poly.setStroke(Black)
      }
      poly
    }

    val komaLabel = {
    val label = new Label
      label.setText(koma.kind.name)
      if (!koma.isSente && !(displayPlace(koma.index) || handDisplayPlace(koma.index))) label.setRotate(180)
      if (koma.kind == ClickedKomaState.Ten || koma.kind == ClickedKomaState.Eleven || koma.kind == ClickedKomaState.Twelve || koma.kind == ClickedKomaState.Thirteen
        || koma.kind == ClickedKomaState.Fourteen || koma.kind == ClickedKomaState.Fifteen || koma.kind == ClickedKomaState.Sixteen
        || koma.kind == ClickedKomaState.Seventeen || koma.kind == ClickedKomaState.Eighteen) {
        label.setFont(Font.font(22))
        label.setMaxSize(75, 75)
        label.setLayoutX(4.3)
        label.setLayoutY(29)
      } else if (koma.kind == ClickedKomaState.Two || koma.kind == ClickedKomaState.Three || koma.kind == ClickedKomaState.Four || koma.kind == ClickedKomaState.Five
        || koma.kind == ClickedKomaState.Six || koma.kind == ClickedKomaState.Seven || koma.kind == ClickedKomaState.Eight || koma.kind == ClickedKomaState.Nine) {
        label.setFont(Font.font(25))
        label.setMaxSize(50, 50)
        label.setLayoutX(14)
        label.setLayoutY(28.5)
      } else if (koma.kind == ClickedKomaState.TouRyo || koma.kind == ClickedKomaState.Nari || koma.kind == ClickedKomaState.FuNari) {
        label.setFont(Font.font(25))
        label.setTextFill(Color.AliceBlue)
        label.setMaxSize(50, 50)
        label.setLayoutX(16)
        label.setLayoutY(27)
      } else if (koma.kind == ClickedKomaState.Normal || koma.kind == ClickedKomaState.Original || koma.kind == ClickedKomaState.KifuOutPut) {
        label.setFont(Font(25))
        label.setTextFill(Color.AliceBlue)
        label.setMaxSize(62, 62)
        label.setLayoutX(16)
        label.setLayoutY(13.5)
      } else if (koma.kind == ClickedKomaState.Matta) {
        label.setFont(Font.font(22))
        label.setTextFill(Color.AliceBlue)
        label.setMaxSize(75, 75)
        label.setLayoutX(8)
        label.setLayoutY(27.5)
      }
      else if (displayPlace(koma.index)) {
        label.setTextFill(Color.AliceBlue)
        label.setFont(Font(30))
        label.setMaxSize(30, 30)
        label.setLayoutX(25)
        if (koma.isSente) label.setLayoutY(25)
        else label.setLayoutY(21)
      }
      else {
        label.setFont(Font(30))
        label.setMaxSize(30, 30)
        label.setLayoutX(25)
        if (koma.isSente) label.setLayoutY(25)
        else label.setLayoutY(21)
        if (isNariKoma(koma.index)) label.setTextFill(Color.Crimson)
      }

      label.setAlignment(Pos.Center)
      label
    }
    val obj = new Group(komaShape, komaLabel) //駒の形、置き場所のセット
    obj
  }

  var isSenteTurnState: Boolean = true
  var (isWin, isCanNari, isNifu) = (false, false, false)
  var enemyOuTakeKomaStock: List[Int] = Nil
  var ouTookKomaStock: List[Int] = Nil

  var toryoPushed = false
  var admitWait = true

  def boardSwitch :Board = {
    val onBoardKomas: List[Koma] = board match { case Board(komas) => komas.takeRight(initialLength) }
    val pastKomas: List[Koma] = pastBoard match { case Board(komas) => komas.takeRight(initialLength) }
    def isInitialBoard = Board(onBoardKomas) == initialBoard

    /** 待ったのボタン更新 */
    board = if (onBoardKomas != pastKomas && !isToryo && admitWait) { //
      Board(Koma(ClickedKomaState.Matta, 106, isSenteTurnState, displayKoma) :: onBoardKomas)
    } else Board(onBoardKomas)

    /** 棋譜出力ボタンの更新 */
    val mattaKomas: List[Koma] = board match { case Board(komas) => komas }
    board = if (isWin && firstKifuMention) {
      Board(Koma(ClickedKomaState.KifuOutPut, 107, displaySenteKoma, displayKoma) :: mattaKomas)
    } else Board(mattaKomas)

    /** 初期化ボタンと投了ボタンの更新 */
    val isInitial = kifu.length <= 3
    val kifuOutPutKomas: List[Koma] = board match { case Board(komas) => komas }

    board = if (isInitialBoard) {
      Board(Koma(ClickedKomaState.Original, 108, displaySenteKoma, displayKoma) :: kifuOutPutKomas)
    } else if (isWin || isInitial) {
      Board(Koma(ClickedKomaState.Original, 108, displaySenteKoma, displayKoma) ::
        Koma(ClickedKomaState.Normal, 109, displaySenteKoma, displayKoma) :: kifuOutPutKomas)
    } else { //投ボタンを出す
      Board(Koma(ClickedKomaState.TouRyo, 107, isSenteTurnState, displayKoma) :: kifuOutPutKomas)
    }

    /** 成り不成ボタンの追加 */
    val toryoKomas: List[Koma] = board match { case Board(komas) => komas }
    board = isCanNari match {
      case true => {
        Board(Koma(ClickedKomaState.Nari, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.FuNari, 110, isSenteTurnState, displayKoma) ::
            toryoKomas)
      }
      case false => Board(toryoKomas)
    }

    /** 局面評価の表示 */
    val evaluationKomas: List[Koma] = board match { case Board(komas) => komas }
    board = if (!isWin) {
      if (board.evaluationFunction >= 500) { //先手優勢
        Board(Koma(ClickedKomaState.Sen, 111, displaySenteKoma, displayKoma) :: Koma(ClickedKomaState.Te, 117, displaySenteKoma, displayKoma) ::
          Koma(ClickedKomaState.YuSeiYu, 123, displaySenteKoma, displayKoma) :: Koma(ClickedKomaState.Sei, 129, displaySenteKoma, displayKoma) :: evaluationKomas)
      } else if (board.evaluationFunction >= 250) { //先手有利
        Board(Koma(ClickedKomaState.Sen, 111, displaySenteKoma, displayKoma) :: Koma(ClickedKomaState.Te, 117, displaySenteKoma, displayKoma) ::
          Koma(ClickedKomaState.Yuu, 123, displaySenteKoma, displayKoma) :: Koma(ClickedKomaState.Yuri, 129, displaySenteKoma, displayKoma) :: evaluationKomas)
      } else if (board.evaluationFunction <= -500) { //後手優勢
        Board(Koma(ClickedKomaState.Go, 111, displayGoteKoma, displayKoma) :: Koma(ClickedKomaState.Te, 117, displayGoteKoma, displayKoma) ::
          Koma(ClickedKomaState.YuSeiYu, 123, displayGoteKoma, displayKoma) :: Koma(ClickedKomaState.Sei, 129, displayGoteKoma, displayKoma) :: evaluationKomas)
      } else if (board.evaluationFunction <= -250) { //後手有利
        Board(Koma(ClickedKomaState.Go, 111, displayGoteKoma, displayKoma) :: Koma(ClickedKomaState.Te, 117, displayGoteKoma, displayKoma) ::
          Koma(ClickedKomaState.Yuu, 123, displayGoteKoma, displayKoma) :: Koma(ClickedKomaState.Yuri, 129, displayGoteKoma, displayKoma) :: evaluationKomas)
      }
      else { //互角
        Board(Koma(ClickedKomaState.GoKakuGo, 117, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.GoKaKuKaKu, 123, isSenteTurnState, displayKoma)
          :: evaluationKomas)
      }
    } else Board(evaluationKomas)

    /** 状態説明系のテンプレートの更新 */
    val realKomas: List[Koma] = board match { case Board(komas) => komas }
    val displayOute: Boolean = ouTookKomaStock.nonEmpty && !isWin

    board = (isNifu, displayOute, isSenteTurnState) match { //1手指すと出てくる
      case (true, _, _) => {
        Board(Koma(ClickedKomaState.Ni, 81, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Fu, 87, isSenteTurnState, displayKoma) ::
          Koma(ClickedKomaState.De, 93, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Su, 99, isSenteTurnState, displayKoma) :: realKomas) //二歩です
      }
      case (false, true, _) => {
        Board(Koma(ClickedKomaState.DisplayOu, 81, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 87, isSenteTurnState, displayKoma) ::
          Koma(ClickedKomaState.De, 93, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Su, 99, isSenteTurnState, displayKoma) :: realKomas) //王手です
      }
      case (false, false, true) => {
        Board(Koma(ClickedKomaState.Sen, 87, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 93, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ban, 99, isSenteTurnState, displayKoma) ::
          realKomas) //先手番
      }
      case (false, false, false) => {
        Board(Koma(ClickedKomaState.Go, 87, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 93, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ban, 99, isSenteTurnState, displayKoma) ::
          realKomas) //後手番
      }
      case _ => Board(realKomas)
    }

    /** 勝敗系のテンプレートの更新 */
    val secondKomas: List[Koma] = board match { case Board(komas) => komas }

    board = (isOuCatch, isCheckmate, isToryo) match {
      case (Some(true), _, _) => isSenteTurnState match { //isOuCatch
        case true => { //先手番 => 先手勝ち
          Board(Koma(ClickedKomaState.Sen, 111, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 117, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Ka, 123, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 129, isSenteTurnState, displayKoma) :: //先手勝ち
            secondKomas)
        }
        case false => { //後手番 => 後手勝ち
          Board(Koma(ClickedKomaState.Go, 111, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 117, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Ka, 123, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 129, isSenteTurnState, displayKoma) :: //後手勝ち
            secondKomas)
        }
      }
      case (_, Some(true), _) => isSenteTurnState match { //isCheckmate
        case true => { //先手番 => 後手勝ち
          Board(Koma(ClickedKomaState.Go, 111, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 117, !isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Ka, 123, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 129, !isSenteTurnState, displayKoma) :: //後手勝ち
            Koma(ClickedKomaState.Sen, 81, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 87, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Tumi, 93, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Mi, 99, isSenteTurnState, displayKoma) :: //詰み
            secondKomas)
        }
        case false => { //後手番 => 先手勝ち
          Board(Koma(ClickedKomaState.Sen, 111, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 117, !isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Ka, 123, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 129, !isSenteTurnState, displayKoma) :: //先手勝ち
            Koma(ClickedKomaState.Go, 81, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 87, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Tumi, 93, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Mi, 99, isSenteTurnState, displayKoma) ::
            secondKomas)
        }
      }
      case (_, _, true) => isSenteTurnState match { //isToryo
        case true => { //先手番 => 後手勝ち
          Board(Koma(ClickedKomaState.Go, 111, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 117, !isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Ka, 123, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 129, !isSenteTurnState, displayKoma) :: //後手の勝ち
            Koma(ClickedKomaState.Sen, 81, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 87, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Tou, 93, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ryo, 99, isSenteTurnState, displayKoma) :: //投了
            secondKomas)
        }
        case false => { //後手番 => 先手勝ち
          Board(Koma(ClickedKomaState.Sen, 111, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 117, !isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Ka, 123, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 129, !isSenteTurnState, displayKoma) :: //先手の勝ち
            Koma(ClickedKomaState.Go, 81, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 87, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.Tou, 93, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ryo, 99, isSenteTurnState, displayKoma) :: //投了
            secondKomas)
        }
      }
      case _ => board
    }

    /** 持ち駒の枚数の更新 */
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
      (Koma(handOverlap(100), 94, displayGoteKoma, displayKoma) :: Koma(handOverlap(101), 95, displayGoteKoma, displayKoma) :: Koma(handOverlap(102), 96, displayGoteKoma, displayKoma) ::
        Koma(handOverlap(103), 97, displayGoteKoma, displayKoma) :: Koma(handOverlap(104), 98, displayGoteKoma, displayKoma) ::
        Koma(handOverlap(88), 82, displayGoteKoma, displayKoma) :: Koma(handOverlap(89), 83, displayGoteKoma, displayKoma) :: Koma(handOverlap(90), 84, displayGoteKoma, displayKoma) ::
        Koma(handOverlap(112), 118, displaySenteKoma, displayKoma) :: Koma(handOverlap(113), 119, displaySenteKoma, displayKoma) :: Koma(handOverlap(114), 120, displaySenteKoma, displayKoma) ::
        Koma(handOverlap(115), 121, displaySenteKoma, displayKoma) :: Koma(handOverlap(116), 122, displaySenteKoma, displayKoma) ::
        Koma(handOverlap(124), 130, displaySenteKoma, displayKoma) :: Koma(handOverlap(125), 131, displaySenteKoma, displayKoma) :: Koma(handOverlap(126), 132, displaySenteKoma, displayKoma) ::
        lastKomas)
    }
    val outputKomas = BlankKomas.filterNot(koma => koma.kind == ClickedKomaState.Blank)
    board = Board(outputKomas)

    repaint
    board
  }

  /** セルの描画処理, ゲーム内での駒の動きはここで定義している */

  /** cellObjGroup内で使われるStateを定義 */
  var selectedCellIndex: Int = -100
  var stockNariIndex: Option[Int] = None
  var optIsSenteKomaState: Option[Boolean] = None
  var optOnBoardKomaState: Option[Boolean] = None
  var utu = false

  def cellObjGroup(komaOpt: Option[Koma], clickedIndex: Int): Group = {

    /** 以下、駒をクリックした時に使う関数のまとまり */
    val optClickedKomaKind: Option[ClickedKomaState] = komaOpt.map(koma => koma.kind)
    val optIsSenteKoma: Option[Boolean] = komaOpt.map(koma => koma.isSente)
    val optOnBoard: Option[Boolean] = komaOpt.map(koma => koma.onBoard)

    /** 駒に応じて、動けるかどうかを判定する関数 */
    def canMove(koma: ClickedKomaState): Boolean = {
      koma match {
        case ClickedKomaState.Fu => board.fuCanMove(selectedCellIndex, clickedIndex, isSenteTurnState) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notCrossOnBoard(selectedCellIndex, clickedIndex)
        case ClickedKomaState.Kyo => board.kyoCanMove(selectedCellIndex, clickedIndex, isSenteTurnState) && board.upJumpCheck(selectedCellIndex, clickedIndex) && board.downJumpCheck(selectedCellIndex, clickedIndex) &&
          board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notCrossOnBoard(selectedCellIndex, clickedIndex)
        case ClickedKomaState.Kei => board.keiCanMove(selectedCellIndex, clickedIndex, isSenteTurnState) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notCrossOnBoard(selectedCellIndex, clickedIndex)
        case ClickedKomaState.Gin => board.ginCanMove(selectedCellIndex, clickedIndex, isSenteTurnState) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notCrossOnBoard(selectedCellIndex, clickedIndex)
        case ClickedKomaState.Kin => board.kinCanMove(selectedCellIndex, clickedIndex, isSenteTurnState) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notCrossOnBoard(selectedCellIndex, clickedIndex)
        case ClickedKomaState.Ou => board.ouCanMove(selectedCellIndex, clickedIndex, isSenteTurnState) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notCrossOnBoard(selectedCellIndex, clickedIndex)
        case ClickedKomaState.Gyoku => board.ouCanMove(selectedCellIndex, clickedIndex, isSenteTurnState) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notCrossOnBoard(selectedCellIndex, clickedIndex)
        case ClickedKomaState.Kaku => (((board.leftUpRightDownMove(selectedCellIndex, clickedIndex) && board.leftUpJumpCheck(selectedCellIndex, clickedIndex) && board.rightDownJumpCheck(selectedCellIndex, clickedIndex)) //左上から右下方向
          || (board.rightUpLeftDownMove(selectedCellIndex, clickedIndex) && board.rightUpJumpCheck(selectedCellIndex, clickedIndex) && board.leftDownJumpCheck(selectedCellIndex, clickedIndex))) //右上から左下方向
          && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notOwn(selectedCellIndex, clickedIndex))
        case ClickedKomaState.Hisha => (((board.upDownMove(selectedCellIndex, clickedIndex) && board.upJumpCheck(selectedCellIndex, clickedIndex) && board.downJumpCheck(selectedCellIndex, clickedIndex)) //縦(上下)方向
          || (board.leftRightMove(selectedCellIndex, clickedIndex) && board.rightJumpCheck(selectedCellIndex, clickedIndex) && board.leftJumpCheck(selectedCellIndex, clickedIndex))) //横方向
          && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notOwn(selectedCellIndex, clickedIndex))
        case ClickedKomaState.To => board.nariKinCanMoves(selectedCellIndex, clickedIndex, isSenteTurnState) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notCrossOnBoard(selectedCellIndex, clickedIndex)
        case ClickedKomaState.NariKyo => board.nariKinCanMoves(selectedCellIndex, clickedIndex, isSenteTurnState) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notCrossOnBoard(selectedCellIndex, clickedIndex)
        case ClickedKomaState.NariKei => board.nariKinCanMoves(selectedCellIndex, clickedIndex, isSenteTurnState) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notCrossOnBoard(selectedCellIndex, clickedIndex)
        case ClickedKomaState.NariGin => board.nariKinCanMoves(selectedCellIndex, clickedIndex, isSenteTurnState) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notCrossOnBoard(selectedCellIndex, clickedIndex)
        case ClickedKomaState.Uma => ((board.leftUpRightDownMove(selectedCellIndex, clickedIndex) && board.leftUpJumpCheck(selectedCellIndex, clickedIndex) && board.rightDownJumpCheck(selectedCellIndex, clickedIndex))
          || (board.rightUpLeftDownMove(selectedCellIndex, clickedIndex) && board.rightUpJumpCheck(selectedCellIndex, clickedIndex) && board.leftDownJumpCheck(selectedCellIndex, clickedIndex))
          || board.umaMove(selectedCellIndex, clickedIndex)) && board.notCrossOnBoard(selectedCellIndex, clickedIndex) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notOwn(selectedCellIndex, clickedIndex)
        case ClickedKomaState.Ryu => ((board.upDownMove(selectedCellIndex, clickedIndex) && board.upJumpCheck(selectedCellIndex, clickedIndex) && board.downJumpCheck(selectedCellIndex, clickedIndex)) //縦(上下)方向
          || (board.leftRightMove(selectedCellIndex, clickedIndex) && board.rightJumpCheck(selectedCellIndex, clickedIndex) && board.leftJumpCheck(selectedCellIndex, clickedIndex)) //横方向
          || board.ryuMove(selectedCellIndex, clickedIndex)) && board.notCrossOnBoard(selectedCellIndex, clickedIndex) && board.fromToMoveBoard(selectedCellIndex, clickedIndex) && board.notOwn(selectedCellIndex, clickedIndex)
        case _ => false
      }
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
    var canNotTyuAi: List[Int] = Nil
    var (canTyuAiFlag: Boolean, notGetBackKoma: Boolean, notGetBackTyuaiKoma: Boolean) = (true, true, true)

    /** def isCheckmateCheck内で使う汎用関数 */
    /** すべての駒を調べた時、そこへ移動できるか調べる関数(from,to引数としたを場所に応じて) */
    /** 盤面内を横切っていないか */

    def canMovePlace(koma: ClickedKomaState, fromIndex: Int, toIndex: Int, isTumasuKoma: Boolean, board: Board): Boolean = {

      def komaCheck(issenteKomaCheck: Boolean) = koma match {
        case ClickedKomaState.Fu => board.fuCanMove(fromIndex, toIndex, issenteKomaCheck) && board.fromToMoveBoard(fromIndex, toIndex) && board.notCrossOnBoard(fromIndex, toIndex)
        case ClickedKomaState.Kyo => board.kyoCanMove(fromIndex, toIndex, issenteKomaCheck) && board.checkMateUpJumpCheck(fromIndex, toIndex, isSenteTurnState) && board.checkMateDownJumpCheck(fromIndex, toIndex, isSenteTurnState) &&
          board.fromToMoveBoard(fromIndex, toIndex) && board.notCrossOnBoard(fromIndex, toIndex)
        case ClickedKomaState.Kei => board.keiCanMove(fromIndex, toIndex, issenteKomaCheck) && board.fromToMoveBoard(fromIndex, toIndex) && board.notCrossOnBoard(fromIndex, toIndex)
        case ClickedKomaState.Gin => board.ginCanMove(fromIndex, toIndex, issenteKomaCheck) && board.fromToMoveBoard(fromIndex, toIndex) && board.notCrossOnBoard(fromIndex, toIndex)
        case ClickedKomaState.Kin => board.kinCanMove(fromIndex, toIndex, issenteKomaCheck) && board.fromToMoveBoard(fromIndex, toIndex) && board.notCrossOnBoard(fromIndex, toIndex)
        case ClickedKomaState.Ou => board.ouCanMove(fromIndex, toIndex, issenteKomaCheck) && board.fromToMoveBoard(fromIndex, toIndex) && board.notCrossOnBoard(fromIndex, toIndex)
        case ClickedKomaState.Gyoku => board.ouCanMove(fromIndex, toIndex, issenteKomaCheck) && board.fromToMoveBoard(fromIndex, toIndex) && board.notCrossOnBoard(fromIndex, toIndex)
        case ClickedKomaState.Kaku => (((board.leftUpRightDownMove(fromIndex, toIndex) && board.checkMateLeftUpJumpCheck(fromIndex, toIndex, isSenteTurnState) && board.checkMateRightDownJumpCheck(fromIndex, toIndex, isSenteTurnState)) //左上から右下方向
          || (board.rightUpLeftDownMove(fromIndex, toIndex) && board.checkMateRightUpJumpCheck(fromIndex, toIndex, isSenteTurnState) && board.checkMateLeftDownJumpCheck(fromIndex, toIndex, isSenteTurnState))) //右上から左下方向
          && board.fromToMoveBoard(fromIndex, toIndex) && board.notOwn(fromIndex, toIndex))
        case ClickedKomaState.Hisha => (((board.upDownMove(fromIndex, toIndex) && board.checkMateUpJumpCheck(fromIndex, toIndex, isSenteTurnState) && board.checkMateDownJumpCheck(fromIndex, toIndex, isSenteTurnState)) //縦(上下)方向
          || (board.leftRightMove(fromIndex, toIndex) && board.checkMateRightJumpCheck(fromIndex, toIndex, isSenteTurnState) && board.checkMateLeftJumpCheck(fromIndex, toIndex, isSenteTurnState))) //横方向
          && board.fromToMoveBoard(fromIndex, toIndex) && board.notOwn(fromIndex, toIndex))
        case ClickedKomaState.To => board.nariKinCanMoves(fromIndex, toIndex, issenteKomaCheck) && board.fromToMoveBoard(fromIndex, toIndex) && board.notCrossOnBoard(fromIndex, toIndex)
        case ClickedKomaState.NariKyo => board.nariKinCanMoves(fromIndex, toIndex, issenteKomaCheck) && board.fromToMoveBoard(fromIndex, toIndex) && board.notCrossOnBoard(fromIndex, toIndex)
        case ClickedKomaState.NariKei => board.nariKinCanMoves(fromIndex, toIndex, issenteKomaCheck) && board.fromToMoveBoard(fromIndex, toIndex) && board.notCrossOnBoard(fromIndex, toIndex)
        case ClickedKomaState.NariGin => board.nariKinCanMoves(fromIndex, toIndex, issenteKomaCheck) && board.fromToMoveBoard(fromIndex, toIndex) && board.notCrossOnBoard(fromIndex, toIndex)
        case ClickedKomaState.Uma => ((board.leftUpRightDownMove(fromIndex, toIndex) && board.checkMateLeftUpJumpCheck(fromIndex, toIndex, isSenteTurnState) && board.checkMateRightDownJumpCheck(fromIndex, toIndex, isSenteTurnState))
          || (board.rightUpLeftDownMove(fromIndex, toIndex) && board.checkMateRightUpJumpCheck(fromIndex, toIndex, isSenteTurnState) && board.checkMateLeftDownJumpCheck(fromIndex, toIndex, isSenteTurnState))
          || board.umaMove(fromIndex, toIndex)) && board.notCrossOnBoard(fromIndex, toIndex) && board.fromToMoveBoard(fromIndex, toIndex) && board.notOwn(fromIndex, toIndex)
        case ClickedKomaState.Ryu => ((board.upDownMove(fromIndex, toIndex) && board.checkMateUpJumpCheck(fromIndex, toIndex, isSenteTurnState) && board.checkMateDownJumpCheck(fromIndex, toIndex, isSenteTurnState)) //縦(上下)方向
          || (board.leftRightMove(fromIndex, toIndex) && board.checkMateRightJumpCheck(fromIndex, toIndex, isSenteTurnState) && board.checkMateLeftJumpCheck(fromIndex, toIndex, isSenteTurnState)) //横方向
          || board.ryuMove(fromIndex, toIndex)) && board.notCrossOnBoard(fromIndex, toIndex) && board.fromToMoveBoard(fromIndex, toIndex) && board.notOwn(fromIndex, toIndex)
        case _ => false
      }

      val (senteKomaCheck, goteKomaCheck) = (true, false)
      isTumasuKoma match {
        case true => {
          if (isSenteTurnState && isThereSenteKoma(fromIndex).contains(!isSenteTurnState)) komaCheck(goteKomaCheck) //先手のときは後手の駒を調べる
          else if (!isSenteTurnState && (isThereSenteKoma(fromIndex).contains(!isSenteTurnState))) komaCheck(senteKomaCheck) //後手のときは先手の駒を調べる
          else false
        }
        case false => {
          if (isSenteTurnState && isThereSenteKoma(fromIndex).contains(isSenteTurnState)) komaCheck(senteKomaCheck) //先手のときは先手の駒を調べる
          else if (!isSenteTurnState && (isThereSenteKoma(fromIndex).contains(isSenteTurnState))) komaCheck(goteKomaCheck) //後手のときは後手の駒を調べる
          else false
        }
      }
    }

    val (enemySideKoma, ownSideKoma) = (true, false)
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

    /** 以下、王が詰んでいるかのチェック関数 */
    def isCheckmateCheck: Boolean = {
      val ownOuIndex: Int = board.findOuGyoku(isSenteTurnState) //自分の王の位置
      val enemyOuIndex: Int = board.findOuGyoku(!isSenteTurnState) //敵の王の位置

      /** getBackWithoutOuPattern */
      //toIndexに王以外の駒が動けるかどうかを調べることで、相手の駒を取り返せるかどうかを調べるための関数
      def canNotGetBack(toIndex: Int): Boolean = {
        for (fromIndex <- 0 to 80) {
          if (canTakePlace(fromIndex, toIndex, ownSideKoma, board)) { //詰まされる側の駒で、王を取ろうとしている駒を取れるか
            //王以外の駒で取る場合、取れる駒を動かしたboard上で効きがない場合に、falseとなる
            if (!(isThereKomaKind(fromIndex).contains(ClickedKomaState.Ou) || isThereKomaKind(fromIndex).contains(ClickedKomaState.Gyoku))) {
              var afterMoveStock: List[Int] = Nil
              for (reCheckfromIndex <- 0 to 80) {
                if (canTakePlace(reCheckfromIndex, ownOuIndex, enemySideKoma, board.moveKoma(fromIndex, toIndex))) {
                  //詰ます側が詰まされる王の場所に効きがある(つまり取れる)駒がどこにいるのか
                  afterMoveStock = reCheckfromIndex :: afterMoveStock
                }
              }
              if (afterMoveStock.length <= 1) notGetBackKoma = false //駒の効きの数が変わらなければ詰みを回避している(実際には駒を取っていない)
            }
            //王で取る場合、移動先で王を取ることができない(取った駒に紐が付いていない)場合、falseになる
            if (isThereKomaKind(fromIndex).contains(ClickedKomaState.Ou) || isThereKomaKind(fromIndex).contains(ClickedKomaState.Gyoku)) {
              var ouAfterMoveStock: List[Int] = Nil
              for (reCheckfromIndex <- 0 to 80) {
                if (canTakePlace(reCheckfromIndex, toIndex, enemySideKoma, board.moveKoma(ownOuIndex, toIndex))) { //王はownOuIndexからtoIndexに移動
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
          if (canTakePlace(fromIndex, toIndex, ownSideKoma, board) &&
            (!(isThereKomaKind(fromIndex).contains(ClickedKomaState.Ou) || isThereKomaKind(fromIndex).contains(ClickedKomaState.Gyoku)))) {
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
              if (canTakePlace(reCheckfromIndex, ownOuIndex, enemySideKoma, board.moveKoma(fromIndex, index))) { //詰ます側が詰まされる王の場所に効きがある(つまり取れる)駒がどこにいるのか
                afterMoveStock = reCheckfromIndex :: afterMoveStock
              }
            }
            if (afterMoveStock.isEmpty) boardTyuai = true //動いた結果、駒の効きが無くなった場合は、中合いに成功している(true)
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
      }.filter(index => inBord(index))

      /** 詰み、勝ちの条件判定 */
      //王の場所に効きがある(つまり取れる)駒がどこにいるのかをStockしている
      for (fromIndex <- 0 to 80) {
        //詰ます側が詰まされる王の場所に効きがある(つまり取れる)駒がどこにいるのか
        if (canTakePlace(fromIndex, ownOuIndex, enemySideKoma, board)) {
          ouTookKomaStock = fromIndex :: ouTookKomaStock
        }
        //詰まされる側が敵の王を取れるかどうか
        if (canTakePlace(fromIndex, enemyOuIndex, ownSideKoma, board)) {
          enemyOuTakeKomaStock = fromIndex :: enemyOuTakeKomaStock
        }

        //すべての駒が効きのある場所を調べる => 逃げるパターンで使う
        for (toIndex <- 0 to 80) {
          if (!toIndexStock.contains(toIndex)) {
            if (canTakePlace(fromIndex, toIndex, enemySideKoma, board)) {
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

      //中合いが効くパターン(ouTookKomaStock.length==1の場合に使う)
      def TyuaiPattern: Boolean = ouTookKomaStock.length match {
        case 1 => {
          val tyuAiKoma = true
          val tumiCheckKomas: List[Koma] = board match { case Board(komas) => komas }

          /* 持ち駒で中合いできるか判別
          できる => Fuをおく,
          できない => 2.盤上の駒で中合いできるか判別
          できる => Fuをおく, できない => Blankをおく */

          val onAllBoard = (0 to 80).toList
          val blankExcept = onAllBoard.filterNot(i => tyuAiAdd(i) == ClickedKomaState.Blank) //盤上からBlankのものを除外する処理が必要
          var tyuaiAddStock: List[Koma] = tumiCheckKomas

          blankExcept.foreach(i => tyuaiAddStock = Koma(tyuAiAdd(i), i, isSenteTurnState, tyuAiKoma) :: tyuaiAddStock)
          val tyuAiBoard: Board = Board(tyuaiAddStock)

          for (fromIndex <- 0 to 80) {
            if (canTakePlace(fromIndex, ownOuIndex, enemySideKoma, tyuAiBoard)) { //再度王が取られることがないかチェック
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
      def isCheckmateCheckLogic(): Boolean = {
        ouTookKomaStock.length match {
          case 0 => false
          case 1 => ouEscapePattern && TyuaiPattern && getBackWithoutOuPattern && enemyOuTakeKomaStock.isEmpty
          case _ => ouEscapePattern && enemyOuTakeKomaStock.isEmpty
        }
      }
      isCheckmateCheckLogic()
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
    def fromOnBoard: Boolean = selectedCellIndex <= 80
    def fromHandPlace: Boolean = (selectedCellIndex >= 81 && selectedCellIndex <= 134) && (selectedCellIndex - 81) % 6 != 0 && (selectedCellIndex - 81) / 6 != 4
    def toMoveBoard: Boolean = clickedIndex <= 80
    def handPlace: Boolean = (clickedIndex >= 81 && clickedIndex <= 134) && (clickedIndex - 81) % 6 != 0 && (clickedIndex-81) / 6 != 4

    /** "selectedCellIndex == clickedIndex && clickedKomaKind != ClickedKomaState.None" である状態 */
    val isFillLightBulue: Boolean = selectedCellIndex == clickedIndex && clickedKomaKind != ClickedKomaState.None
    val fillColor = if (isFillLightBulue) {
      LightBlue
    }
    else if (canMove(clickedKomaKind) && isNotFriendKoma(clickedIndex)
      && optOnBoardKomaState.contains(true)
    ) AliceBlue
    else if (toMoveBoard || handPlace) Burlywood
    else White

    val grid = {
      val rect = Rectangle(82, 82, fillColor)
      if (toMoveBoard || handPlace) rect.setStroke(Black)
      rect
    }
    val group =  new Group { children = List(Some(grid), komaOpt.map(komaObjGroup)).flatten }

    /** クリック時にどの判定を行うべきか分岐 */
    def useHandKomaBranch: Boolean = {
      isSenteTurnState match {
        case true => (optOnBoard.contains(false) || optOnBoardKomaState.contains(false)) && !optOnBoard.contains(true) &&
          (optIsSenteKoma.contains(true) || optIsSenteKomaState.contains(true)) && isSenteTurnState && !isCanNari && !isWin
        case false => (optOnBoard.contains(false) || optOnBoardKomaState.contains(false)) && !optOnBoard.contains(true) &&
          (optIsSenteKoma.contains(false) || optIsSenteKomaState.contains(false)) && !isSenteTurnState && !isCanNari && !isWin
      }
    }

    def inBoardKomaBranch(koma: ClickedKomaState): Boolean = {
      isSenteTurnState match {
        case true => (optClickedKomaKind.contains(koma) || clickedKomaKind == koma && optClickedKomaKind.isEmpty) &&
          (optIsSenteKoma.contains(true) || optIsSenteKomaState.contains(true)) && isSenteTurnState && !isCanNari && !isWin
        case false => (optClickedKomaKind.contains(koma) || clickedKomaKind == koma && optClickedKomaKind.isEmpty) &&
          (optIsSenteKoma.contains(false) || optIsSenteKomaState.contains(false)) && !isSenteTurnState && !isCanNari && !isWin
      }
    }

    /** 以下棋譜出力の方向 */
    //上から移動してくる
    def fromLeftUp(moveDistance: Int) = moveDistance % 10 == 0 && moveDistance > 0
    def fromUp(moveDistance: Int) = moveDistance % 9 == 0 && moveDistance > 0
    def fromRightUp(moveDistance: Int) = moveDistance % 8 == 0 && moveDistance > 0

    //横から移動してくる
    def fromRight(moveDistance: Int) = clickedIndex / 9 == selectedCellIndex / 9 && moveDistance > 0
    def fromLeft(moveDistance: Int) = clickedIndex / 9 == selectedCellIndex / 9 && moveDistance < 0

    //下から移動してくる
    def fromLeftDown(moveDistance: Int) = moveDistance % 8 == 0 && moveDistance < 0
    def fromDown(moveDistance: Int) = moveDistance % 9 == 0 && moveDistance < 0
    def fromRightDown(moveDistance: Int) = moveDistance % 10 == 0 && moveDistance < 0

    def tryDirectionPrint1(sameKomaMoveDistanceList: List[Int]): List[String] = {
      var directionPrintList1: List[String] = Nil
      sameKomaMoveDistanceList.foreach(sameKomaMoveDistance => {
        val tryDirectionPrint1 = {
          if (fromLeftUp(sameKomaMoveDistance) || fromUp(sameKomaMoveDistance) || fromRightUp(sameKomaMoveDistance)) {
            isSenteTurnState match {
              case true => "引"
              case false => "上"
            }
          }
          else if (fromRight(sameKomaMoveDistance) || fromLeft(sameKomaMoveDistance)) {
            "寄"
          }
          else if (fromLeftDown(sameKomaMoveDistance) || fromRightDown(sameKomaMoveDistance) || fromDown(sameKomaMoveDistance)) {
            isSenteTurnState match {
              case true => "上"
              case false => "引"
            }
          }
          else ""
        }
        directionPrintList1 = tryDirectionPrint1 :: directionPrintList1
      })
      directionPrintList1
    }
    def tryDirectionPrint2(sameKomaMoveDistanceList: List[Int]): List[String] = {
      var directionPrintList2: List[String] = Nil
      sameKomaMoveDistanceList.foreach(sameKomaMoveDistance => {
        val tryDirectionPrint2 = {
          if (fromLeftUp(sameKomaMoveDistance) || fromLeftDown(sameKomaMoveDistance) || fromLeft(sameKomaMoveDistance)) {
            isSenteTurnState match {
              case true => "左"
              case false => "右"
            }
          }
          else if (fromUp(sameKomaMoveDistance)) {
            isSenteTurnState match {
              case true => "引"
              case false => "直"
            }
          }
          else if (fromDown(sameKomaMoveDistance)) {
            isSenteTurnState match {
              case true => "直"
              case false => "引"
            }
          }
          else if (fromRightUp(sameKomaMoveDistance) || fromRightDown(sameKomaMoveDistance) || fromRight(sameKomaMoveDistance)) {
            isSenteTurnState match {
              case true => "右"
              case false => "左"
            }
          }
          else ""
        }
        directionPrintList2 = tryDirectionPrint2 :: directionPrintList2
      })
      directionPrintList2
    }

    def directionPrint1(sameKomaMoveDistanceList: List[Int], moveDistance: Int): String = { //移動する駒を試す
      if (fromLeftUp(moveDistance) || fromUp(moveDistance) || fromRightUp(moveDistance)) {
        isSenteTurnState match {
          case true => "引"
          case false => "上"
        }
      }
      else if (fromRight(moveDistance) || fromLeft(moveDistance)) {
        "寄"
      }
      else if (fromLeftDown(moveDistance) || fromRightDown(moveDistance) || fromDown(moveDistance)) {
        isSenteTurnState match {
          case true => "上"
          case false => "引"
        }
      }
      else ""
    }
    def directionPrint2(sameKomaMoveDistanceList: List[Int], moveDistance: Int): String = {
      if (fromLeftUp(moveDistance) || fromLeftDown(moveDistance) || fromLeft(moveDistance)) {
        isSenteTurnState match {
          case true => "左"
          case false => "右"
        }
      }
      else if (fromUp(moveDistance)) {
        isSenteTurnState match {
          case true => "引"
          case false => "直"
        }
      }
      else if (fromDown(moveDistance)) {
        isSenteTurnState match {
          case true => "直"
          case false => "引"
        }
      }
      else if (fromRightUp(moveDistance) || fromRightDown(moveDistance) || fromRight(moveDistance)) {
        isSenteTurnState match {
          case true => "右"
          case false => "左"
        }
      }
      else ""
    }
    def directionPrint3(sameKomaMoveDistanceList: List[Int], moveDistance: Int): String = {
      if (fromLeftUp(moveDistance)) {
        isSenteTurnState match {
          case true => "左引"
          case false => "右上"
        }
      }
      else if (fromUp(moveDistance)) {
        isSenteTurnState match {
          case true => "引"
          case false => "上"
        }
      }
      else if (fromRightUp(moveDistance)) {
        isSenteTurnState match {
          case true => "右引"
          case false => "左上"
        }
      }
      else if (fromRight(moveDistance)) {
        isSenteTurnState match {
          case true => "右"
          case false => "左"
        }
      }
      else if (fromLeft(moveDistance)) {
        isSenteTurnState match {
          case true => "左"
          case false => "右"
        }
      }
      else if (fromLeftDown(moveDistance)) {
        isSenteTurnState match {
          case true => "左上"
          case false => "右引"
        }
      }
      else if (fromDown(moveDistance)) {
        isSenteTurnState match {
          case true => "上"
          case false => "引"
        }
      }
      else if (fromRightDown(moveDistance)) {
        isSenteTurnState match {
          case true => "右上"
          case false => "左引"
        }
      }
      else ""
    }

    def tyofukuCheck: List[ClickedKomaState] = {
      var stockKoma: List[ClickedKomaState] = Nil
      var stockIndex: List[Int] = Nil
      for (fromIndex <- 0 to 80) { //詰まされる側が敵の王を取れるかどうか
        if (canTakePlace(fromIndex, clickedIndex, ownSideKoma, board)) {
          val koma = board.findPlaceKomaKind(fromIndex)
          stockKoma = koma :: stockKoma
        }
      }
      val tyofukuKoma = stockKoma diff stockKoma.distinct
      tyofukuKoma
    }
    /** ここまで棋譜出力の方向 */

    /** 駒をクリックした場合の分岐 */
    def nariChoiceBranch: Boolean = optClickedKomaKind.contains(ClickedKomaState.Nari)
    def funariChoiceBranch: Boolean = optClickedKomaKind.contains(ClickedKomaState.FuNari)
    def touRyoBranch: Boolean = optClickedKomaKind.contains(ClickedKomaState.TouRyo)

    def initializationBranch = optClickedKomaKind.contains(ClickedKomaState.Normal) || optClickedKomaKind.contains(ClickedKomaState.Original)
    def waitBranch = optClickedKomaKind.contains(ClickedKomaState.Matta)
    def kifuOutPutBranch = optClickedKomaKind.contains(ClickedKomaState.KifuOutPut)

    /** 複数回クリックした時に、駒の情報を保存したり、条件を外したり、条件制御を行う */
    def addState = {
      clickedKomaKind = optClickedKomaKind.getOrElse(ClickedKomaState.None)
      optIsSenteKomaState = optIsSenteKoma
      optOnBoardKomaState = optOnBoard
    }

    //自分の駒である場合に付与というようにしたい
    def fromToBoradAddState(koma: ClickedKomaState) = {
      if (optClickedKomaKind.contains(koma) && optOnBoard.contains(true) && optIsSenteKoma.contains(isSenteTurnState)) addState
    }

    def fromHandToBoradAddState = {
      if (optOnBoard.contains(false) && optIsSenteKoma.contains(isSenteTurnState)) addState
    }

    var firstClickFlag: Boolean = false
    def clickCancel = {
      if (board.notOwn(selectedCellIndex, clickedIndex) && !firstClickFlag) {
        clickedKomaKind = ClickedKomaState.None
        optIsSenteKomaState = None
        optOnBoardKomaState = None
        selectedCellIndex = -100
      }
    }

    /** 実際に手を指し、今までの条件を初期化する */
    def switchTurn(nextTurn: Boolean): Boolean = if (nextTurn) false else true
    def playAndInitialize = {

      //takeKomaでを保存していない場合に, moveKomaする前に保存しておく
      if ((isSenteTurnState && !optIsSenteKoma.contains(false)) || (!isSenteTurnState && !optIsSenteKoma.contains(true))) {
        pastBoard = board
      }

      /** 棋譜内での重複発見のための処理 */
      //仮に効きが重複していた駒が今動いた駒と一致していた場合
      var directionPrint: String = ""
      if (tyofukuCheck.contains(board.findPlaceKomaKind(selectedCellIndex))) {
        val sameKomaIndex = board.filterKomaKind(board.findPlaceKomaKind(selectedCellIndex), isSenteTurnState)
        var sameKomaMoveDistanceList: List[Int] = Nil

        val moveDistance = clickedIndex - selectedCellIndex
        sameKomaIndex.foreach(index => sameKomaMoveDistanceList = (clickedIndex - index) :: sameKomaMoveDistanceList)

        directionPrint = {
          if (tryDirectionPrint1(sameKomaMoveDistanceList).count(_ == directionPrint1(sameKomaMoveDistanceList, moveDistance)) <= 1) {
            directionPrint1(sameKomaMoveDistanceList, moveDistance)
          }
          else if (tryDirectionPrint2(sameKomaMoveDistanceList).count(_ == directionPrint2(sameKomaMoveDistanceList, moveDistance)) <= 1) {
            directionPrint2(sameKomaMoveDistanceList, moveDistance)
          }
          else {
            directionPrint3(sameKomaMoveDistanceList, moveDistance)
          }
        }

      }

      /** 成り不成の処理 */
      if (mustNari) {
        board = board.nariKoma(selectedCellIndex) //強制的に成り、相手の手番へ
        isSenteTurnState = switchTurn(isSenteTurnState)
      }
      else if (canNari) addNariGomaState //どこにいる駒が成れる状態、という状態を付与
      else isSenteTurnState = switchTurn(isSenteTurnState) //成れない場合は相手の手番へ

      /** 実際に駒を動かす処理(本体) */
      board = board.moveKoma(selectedCellIndex, clickedIndex)

      /** 棋譜の出力 */
      val place = clickedIndex
      val movedKoma = {
        //mustNariの場合はここで棋譜に追加、canNariの場合はボタンを選択した時に追加する方式に
        if (mustNari) board.findPlaceKomaKind(clickedIndex).name + directionPrint + "成り"
        else if (utu) board.findPlaceKomaKind(clickedIndex).name + directionPrint + "打"
        else board.findPlaceKomaKind(clickedIndex).name + directionPrint
      }

      val tate = (clickedIndex / 9 + 1).toString
      val yoko = (9 - (clickedIndex % 9)).toString
      kifu = yoko :: tate :: movedKoma :: "("+board.evaluationFunction.toString+")" :: kifu

      if (optOnBoardKomaState.contains(false)) {
        board = board.spaceChangeKoma(clickedIndex, optOnBoard.contains(false)) //打ち終わった駒は盤上の駒になる
      }

      /** 初期化 */
      selectedCellIndex = -100
      clickedKomaKind = ClickedKomaState.None
      optIsSenteKomaState = None
      optOnBoardKomaState = None
      //王を取ろうとしていた駒の情報も初期化
      enemyOuTakeKomaStock = Nil
      ouTookKomaStock = Nil
    }

    /** 駒を取った時の処理と王様が取られた場合の判定 */
    def takeKoma(clickedIndex: Int) = { //駒を取った時に行う処理の集まり
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

      val movePlace: Int  = isSenteTurnState match {  //isSenteTurnStateをひっくり返す前にコマを取っている
        case true => tookKomaOpt match {
          case Some(ClickedKomaState.Fu) => 112
          case Some(ClickedKomaState.Kyo) => 113
          case Some(ClickedKomaState.Kei) => 114
          case Some(ClickedKomaState.Gin) => 115
          case Some(ClickedKomaState.Kin) => 116
          case Some(ClickedKomaState.Kaku) => 124
          case Some(ClickedKomaState.Hisha) => 125
          case Some(ClickedKomaState.Ou) => 126
          case Some(ClickedKomaState.Gyoku) => 126
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
          case Some(ClickedKomaState.Gyoku) => 90
          case _ => 90
        }
      }
      movePlace
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
      stockNariIndex = Some(clickedIndex)
    }
    def initializeNariGomaState = {
      isCanNari = false
      isSenteTurnState = switchTurn(isSenteTurnState)
      stockNariIndex = None
      selectedCellIndex = -100
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
        fromHandPlace && toMoveBoard && optIsSenteKoma.isEmpty &&
          (!((clickedKomaKind == ClickedKomaState.Fu || clickedKomaKind == ClickedKomaState.Kyo) && (clickedIndex / 9) + 1 == 1)) && //先手の歩と香車は、1段目に打てない
          !(clickedKomaKind == ClickedKomaState.Kei && (clickedIndex / 9 + 1) <= 2) && //先手の桂馬は、1段目と2段目に打てない
          (clickedKomaKind != ClickedKomaState.Fu || board.nifuCheck(clickedIndex, optIsSenteKomaState.contains(true))) //二歩ではない
      } else {
        fromHandPlace && toMoveBoard && optIsSenteKoma.isEmpty &&
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
        utu = true
        playAndInitialize
        utu = false
        tumiCheckFlow
      }
      else clickCancel
    }

    /** 盤上から盤上へ移動する駒が行う処理 */
    def inBordKomaMoveFlow(koma: ClickedKomaState) = {
      fromToBoradAddState(koma)
      if (canMove(clickedKomaKind)) {
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
        }
        if (enemyOuTakeKomaStock.nonEmpty) { //手番で、手側側の駒が成ろうとしてる状態を除く
          isWin = true
          isOuCatch = Some(true)
        }
      }
    }

    def initialWinCheck() :Boolean = {
      val ownOuIndex: Int = board.findOuGyoku(isSenteTurnState) //自分の王の位置
      val enemyOuIndex: Int = board.findOuGyoku(!isSenteTurnState) //敵の王の位置
      var isInitialWin: Boolean = false
      for (fromIndex <- 0 to 80) { //詰まされる側が敵の王を取れるかどうか
        if (canTakePlace(fromIndex, enemyOuIndex, ownSideKoma, board)) {
          isInitialWin = true
        } else if (canTakePlace(fromIndex, ownOuIndex, enemySideKoma, board)) {
          isInitialWin = true
        }
      }
      isInitialWin
    }

    def initializationOrWaitFlow = {
      if (initializationBranch) {
        optClickedKomaKind match { //初期化時の駒配置の選択が可能
          case Some(ClickedKomaState.Normal) => board = initialBoard
          case Some(ClickedKomaState.Original) => originalPushed = true
          case _ =>
        }
        var isinitialWin = true

        /** 初期局面確認ボタン */
        val ButtonTypeOne = new ButtonType("ルール１")
        val ButtonTypeTwo = new ButtonType("ルール２")
        val ButtonTypeThree = new ButtonType("ルール３")
        val ButtonTypeFour = new ButtonType("ルール４")
        val ButtonTypeFive = new ButtonType("ルール５")
        val ButtonTypeSix = new ButtonType("ルール６")

        if (originalPushed) {
          originalPushed = false //元に戻す
          val alert = new Alert(AlertType.Confirmation) {
            initOwner(stage)
            title = "独自の6パターンの初期局面からゲームを開始できます"
            headerText = "ルール１〜ルール６の中から初期盤面を選択して下さい"
            contentText =  "\n" + "ルール１では、歩以外の駒が歩の下の駒にランダムに配置されます" + "\n" + "\n" +
              "ルール２では、歩の位置が1つ上がり、歩の下の3段に駒がランダムに配置されます" + "\n" + "\n" +
              "ルール３では、先手は6~9段目、後手は1~4段目に駒がランダムに配置されます" + "\n" + "\n" +
              "ルール４では、斜めに並んだ歩の下に駒が配置されます" + "\n" + "\n" +
              "ルール５では、斜めの位置にランダムに駒が配置されます" + "\n" + "\n" +
              "ルール６では、9×9マス中にランダムに駒が配置されます" + "\n" + "\n"
              buttonTypes = Seq(ButtonTypeOne, ButtonTypeTwo, ButtonTypeThree, ButtonTypeFour, ButtonTypeFive, ButtonTypeSix, ButtonType.Cancel)
          }
          val result = alert.showAndWait()

          result match {
            case Some(ButtonTypeOne) => board = threeUnderFuInitialBoard
            case Some(ButtonTypeTwo) => board = fourUnderFuInitialBoard
            case Some(ButtonTypeThree) => {
              while (isinitialWin) {
                board = halfRandomBoard
                isinitialWin = initialWinCheck()
              }
            }
            case Some(ButtonTypeFour) => {
              while (isinitialWin) {
                board = slashUnderFuInitialBoard
                isinitialWin = initialWinCheck()
              }
            }
            case Some(ButtonTypeFive) => {
              while (isinitialWin) {
                board = slashHalfRandomBoard
                isinitialWin = initialWinCheck()
              }
            }
            case Some(ButtonTypeSix) => {
              while (isinitialWin) {
                board = allRandomBoard
                isinitialWin = initialWinCheck()
              }
            }
            case _ =>
          }
          boardSwitch
        }

        pastBoard = board //待ったはなし
        kifu = List("まで","手で","勝ち")
        admitWait = true
        isSenteTurnState = true
        isToryo = false
      }
      else if (waitBranch) {
        board = pastBoard
        kifu = kifu.drop(4) //待ったをした場合を取り除く
        if (!isCanNari) isSenteTurnState = !isSenteTurnState
      }
      firstWinMention = true
      firstKifuMention = true
      firstClickFlag = false
      clickCancel
      isCanNari = false
      isNifu = false
      isWin = false
      toryoPushed = false
      selectedCellIndex = -100
      stockNariIndex = None
      initializeTumiState
      tumiCheckFlow
    }

    def nariChoiceFlow = {
      board = board.nariKoma(stockNariIndex.getOrElse(-1))
      kifu = kifu match { //kifuListの3番目に不成を追加
        case a :: b :: c :: d :: past => a :: b :: c+"成り" :: d :: past
        case _ => List("")
      }
      initializeNariGomaState //状態を元に戻す
      tumiCheckFlow //成りの状態を加えたらチェック
    }

    def fuNariChoiceFlow = {
      kifu = kifu match { //kifuListの3番目に不成を追加
        case a :: b :: c :: d :: past => a :: b :: c+"不成" :: d :: past
        case _ => List("")
      }
      initializeNariGomaState //状態を元に戻す
      tumiCheckFlow //不成の状態を加えたらチェック
    }

    def touRyoFlow = {
      toryoPushed = true
      clickCancel
    }

    def kifuOutPutFlow = {
      logOutPut
      admitWait = false
      firstKifuMention = false
      clickCancel
    }
    /** ここまで駒をクリックした時に使われる関数群 */


    /** 実際に駒にマウスが乗ってきた場合の処理 */
    val movedIndex = clickedIndex //movedIndexとして扱う
    group.setOnMouseMoved(e => {
      if (isWin || isCanNari) { //勝ちと成り不成の場合に押せるのはボタンのみ
        if (buttomPlace(movedIndex) && board.findPlaceKomaisSente(movedIndex).isDefined) {
          selectHand = Cursor.Hand
        } else {
          selectHand = Cursor.Default
        }
      } else { //普段は
        if ((board.findPlaceKomaisSente(movedIndex).contains(isSenteTurnState) && (inBord(movedIndex) || (inHand(movedIndex) && !handDisplayPlace(movedIndex))) && movedIndex != selectedCellIndex) || //そこに手番側の動かせる駒がある(2度目のクリックの場合は反応しない)
          (canMove(clickedKomaKind) && isNotFriendKoma(clickedIndex) && optOnBoardKomaState.contains(true)) || //盤上の駒選択中
          (optOnBoardKomaState.contains(false) && canSetFromHand) //持ち駒選択中
        ) { //選択した駒が動ける
          selectHand = Cursor.OpenHand
        } else if (buttomPlace(movedIndex) && board.findPlaceKomaisSente(movedIndex).isDefined){ //ボタンの場所でそこに駒がある
          selectHand = Cursor.Hand
        } else {
          selectHand = Cursor.Default
        }
      }
      repaint
    })

    def selectedCellIndexBranch = {
      board.findPlaceKomaisSente(clickedIndex).contains(isSenteTurnState) &&
        !(optOnBoard.contains(true) && outOfBord(clickedIndex)) //盤外かつOnBoardがtrueである駒はダメ
    }

    /** 実際に駒がクリックがされた場合の処理 */
    group.setOnMouseClicked(e => {

      /** 初期化されている場合は、クリックされている座標を代入する */
      firstClickFlag = false
      if (selectedCellIndexBranch) {
        selectedCellIndex = clickedIndex
        firstClickFlag = true
      }

      /** 初期化、待った、投了ボタン、棋譜出力をクリックした場合の処理 */
      if (initializationBranch) initializationOrWaitFlow
      if (touRyoBranch) touRyoFlow
      if (waitBranch) initializationOrWaitFlow
      if (kifuOutPutBranch) kifuOutPutFlow

      /** 駒が成るかどうかの判定をクリックした場合の処理 */
      if (nariChoiceBranch) nariChoiceFlow
      if (funariChoiceBranch) fuNariChoiceFlow

      /** 持ち駒をクリックして盤面に打つ場合の処理 */
      if (useHandKomaBranch) useHandKomaFlow
      /** 盤面の歩を移動させる場合の処理 */
      if (inBoardKomaBranch(ClickedKomaState.Fu)) inBordKomaMoveFlow(ClickedKomaState.Fu)
      /** 香車の場合 */
      if (inBoardKomaBranch(ClickedKomaState.Kyo)) inBordKomaMoveFlow(ClickedKomaState.Kyo)
      /** 桂馬の場合 */
      if (inBoardKomaBranch(ClickedKomaState.Kei)) inBordKomaMoveFlow(ClickedKomaState.Kei)
      /** 銀の場合 */
      if (inBoardKomaBranch(ClickedKomaState.Gin)) inBordKomaMoveFlow(ClickedKomaState.Gin)
      /** 金の場合 */
      if (inBoardKomaBranch(ClickedKomaState.Kin)) inBordKomaMoveFlow(ClickedKomaState.Kin)
      /** 王の場合 */
      if (inBoardKomaBranch(ClickedKomaState.Ou)) inBordKomaMoveFlow(ClickedKomaState.Ou)
      /** 玉の場合 */
      if (inBoardKomaBranch(ClickedKomaState.Gyoku)) inBordKomaMoveFlow(ClickedKomaState.Gyoku)
      /** 角の場合 */
      if (inBoardKomaBranch(ClickedKomaState.Kaku)) inBordKomaMoveFlow(ClickedKomaState.Kaku)
      /** 飛車の場合 */
      if (inBoardKomaBranch(ClickedKomaState.Hisha)) inBordKomaMoveFlow(ClickedKomaState.Hisha)
      /** との場合 */
      if (inBoardKomaBranch(ClickedKomaState.To)) inBordKomaMoveFlow(ClickedKomaState.To)
      /** 成香の場合 */
      if (inBoardKomaBranch(ClickedKomaState.NariKyo)) inBordKomaMoveFlow(ClickedKomaState.NariKyo)
      /** 成桂の場合 */
      if (inBoardKomaBranch(ClickedKomaState.NariKei)) inBordKomaMoveFlow(ClickedKomaState.NariKei)
      /** 成銀の場合 */
      if (inBoardKomaBranch(ClickedKomaState.NariGin)) inBordKomaMoveFlow(ClickedKomaState.NariGin)
      /** 馬の場合 */
      if (inBoardKomaBranch(ClickedKomaState.Uma)) inBordKomaMoveFlow(ClickedKomaState.Uma)
      /** 龍の場合 */
      if (inBoardKomaBranch(ClickedKomaState.Ryu)) inBordKomaMoveFlow(ClickedKomaState.Ryu)

      //デバッグ用
      println("selectedCellIndex:" + selectedCellIndex,"clickedIndex:"+clickedIndex,"stockNariIndex:" + stockNariIndex)
      println("optOnBoard:" + optOnBoard, "optOnBoardKomaState:" + optOnBoardKomaState)
      println("optIsSenteKoma:" + optIsSenteKoma, "optIsSenteKomaState:" + optIsSenteKomaState)
      println("optClickedKomaKind:" + optClickedKomaKind, "clickedKomaKind:" + clickedKomaKind)
      println("isSenteTurnState:" + isSenteTurnState)
      println("")

      /*
      println(board.evaluationFunction)
      println(board.senteEvaluation.toInt, 15 * board.senteAmountEvaluation, board.senteOuDistanceEvaluation.toInt)
      println(board.goteEvaluation.toInt, 15 * board.goteAmountEvaluation, board.goteOuDistanceEvaluation.toInt)
      println("")
      */

      boardSwitch
      isNifu = false
      winDisplay

    })
    group
  }

  def winDisplay {
    (isOuCatch, isCheckmate, isToryo, firstWinMention) match {
      case (Some(true), _, _, true) => isSenteTurnState match { //isOuCatch
        case true => { //先手番 => 先手勝ち
          new Alert(AlertType.Information) {
            initOwner(stage)
            title = "対局終了"
            headerText = "先手の勝ちです"
            contentText = "次に王が取れる状態です"
          }.showAndWait()
          firstWinMention = false
        }
        case false => { //後手番 => 後手勝ち
          new Alert(AlertType.Information) {
            initOwner(stage)
            title = "対局終了"
            headerText = "後手の勝ちです"
            contentText = "次に玉が取れる状態です"
          }.showAndWait()
          firstWinMention = false
        }
      }
      case (_, Some(true), _, true) => isSenteTurnState match { //isCheckmate
        case true => { //先手番 => 後手勝ち
          new Alert(AlertType.Information) {
            initOwner(stage)
            title = "対局終了"
            headerText = "後手の勝ちです"
            contentText = "先手詰みです"
          }.showAndWait()
          firstWinMention = false
        }
        case false => { //後手番 => 先手勝ち
          new Alert(AlertType.Information) {
            initOwner(stage)
            title = "対局終了"
            headerText = "先手の勝ちです"
            contentText = "後手詰みです"
          }.showAndWait()
          firstWinMention = false
        }
      }
      case (_, _, true, true) => isSenteTurnState match { //isToryo
        case true => { //先手番 => 後手勝ち
          new Alert(AlertType.Information) {
            initOwner(stage)
            title = "対局終了"
            headerText = "後手の勝ちです"
            contentText = "先手投了です"
          }.showAndWait()
          firstWinMention = false
        }
        case false => { //後手番 => 先手勝ち
          new Alert(AlertType.Information) {
            initOwner(stage)
            title = "対局終了"
            headerText = "先手の勝ちです"
            contentText = "後手投了です"
          }.showAndWait()
          firstWinMention = false
        }
      }
      case _ =>
    }
  }

  def allRandomBoard: Board = { //G
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

    Board(List(
      Koma(ClickedKomaState.Fu, stockFu(0), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, stockFu(2), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, stockFu(4), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, stockFu(6), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, stockFu(8), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, stockFu(10), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, stockFu(12), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, stockFu(14), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, stockFu(16), goteSideKoma, onBoardStartKoma),

      Koma(ClickedKomaState.Kyo, goteKeiKyoIndex(0), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, goteKeiKyoIndex(1), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, possibleKomaIndex(0), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, possibleKomaIndex(1), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Ou, possibleKomaIndex(2), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, goteKeiKyoIndex(2), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, goteKeiKyoIndex(3), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, possibleKomaIndex(4), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, possibleKomaIndex(3), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Hisha, possibleKomaIndex(5), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, possibleKomaIndex(6), goteSideKoma, onBoardStartKoma),

      Koma(ClickedKomaState.Hisha, possibleKomaIndex(8), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, possibleKomaIndex(9), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, senteKeiKyoIndex(0), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, senteKeiKyoIndex(1), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, possibleKomaIndex(10), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, possibleKomaIndex(11), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Gyoku, possibleKomaIndex(12), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, senteKeiKyoIndex(2), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, senteKeiKyoIndex(3), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, possibleKomaIndex(7), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, possibleKomaIndex(13), senteSideKoma, onBoardStartKoma),

      Koma(ClickedKomaState.Fu, stockFu(1), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, stockFu(3), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, stockFu(5), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, stockFu(7), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, stockFu(9), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, stockFu(11), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, stockFu(13), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, stockFu(15), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, stockFu(17), senteSideKoma, onBoardStartKoma)
    ))
  }

  def slashHalfRandomBoard: Board = { //F
    val senteZone = List(35,34,33, 44,43,42,41, 53,52,51,50,49, 62,61,60,59,58,57, 71,70,69,68,67,66,65, 72,73)
    val goteZone = List(45,46,47, 36,37,38,39, 27,28,29,30,31, 18,19,20,21,22,23, 9,10,11,12,13,14,15, 7,8)
    val r = new Random

    val senteFuPlace = List( 72, 73, (9 - (r.nextInt(2) + 1)) * 9 + 2, (9 - (r.nextInt(3) + 1)) * 9 + 3, (9 - (r.nextInt(4) + 1)) * 9 + 4,
      (9 - (r.nextInt(5) + 1)) * 9 + 5, (9 - (r.nextInt(6) + 1)) * 9 + 6, (9 - (r.nextInt(7) + 1)) * 9 + 7, (9 - (r.nextInt(7) + 1)) * 9 + 8)
    val senteWithoutFuPlace: List[Int] = scala.util.Random.shuffle(senteZone diff senteFuPlace)

    val goteFuPlace = List(r.nextInt(7) * 9, r.nextInt(7) * 9 + 1, r.nextInt(6) * 9 + 2, r.nextInt(5) * 9 + 3,
      r.nextInt(4) * 9 + 4, r.nextInt(3) * 9 + 5, r.nextInt(2) * 9 + 6, 7, 8)
    val goteWithoutFuPlace: List[Int] = scala.util.Random.shuffle(goteZone diff goteFuPlace)

    Board(List(
      Koma(ClickedKomaState.Fu, goteFuPlace(0), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, goteFuPlace(1), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, goteFuPlace(2), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, goteFuPlace(3), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, goteFuPlace(4), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, goteFuPlace(5), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, goteFuPlace(6), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, goteFuPlace(7), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, goteFuPlace(8), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, goteWithoutFuPlace(0), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, goteWithoutFuPlace(1), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, goteWithoutFuPlace(2), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, goteWithoutFuPlace(3), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Ou, goteWithoutFuPlace(4), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, goteWithoutFuPlace(5), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, goteWithoutFuPlace(6), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, goteWithoutFuPlace(7), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, goteWithoutFuPlace(8), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Hisha, goteWithoutFuPlace(9), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, goteWithoutFuPlace(10), goteSideKoma, onBoardStartKoma),

      Koma(ClickedKomaState.Hisha, senteWithoutFuPlace(0), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, senteWithoutFuPlace(1), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, senteWithoutFuPlace(2), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, senteWithoutFuPlace(3), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, senteWithoutFuPlace(4), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, senteWithoutFuPlace(5), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Gyoku, senteWithoutFuPlace(6), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, senteWithoutFuPlace(7), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, senteWithoutFuPlace(8), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, senteWithoutFuPlace(9), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, senteWithoutFuPlace(10), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, senteFuPlace(0), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, senteFuPlace(1), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, senteFuPlace(2), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, senteFuPlace(3), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, senteFuPlace(4), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, senteFuPlace(5), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, senteFuPlace(6), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, senteFuPlace(7), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, senteFuPlace(8), senteSideKoma, onBoardStartKoma)
    ))
  }

  def slashUnderFuInitialBoard: Board = { //E
    val senteZone = List(35,34, 44,43,42, 53,52,51,50, 62,61,60,59,58, 71,70,69,68,67,66)
    val goteZone = List(45,46, 36,37,38, 27,28,29,30, 18,19,20,21,22, 9,10,11,12,13,14)
    val (senteKomaPlace: List[Int], goteKomaPlace: List[Int]) = (scala.util.Random.shuffle(senteZone), scala.util.Random.shuffle(goteZone))
    Board(List( //歩の下でランダムな初期の駒配置
      Koma(ClickedKomaState.Fu, 54, goteSideKoma, handStartKoma), Koma(ClickedKomaState.Fu, 55, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 47, goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 39, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 31, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 23, goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 15, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 7, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 8, goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, goteKomaPlace(0), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, goteKomaPlace(1), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, goteKomaPlace(2), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, goteKomaPlace(3), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Ou, goteKomaPlace(4), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, goteKomaPlace(5), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, goteKomaPlace(6), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, goteKomaPlace(7), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, goteKomaPlace(8), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Hisha, goteKomaPlace(9), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, goteKomaPlace(10), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Hisha, senteKomaPlace(9), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, senteKomaPlace(10), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, senteKomaPlace(0), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, senteKomaPlace(1), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, senteKomaPlace(2), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, senteKomaPlace(3), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Gyoku, senteKomaPlace(4), onBoardStartKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, senteKomaPlace(5), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, senteKomaPlace(6), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, senteKomaPlace(7), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, senteKomaPlace(8), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 26, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 25, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 33, senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 41, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 49, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 57, senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 65, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 73, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 72, senteSideKoma, onBoardStartKoma)
    ))
  }

  def halfRandomBoard: Board = { //D
  val (senteZone, goteZone) = ((45 to 80).toList, (0 to 35).toList)
    val r = new Random

    val senteFuPlace = List( (r.nextInt(4) + 5) * 9 + 0, (r.nextInt(4) + 5) * 9 + 1, (r.nextInt(4) + 5) * 9 + 2, (r.nextInt(4) + 5) * 9 + 3, (r.nextInt(4) + 5) * 9 + 4,
      (r.nextInt(4) + 5) * 9 + 5, (r.nextInt(4) + 5) * 9 + 6, (r.nextInt(4) + 5) * 9 + 7, (r.nextInt(4) + 5) * 9 + 8)
    val senteWithoutFuPlace: List[Int] = scala.util.Random.shuffle(senteZone diff senteFuPlace)

    val goteFuPlace = List(r.nextInt(4) * 9 + 0, r.nextInt(4) * 9 + 1, r.nextInt(4) * 9 + 2, r.nextInt(4) * 9 + 3, r.nextInt(4) * 9 + 4,
      r.nextInt(3) * 9 + 5, r.nextInt(4) * 9 + 6, r.nextInt(4) * 9 + 7, r.nextInt(4) * 9 + 8)
    val goteWithoutFuPlace: List[Int] = scala.util.Random.shuffle(goteZone diff goteFuPlace)

    Board(List(
      Koma(ClickedKomaState.Fu, goteFuPlace(0), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, goteFuPlace(1), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, goteFuPlace(2), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, goteFuPlace(3), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, goteFuPlace(4), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, goteFuPlace(5), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, goteFuPlace(6), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, goteFuPlace(7), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, goteFuPlace(8), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, goteWithoutFuPlace(0), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, goteWithoutFuPlace(1), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, goteWithoutFuPlace(2), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, goteWithoutFuPlace(3), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Ou, goteWithoutFuPlace(4), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, goteWithoutFuPlace(5), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, goteWithoutFuPlace(6), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, goteWithoutFuPlace(7), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, goteWithoutFuPlace(8), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Hisha, goteWithoutFuPlace(9), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, goteWithoutFuPlace(10), goteSideKoma, onBoardStartKoma),

      Koma(ClickedKomaState.Hisha, senteWithoutFuPlace(0), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, senteWithoutFuPlace(1), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, senteWithoutFuPlace(2), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, senteWithoutFuPlace(3), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, senteWithoutFuPlace(4), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, senteWithoutFuPlace(5), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Gyoku, senteWithoutFuPlace(6), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, senteWithoutFuPlace(7), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, senteWithoutFuPlace(8), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, senteWithoutFuPlace(9), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, senteWithoutFuPlace(10), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, senteFuPlace(0), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, senteFuPlace(1), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, senteFuPlace(2), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, senteFuPlace(3), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, senteFuPlace(4), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, senteFuPlace(5), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, senteFuPlace(6), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, senteFuPlace(7), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, senteFuPlace(8), senteSideKoma, onBoardStartKoma)
    ))
  }

  def fourUnderFuInitialBoard: Board = { //C
  val (senteZone, goteZone) = ((54 to 80).toList, (0 to 26).toList)
    val (senteKomaPlace: List[Int], goteKomaPlace: List[Int]) = (scala.util.Random.shuffle(senteZone), scala.util.Random.shuffle(goteZone))
    Board(List( //歩が4段目、その下でランダムな初期の駒配置
      Koma(ClickedKomaState.Fu, 27, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 28, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 29, goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 30, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 31, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 32, goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 33, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 34, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 35, goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, goteKomaPlace(0), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, goteKomaPlace(1), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, goteKomaPlace(2), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, goteKomaPlace(3), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Ou, goteKomaPlace(4), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, goteKomaPlace(5), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, goteKomaPlace(6), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, goteKomaPlace(7), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, goteKomaPlace(8), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Hisha, goteKomaPlace(9), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, goteKomaPlace(10), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Hisha, senteKomaPlace(9), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, senteKomaPlace(10), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, senteKomaPlace(0), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, senteKomaPlace(1), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, senteKomaPlace(2), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, senteKomaPlace(3), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Gyoku, senteKomaPlace(4), onBoardStartKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, senteKomaPlace(5), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, senteKomaPlace(6), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, senteKomaPlace(7), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, senteKomaPlace(8), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 53, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 52, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 51, senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 50, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 49, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 48, senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 47, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 46, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 45, senteSideKoma, onBoardStartKoma)
    ))
  }

  def threeUnderFuInitialBoard: Board = { //B
    val (senteZone, goteZone) = ((63 to 80).toList, (0 to 17).toList)
    val (senteKomaPlace: List[Int], goteKomaPlace: List[Int]) = (scala.util.Random.shuffle(senteZone), scala.util.Random.shuffle(goteZone))
    Board(List( //歩の下でランダムな初期の駒配置
      Koma(ClickedKomaState.Fu, 18, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 19, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 20, goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 21, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 22, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 23, goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 24, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 25, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 26, goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, goteKomaPlace(0), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, goteKomaPlace(1), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, goteKomaPlace(2), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, goteKomaPlace(3), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Ou, goteKomaPlace(4), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, goteKomaPlace(5), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, goteKomaPlace(6), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, goteKomaPlace(7), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, goteKomaPlace(8), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Hisha, goteKomaPlace(9), goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, goteKomaPlace(10), goteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Hisha, senteKomaPlace(9), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, senteKomaPlace(10), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, senteKomaPlace(0), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, senteKomaPlace(1), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, senteKomaPlace(2), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, senteKomaPlace(3), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Gyoku, senteKomaPlace(4), onBoardStartKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Kyo, senteKomaPlace(5), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, senteKomaPlace(6), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Gin, senteKomaPlace(7), senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, senteKomaPlace(8), senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 62, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 61, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 60, senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 59, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 58, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 57, senteSideKoma, onBoardStartKoma),
      Koma(ClickedKomaState.Fu, 56, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 55, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 54, senteSideKoma, onBoardStartKoma)
    ))
  }

  def initialBoard: Board = Board(List( //A
    Koma(ClickedKomaState.Fu, 18, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 19, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 20, goteSideKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Fu, 21, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 22, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 23, goteSideKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Fu, 24, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 25, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 26, goteSideKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Kyo, 0, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, 1, goteSideKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Gin, 2, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, 3, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Ou, 4, goteSideKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Kyo, 8, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, 7, goteSideKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Gin, 6, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, 5, goteSideKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Hisha, 10, goteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, 16, goteSideKoma, onBoardStartKoma),

    Koma(ClickedKomaState.Hisha, 70, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kaku, 64, onBoardStartKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Kyo, 80, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, 79, senteSideKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Gin, 78, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, 77, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Gyoku, 76, senteSideKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Kyo, 72, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kei, 73, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Gin, 74, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Kin, 75, senteSideKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Fu, 62, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 61, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 60, senteSideKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Fu, 59, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 58, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 57, senteSideKoma, onBoardStartKoma),
    Koma(ClickedKomaState.Fu, 56, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 55, senteSideKoma, onBoardStartKoma), Koma(ClickedKomaState.Fu, 54, senteSideKoma, onBoardStartKoma)
  ))

}
