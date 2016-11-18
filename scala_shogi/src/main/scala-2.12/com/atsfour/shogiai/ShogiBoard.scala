package com.atsfour.shogiai

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

  var board: Board = Board(List( //初期の駒配置
    Koma(clickedKomaStates.Fu, 18, false, true), Koma(clickedKomaStates.Fu, 19, false, true), Koma(clickedKomaStates.Fu, 20, false, true),
    Koma(clickedKomaStates.Fu, 21, false, true), Koma(clickedKomaStates.Fu, 22, false, true), Koma(clickedKomaStates.Fu, 23, false, true),
    Koma(clickedKomaStates.Fu, 24, false, true), Koma(clickedKomaStates.Fu, 25, false, true), Koma(clickedKomaStates.Fu, 26, false, true),
    Koma(clickedKomaStates.Kyo, 0, false, true), Koma(clickedKomaStates.Kei, 1, false, true),
    Koma(clickedKomaStates.Gin, 2, false, true), Koma(clickedKomaStates.Kin, 3, false, true), Koma(clickedKomaStates.Ou, 4, false, true),
    Koma(clickedKomaStates.Kyo, 8, false, true), Koma(clickedKomaStates.Kei, 7, false, true), Koma(clickedKomaStates.Gin, 6, false, true), Koma(clickedKomaStates.Kin, 5, false, true),
    Koma(clickedKomaStates.Hisha, 10, false, true), Koma(clickedKomaStates.Kaku, 16, false, true), Koma(clickedKomaStates.Hisha, 70, true, true), Koma(clickedKomaStates.Kaku, 64, true, true),
    Koma(clickedKomaStates.Kyo, 80, true, true), Koma(clickedKomaStates.Kei, 79, true, true),
    Koma(clickedKomaStates.Gin, 78, true, true), Koma(clickedKomaStates.Kin, 77, true, true), Koma(clickedKomaStates.Ou, 76, true, true),
    Koma(clickedKomaStates.Kyo, 72, true, true), Koma(clickedKomaStates.Kei, 73, true, true), Koma(clickedKomaStates.Gin, 74, true, true), Koma(clickedKomaStates.Kin, 75, true, true),
    Koma(clickedKomaStates.Fu, 62, true, true), Koma(clickedKomaStates.Fu, 61, true, true), Koma(clickedKomaStates.Fu, 60, true, true),
    Koma(clickedKomaStates.Fu, 59, true, true), Koma(clickedKomaStates.Fu, 58, true, true), Koma(clickedKomaStates.Fu, 57, true, true),
    Koma(clickedKomaStates.Fu, 56, true, true), Koma(clickedKomaStates.Fu, 55, true, true), Koma(clickedKomaStates.Fu, 54, true, true)
  ))

  //todo Stateを減らしたい
  /** StateとFlagを定義 */
  var selectedCellIndex: Option[Int] = None
  var optIsSenteKomaState: Option[Boolean] = None
  var optOnBoardKomaState: Option[Boolean] = None
  var isSenteTurnState: Boolean = true
  var stockNariIndex = -1

  var clickedKomaKind: ClickedKomaState = clickedKomaStates.None
  abstract class ClickedKomaState(val name: String)
  case object clickedKomaStates {
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
    case object Bikkuri extends ClickedKomaState("!")
    case object V extends ClickedKomaState("V")
    case object S extends ClickedKomaState("S")
    case object Not extends ClickedKomaState("不")
    case object Na extends ClickedKomaState("成")
    case object Ri extends ClickedKomaState("り")
    case object Slash extends ClickedKomaState("/")
    case object O extends ClickedKomaState("o")
    case object R extends ClickedKomaState("r")
    case object Ta extends ClickedKomaState("タ")
    case object A extends ClickedKomaState("ー")
    case object N extends ClickedKomaState("ン")

    lazy val values = Seq(None, Fu, Kyo, Kei, Gin, Kin, Ou, Kaku, Hisha, To, NariKyo, NariKei, NariGin, Uma, Ryu, Sen, Go, Te, No, Ka, Chi, Bikkuri, V, S, Not, Na, Ri, Slash, O, R, Ta, A, N)
  }

  /** 将棋盤のテンプレートの切り替え */
  var (isWin, isCanNari) = (false, false)
  def boardSwitch :Board = {
    val realKomas: List[Koma] = board match { case Board(komas) => komas }
    val displayKoma: Boolean = true
    board = (isWin, isCanNari, isSenteTurnState) match { //1手指すと出てくる
      case (false, false  ,true) => {
      val normalBoard: Board = Board( //先手のターン
          Koma(clickedKomaStates.Sen, 105, isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Te, 106, isSenteTurnState, displayKoma):: Koma(clickedKomaStates.No, 107, isSenteTurnState, displayKoma) ::
            (Koma(clickedKomaStates.Ta, 108, isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.A, 109, isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.N, 110, isSenteTurnState, displayKoma) ::
              realKomas))
        normalBoard
      }
      case (false, false , false) => {
      val normalBoard: Board = Board( //後手のターン
          Koma(clickedKomaStates.Go, 105, isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Te, 106, isSenteTurnState, displayKoma):: Koma(clickedKomaStates.No, 107, isSenteTurnState, displayKoma) ::
            (Koma(clickedKomaStates.Ta, 108, isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.A, 109, isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.N, 110, isSenteTurnState, displayKoma) ::
              realKomas))
        normalBoard
      }
      case (true, _ , false) => { //駒を取った時には後手の番に移っている
        val SenteWinBoard: Board = Board( //先手の勝ち！
          Koma(clickedKomaStates.Sen, 105, !isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Te, 106, !isSenteTurnState, displayKoma):: Koma(clickedKomaStates.No, 107, !isSenteTurnState, displayKoma) ::
          (Koma(clickedKomaStates.Ka, 108, !isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Chi, 109, !isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Bikkuri, 110, !isSenteTurnState, displayKoma) ::
          realKomas))
        SenteWinBoard
      }
      case (true, _ , true) => {
        val GoteWinboard: Board = Board( //後手の勝ち！
          Koma(clickedKomaStates.Go, 105, !isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Te, 106, !isSenteTurnState, displayKoma):: Koma(clickedKomaStates.No, 107, !isSenteTurnState, displayKoma) ::
          (Koma(clickedKomaStates.Ka, 108, !isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Chi, 109, !isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Bikkuri, 110, !isSenteTurnState, displayKoma) ::
          realKomas))
        GoteWinboard
      }
      case (_, true , true) => {
        val SenteWinBoard: Board = Board( // 成りor不成
          Koma(clickedKomaStates.Na, 105, isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Ri, 106, isSenteTurnState, displayKoma):: Koma(clickedKomaStates.O, 107, isSenteTurnState, displayKoma) ::
            Koma(clickedKomaStates.R, 108, isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Not, 109, isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Na, 110, isSenteTurnState, displayKoma) ::
            realKomas)
        SenteWinBoard
      }
      case (_, true , false) => {
        val GoteWinboard: Board = Board( // 成りor不成
          Koma(clickedKomaStates.Na, 105, isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Ri, 106, isSenteTurnState, displayKoma):: Koma(clickedKomaStates.O, 107, isSenteTurnState, displayKoma) ::
            Koma(clickedKomaStates.R, 108, isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Not, 109, isSenteTurnState, displayKoma) :: Koma(clickedKomaStates.Na, 110, isSenteTurnState, displayKoma) :: realKomas)
        GoteWinboard
      }
      case _ => {
        val normalBoard: Board = Board(realKomas)
        normalBoard
      }
    }
    board
  }

  /** 描画定義 */
  def inBord(index:Int) = index <= 80
  def outOfBord(index:Int) = (index >= 81 && index <= 134) //持ち駒,テンプレートの描画をする場所

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

    /** Cellの描画を定義 */
    val fillColor = if (selectedCellIndex.contains(clickedIndex) && clickedKomaKind != clickedKomaStates.None ) { LightBlue }
    else if (clickedIndex <= 80 //盤面
      || ((clickedIndex >= 81 && clickedIndex <= 134) && (clickedIndex-81) % 6 != 0 && (clickedIndex-81) / 6 != 4) //持ち駒
    ) { Burlywood }
    else White

    val grid = {
      val rect = Rectangle(80, 80, fillColor)
      if (clickedIndex <= 80 //盤面
        || ((clickedIndex >= 81 && clickedIndex <= 134) && (clickedIndex-81) % 6 != 0 && (clickedIndex-81) / 6 != 4) //持ち駒
      ){ rect.setStroke(Black) }
      rect
    }

    val group =  new Group { children = List(Some(grid), komaOpt.map(komaObjGroup)).flatten }

    /** 以下、駒をクリックした時に使う関数のまとまり */
    /* clickedIndexと一致した場合の駒情報を取得する (komaの中のClickedIndexと一致する、という条件が必要(本当は)) */
    val optClickedKomaKind: Option[ClickedKomaState] = komaOpt.map(koma => koma.kind)
    val optIsSenteKoma: Option[Boolean] = komaOpt.map(koma => koma.isSente)
    val optOnBoard: Option[Boolean] = komaOpt.map(koma => koma.onBoard)

    val existSelectedCellIndex = selectedCellIndex.getOrElse(-1)
    val absMoveDistance = Math.abs(existSelectedCellIndex - clickedIndex) //駒の移動距離の絶対値を定義
    val moveDistance = existSelectedCellIndex - clickedIndex //駒の移動距離を定義

    /** クリック時にどの判定を行うべきか分岐 */
    def senteHandKomaBranch: Boolean = {
      ((optOnBoard.contains(false) && clickedKomaKind == clickedKomaStates.None) || optOnBoardKomaState.contains(false)) &&
        (optIsSenteKoma.contains(true) || optIsSenteKomaState.contains(true)) && isSenteTurnState && !optOnBoardKomaState.contains(true) && !isCanNari
    }
    def goteHandKomaBranch: Boolean = {
      ((optOnBoard.contains(false) && clickedKomaKind == clickedKomaStates.None) || optOnBoardKomaState == Option(false)) &&
        (optIsSenteKoma.contains(false) || optIsSenteKomaState.contains(false)) && !isSenteTurnState && !optOnBoardKomaState.contains(true) && !isCanNari
    }

    def senteKomaBranch(koma: ClickedKomaState): Boolean = {
      ((optClickedKomaKind.contains(koma) && clickedKomaKind == clickedKomaStates.None) || clickedKomaKind == koma) &&
        (optIsSenteKoma.contains(true) || optIsSenteKomaState.contains(true)) && isSenteTurnState && !optOnBoardKomaState.contains(false) && !isCanNari
    }
    def goteKomaBranch(koma: ClickedKomaState): Boolean = {
      ((optClickedKomaKind.contains(koma) && clickedKomaKind == clickedKomaStates.None) || clickedKomaKind == koma) &&
        (optIsSenteKoma.contains(false) || optIsSenteKomaState.contains(false)) && !isSenteTurnState && !optOnBoardKomaState.contains(false) && !isCanNari
    }

    def nariChoiceBranch: Boolean = {
      (optClickedKomaKind.contains(clickedKomaStates.Na) && selectedCellIndex.contains(105)) || optClickedKomaKind.contains(clickedKomaStates.Ri)
    }
    def funariChoiceBranch: Boolean = {
      (optClickedKomaKind.contains(clickedKomaStates.Na) && selectedCellIndex.contains(110)) || optClickedKomaKind.contains(clickedKomaStates.Not)
    }

    /** 複数回クリックした時に、駒の情報を保存したり、条件を外したり、条件制御を行う */
    def addState = {
      clickedKomaKind = optClickedKomaKind.getOrElse(clickedKomaStates.None)
      optIsSenteKomaState = optIsSenteKoma
      optOnBoardKomaState = optOnBoard
    }
    def fromToBoradAddState(koma: ClickedKomaState) = {
      if (optClickedKomaKind.contains(koma) && clickedKomaKind == clickedKomaStates.None) addState
    }
    def clickCancel = {
      if (moveDistance != 0) {
        clickedKomaKind = clickedKomaStates.None
        optIsSenteKomaState = None
        optOnBoardKomaState = None
        selectedCellIndex = None
      }
    }

    /** 駒を取った時の処理と王様が取られた場合の判定 */
    def takeKoma(clickedIndex: Int) = { //駒を取った時に行う処理の集まり
      takeOuCheck(clickedIndex)
      board = board.ownerChangeKoma(clickedIndex, optIsSenteKoma.contains(true)) //相手の駒が自分の駒になる
      board = board.spaceChangeKoma(clickedIndex, optOnBoard.contains(true)) //盤上の取られた駒が持ち駒になる
      board = board.returnNariKoma(clickedIndex)
      board = board.moveKoma(clickedIndex, handMove) //取られた駒の情報を書き換えて、最後に持ち駒に移動する
    }

    //持ち駒をどこに置くかを決める
    //todo varを無くす, 持ち駒をソートする機能があると見栄えが良い
    def handMove: Int = {
      var (handPlace, count) = (108, 0)
      if (isSenteTurnState) {
        for (handsPlace <- 111 until 135) {
          if (board.findKoma(handsPlace).isEmpty && (handsPlace - 81) % 6 != 0 ) handPlace = handsPlace
        }
      } else if (!isSenteTurnState) {
        for (handsPlace <- 81 until 105) {
          if (board.findKoma(handsPlace).isEmpty && (handsPlace - 81) % 6 != 0 && count == 0) {
            handPlace = handsPlace
            count = count  + 1
          }
        }
      }
      handPlace
    }

    def takeOuCheck(clickedIndex: Int) = {
      optClickedKomaKind.getOrElse(clickedKomaStates.None) match {
        case clickedKomaStates.Ou => isWin = true
        case _ =>
      }
    }

    /** 成れるかどうかの条件判定 */
    def mustNari(num: Int): Boolean = isSenteTurnState match {
      case true => {
        println((clickedKomaKind == clickedKomaStates.Fu || clickedKomaKind == clickedKomaStates.Kyo), (clickedIndex / 9) + 1 == 1)
        ((clickedKomaKind == clickedKomaStates.Fu || clickedKomaKind == clickedKomaStates.Kyo) && (clickedIndex / 9) + 1 == 1) || //先手の歩と香車が1段目
          (clickedKomaKind == clickedKomaStates.Kei && (clickedIndex / 9 + 1) <= 2) //先手の桂馬が1段目と2段目
      }
      case false => {
        println((clickedKomaKind == clickedKomaStates.Fu || clickedKomaKind == clickedKomaStates.Kyo), (clickedIndex / 9) + 1 == 9)
        ((clickedKomaKind == clickedKomaStates.Fu || clickedKomaKind == clickedKomaStates.Kyo) && (clickedIndex / 9) + 1 == 9) ||
          (clickedKomaKind == clickedKomaStates.Kei && (clickedIndex / 9 + 1) >= 8)
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
      clickedKomaKind == clickedKomaStates.Fu || clickedKomaKind == clickedKomaStates.Kyo ||
        clickedKomaKind == clickedKomaStates.Kei || clickedKomaKind == clickedKomaStates.Gin ||
        clickedKomaKind == clickedKomaStates.Hisha || clickedKomaKind == clickedKomaStates.Kaku
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
    def toMoveBoard: Boolean = clickedIndex <= 80
    def fromOnBoard(num: Int): Boolean = num <= 80
    def fromOutOfBoard(num: Int): Boolean = num >= 81 && num <= 134
    def switchTurn(nextTurn: Boolean): Boolean = if (nextTurn) false else true

    def playAndInitialize(num: Int) = {

      if (mustNari(num)) {
        board = board.nariKoma(num) //強制的に成り、相手の手番へ
        isSenteTurnState = switchTurn(isSenteTurnState)
      }
      else if (canNari(num)) addNariGomaState //どこにいる駒が成れる状態、という状態を付与
      else isSenteTurnState = switchTurn(isSenteTurnState) //成れない場合は相手の手番へ

      board = board.moveKoma(num, clickedIndex)
      if (optOnBoardKomaState.contains(false))
        board = board.spaceChangeKoma(clickedIndex, optOnBoard.contains(false)) //打ち終わった駒は盤上の駒になる

      selectedCellIndex = None
      optIsSenteKomaState = None
      clickedKomaKind = clickedKomaStates.None
      optOnBoardKomaState = None
    }

    /** 盤上の駒を動かす時のtakeKomaとplayAndInitializeの条件分岐処理をまとめた */
    def takeKomaAndplayAndInitialize(num: Int) = {
      isSenteTurnState match {
        case true => {
          if (optIsSenteKoma.contains(false)) takeKoma(clickedIndex)
          if (!optIsSenteKoma.contains(true)) playAndInitialize(num)
        }
        case false => {
          if (optIsSenteKoma.contains(true)) takeKoma(clickedIndex)
          if (!optIsSenteKoma.contains(false)) playAndInitialize(num)
        }
      }
    }
    /** ここまで駒をクリックした時に使われる関数群 */

    //todo 移動条件を関数化、呼び出す形にする
    /** boardに、selectedCellIndex(選択したセルのIndex)をboard変数に移動させ、repaint関数で 再描画する */
    group.setOnMouseClicked(e => {
      selectedCellIndex match {
        case Some(num) => {

          // デバッグ用
          println("existSelectedCellIndex:" + existSelectedCellIndex, "selectedCellIndex:" + selectedCellIndex, "stockNariIndex:" + stockNariIndex)
          println("optOnBoard:" + optOnBoard, "optOnBoardKomaState:" + optOnBoardKomaState)
          println("optIsSenteKoma:" + optIsSenteKoma, "optIsSenteKomaState:" + optIsSenteKomaState)
          println("optClickedKomaKind:" + optClickedKomaKind, "clickedKomaKind:" + clickedKomaKind)
          println("isSenteTurnState:" + isSenteTurnState)
          println("")

          /** 駒が成るかどうかの判定をクリックした場合の処理 */
          if (nariChoiceBranch) {
            board = board.nariKoma(stockNariIndex)
            initializeNariGomaState //立てた成りフラグを元に戻す
          }
          else if (funariChoiceBranch) { initializeNariGomaState }

          /** 持ち駒をクリックした場合の処理 */
          if (senteHandKomaBranch) { /** 先手で持ち駒をクリックした、選択していた場合 */
            /** クリックした時に一度入ったというStateを用意(行き先をクリックした時、駒を認識できなくなっている) */
            if (optOnBoard.contains(false) && clickedKomaKind == clickedKomaStates.None) addState

            if (toMoveBoard && optIsSenteKoma.isEmpty &&
              (!((clickedKomaKind == clickedKomaStates.Fu || clickedKomaKind == clickedKomaStates.Kyo) && (clickedIndex / 9) + 1 == 1)) && //先手の歩と香車は、1段目に打てない
              !(clickedKomaKind == clickedKomaStates.Kei && (clickedIndex / 9 + 1) <= 2) && //先手の桂馬は、1段目と2段目に打てない
              (clickedKomaKind != clickedKomaStates.Fu || board.nifuCheck(clickedIndex, optIsSenteKomaState.contains(true)))
            ) {
              playAndInitialize(num)

            } else clickCancel
          }

          else if (goteHandKomaBranch) { /** 後手で持ち駒をクリックした、選択していた場合 */
            if (optOnBoard.contains(false) && clickedKomaKind == clickedKomaStates.None) addState
            if (toMoveBoard && optIsSenteKoma.isEmpty &&
              (!((clickedKomaKind == clickedKomaStates.Fu || clickedKomaKind == clickedKomaStates.Kyo) && (clickedIndex / 9) + 1 == 9)) && //後手の歩と香車は、9段目に打てない
              (!(clickedKomaKind == clickedKomaStates.Kei && (clickedIndex / 9 + 1) >= 8)) && //後手の桂馬は、8段目と9段目に打てない
              (clickedKomaKind != clickedKomaStates.Fu || board.nifuCheck(clickedIndex, optIsSenteKomaState.contains(true))) //先手ならtrue, 後手ならfalseを返す
            ) {
              playAndInitialize(num)
            } else clickCancel
          }

          /** 持ち駒でない場合、盤上の駒なので、処理に入る */
          else if (senteKomaBranch(clickedKomaStates.Fu)) { /** 先手の歩の場合 */
            fromToBoradAddState(clickedKomaStates.Fu)
            if (moveDistance == 9 && toMoveBoard) takeKomaAndplayAndInitialize(num)
            else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.Fu)) { /** 後手の歩の場合 */
            fromToBoradAddState(clickedKomaStates.Fu)
            if (moveDistance == -9 && toMoveBoard) takeKomaAndplayAndInitialize(num)
            else clickCancel
          }

          else if (senteKomaBranch(clickedKomaStates.Kyo)) { /** 先手の香車の場合 */
            fromToBoradAddState(clickedKomaStates.Kyo)
            if (moveDistance % 9 == 0 && toMoveBoard) takeKomaAndplayAndInitialize(num)
            else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.Kyo)) { /** 後手の香車の場合 */
            fromToBoradAddState(clickedKomaStates.Kyo)
            if (moveDistance % 9 == 0 && moveDistance < 0 && board.downJumpCheck(num, clickedIndex) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }

          else if (senteKomaBranch(clickedKomaStates.Kei)) { /** 先手の桂馬の場合 */
            fromToBoradAddState(clickedKomaStates.Kei)
            if (moveDistance == 17 || moveDistance == 19 && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.Kei)) { /** 後手の桂馬の場合 */
            fromToBoradAddState(clickedKomaStates.Kei)
            if (moveDistance == -17 || moveDistance == -19 && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }

          else if (senteKomaBranch(clickedKomaStates.Gin)) { /** 先手の銀の場合 */
            fromToBoradAddState(clickedKomaStates.Gin)
            if ((absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == 9) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.Gin)) { /** 後手の銀の場合 */
            fromToBoradAddState(clickedKomaStates.Gin)
            if ((absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == -9) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }

          else if (senteKomaBranch(clickedKomaStates.Kin)) { /** 先手の金の場合 */
            fromToBoradAddState(clickedKomaStates.Kin)
            if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.Kin)) { /** 後手の金の場合 */
            fromToBoradAddState(clickedKomaStates.Kin)
            if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }

          else if (senteKomaBranch(clickedKomaStates.Ou)) { /** 先手の王の場合 */
            fromToBoradAddState(clickedKomaStates.Ou)
            if ((absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.Ou)) { /** 後手の王の場合 */
            fromToBoradAddState(clickedKomaStates.Ou)
            if ((absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }

          else if (senteKomaBranch(clickedKomaStates.Kaku)) { /** 先手の角の場合 */
            fromToBoradAddState(clickedKomaStates.Kaku)
            if (((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex)) //左上から右下方向
              || absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex)) //右上から左下方向
              && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.Kaku)) { /** 後手の角の場合 */
            fromToBoradAddState(clickedKomaStates.Kaku)
            if (((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex))
              || absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex))
              && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }

          else if (senteKomaBranch(clickedKomaStates.Hisha)) { /** 先手の飛車の場合 */
            fromToBoradAddState(clickedKomaStates.Hisha)
            if ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
              || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex)) //横方向
              && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.Hisha)) { /** 後手の飛車の場合 */
            fromToBoradAddState(clickedKomaStates.Hisha)
            if ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex))
              || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex))
              && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }

          /** 成り駒 */
          else if (senteKomaBranch(clickedKomaStates.To)) { /** 先手のとの場合 */
            fromToBoradAddState(clickedKomaStates.To)
            if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.To)) { /** 後手のとの場合 */
            fromToBoradAddState(clickedKomaStates.To)
            if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (senteKomaBranch(clickedKomaStates.NariKyo)) { /** 先手の成香の場合 */
            fromToBoradAddState(clickedKomaStates.NariKyo)
            if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.NariKyo)) { /** 後手の成香の場合 */
            fromToBoradAddState(clickedKomaStates.NariKyo)
            if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (senteKomaBranch(clickedKomaStates.NariKei)) { /** 先手の成桂の場合 */
            fromToBoradAddState(clickedKomaStates.NariKei)
            if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.NariKei)) { /** 後手の成桂の場合 */
            fromToBoradAddState(clickedKomaStates.NariKei)
            if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (senteKomaBranch(clickedKomaStates.NariGin)) { /** 先手の成銀の場合 */
            fromToBoradAddState(clickedKomaStates.NariGin)
            if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.NariGin)) { /** 後手の成銀の場合 */
            fromToBoradAddState(clickedKomaStates.NariGin)
            if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (senteKomaBranch(clickedKomaStates.Uma)) { /** 先手の馬の場合 */
            fromToBoradAddState(clickedKomaStates.Uma)
            if (((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex)) //左上~右下
              || (absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex)) //左下~右上
              || absMoveDistance == 1 || absMoveDistance == 9) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.Uma)) { /** 後手の馬の場合 */
            fromToBoradAddState(clickedKomaStates.Uma)
            if (((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex))
              || (absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex))
              || absMoveDistance == 1 || absMoveDistance == 9) && toMoveBoard) {
              println(board.leftUpJumpCheck(num, clickedIndex))
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (senteKomaBranch(clickedKomaStates.Ryu)) { /** 先手の龍の場合 */
            fromToBoradAddState(clickedKomaStates.Ryu)
            if (((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
              || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex)) //横方向
              || absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }
          else if (goteKomaBranch(clickedKomaStates.Ryu)) { /** 後手の龍の場合 */
            fromToBoradAddState(clickedKomaStates.Ryu)
            if (((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex))
              || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex))
              || absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) && toMoveBoard) {
              takeKomaAndplayAndInitialize(num)
            } else clickCancel
          }

        }
        case None => selectedCellIndex = Some(clickedIndex)
      }
      boardSwitch
      repaint
    })
    group
  }

  def komaObjGroup(koma: Koma): Group = {
    val senteKomaShape = { //駒の形を定義している
      val poly = koma.isSente match {
        case true => Polygon(40, 10, 60, 20, 70, 70, 10, 70, 20, 20)
        case false => Polygon(20, 20, 60, 70, 70, 20, 10, 60, 10, 40)
      }
        poly.setFill(Sienna)
        poly.setStroke(Black)
        poly
    }

    val komaLabel = { //升ないの駒の置き場所を定義してる
      val label = new Label
      label.setText(koma.kind.name)
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

}
