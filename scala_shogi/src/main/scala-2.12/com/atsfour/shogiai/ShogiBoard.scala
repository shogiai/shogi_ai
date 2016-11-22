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

  val initalKomas: List[Koma] = initalBoard match { case Board(komas) => komas }
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
    case object Bikkuri extends ClickedKomaState("!")

    case object Not extends ClickedKomaState("不")
    case object Na extends ClickedKomaState("成")
    case object Ri extends ClickedKomaState("り")
    case object O extends ClickedKomaState("o")
    case object R extends ClickedKomaState("r")

    case object Ban extends ClickedKomaState("番")
    case object Ni extends ClickedKomaState("二")
    case object De extends ClickedKomaState("で")
    case object Su extends ClickedKomaState("す")

    case object Ma extends ClickedKomaState("待")
    case object Ltu extends ClickedKomaState("っ")
    case object TaHira extends ClickedKomaState("た")

    case object Sho extends ClickedKomaState("初")
    case object Ki extends ClickedKomaState("期")
    case object KaGo extends ClickedKomaState("化")

    case object A extends ClickedKomaState("A")
    case object B extends ClickedKomaState("B")
    case object C extends ClickedKomaState("C")
    case object D extends ClickedKomaState("D")

    lazy val values = Seq(None, Fu, Kyo, Kei, Gin, Kin, Ou, Kaku, Hisha, To, NariKyo, NariKei, NariGin, Uma, Ryu, Sen, Go, Te, No, Ka, Chi, Bikkuri, Not, Na, Ri)
  }

  /** 将棋盤のテンプレートの切り替え */
  var (isWin, isCanNari, isNifu) = (false, false, false)
  def boardSwitch :Board = {

   /** 初期化と待ったのボタン更新 */
   val transitionKoma: Boolean = true
   val onBoardKomas: List[Koma] = board match { case Board(komas) => komas.takeRight(40) }
   val pastKomas: List[Koma] = pastBoard match { case Board(komas) => komas.takeRight(40) }

    board = if (onBoardKomas == pastKomas) {
      val addInitializeBoard: Board = Board(
        Koma(ClickedKomaState.A, 81, true, transitionKoma) :: Koma(ClickedKomaState.B, 87, true, transitionKoma) ::
          Koma(ClickedKomaState.C, 93, true, transitionKoma) :: Koma(ClickedKomaState.D, 99, true, transitionKoma) :: //初期化
        onBoardKomas)
      addInitializeBoard
    } else {
      val addBoard: Board = Board (
        Koma(ClickedKomaState.A, 81, true, transitionKoma) :: Koma(ClickedKomaState.B, 87, true, transitionKoma) ::
          Koma(ClickedKomaState.C, 93, true, transitionKoma) :: Koma(ClickedKomaState.D, 99, true, transitionKoma) :: //初期化
        Koma (ClickedKomaState.Ma, 117, true, transitionKoma) :: Koma (ClickedKomaState.Ltu, 123, true, transitionKoma) :: Koma (ClickedKomaState.TaHira, 129, true, transitionKoma) :: //待った
        onBoardKomas)
      addBoard
    }

    /** その他テンプレートの更新 */
    val realKomas: List[Koma] = board match { case Board(komas) => komas }
    val displayKoma: Boolean = true
    board = (isWin, isCanNari, isNifu, isSenteTurnState) match { //1手指すと出てくる
      case (false, false, false ,true) => {
        val normalBoard: Board = Board( //先手番
          Koma(ClickedKomaState.Sen, 107, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 108, isSenteTurnState, displayKoma):: Koma(ClickedKomaState.Ban, 109, isSenteTurnState, displayKoma) ::
              realKomas)
        normalBoard
      }
      case (false, false, false ,false) => {
        val normalBoard: Board = Board( //後手番
          Koma(ClickedKomaState.Go, 107, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 108, isSenteTurnState, displayKoma):: Koma(ClickedKomaState.Ban, 109, isSenteTurnState, displayKoma) ::
              realKomas)
        normalBoard
      }
      case (true, _, _, false) => { //駒を取った時には後手の番に移っている
      val SenteWinBoard: Board = Board( //先手の勝ち！
          Koma(ClickedKomaState.Sen, 105, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 106, !isSenteTurnState, displayKoma):: Koma(ClickedKomaState.No, 107, !isSenteTurnState, displayKoma) ::
            (Koma(ClickedKomaState.Ka, 108, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 109, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Bikkuri, 110, !isSenteTurnState, displayKoma) ::
              realKomas))
        SenteWinBoard
      }
      case (true, _, _, true) => {
        val GoteWinboard: Board = Board( //後手の勝ち！
          Koma(ClickedKomaState.Go, 105, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 106, !isSenteTurnState, displayKoma):: Koma(ClickedKomaState.No, 107, !isSenteTurnState, displayKoma) ::
            (Koma(ClickedKomaState.Ka, 108, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Chi, 109, !isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Bikkuri, 110, !isSenteTurnState, displayKoma) ::
              realKomas))
        GoteWinboard
      }
      case (_, true, _, true) => {
        val SenteWinBoard: Board = Board( // 成りor不成
          Koma(ClickedKomaState.Na, 105, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ri, 106, isSenteTurnState, displayKoma):: Koma(ClickedKomaState.O, 107, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.R, 108, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Not, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Na, 110, isSenteTurnState, displayKoma) ::
            realKomas)
        SenteWinBoard
      }
      case (_, true, _, false) => {
        val GoteWinboard: Board = Board( // 成りor不成
          Koma(ClickedKomaState.Na, 105, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Ri, 106, isSenteTurnState, displayKoma):: Koma(ClickedKomaState.O, 107, isSenteTurnState, displayKoma) ::
            Koma(ClickedKomaState.R, 108, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Not, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Na, 110, isSenteTurnState, displayKoma) :: realKomas)
        GoteWinboard
      }
      case (_, _, true, true) => {
        val SenteNifuBoard: Board = Board( //先手二歩です
          Koma(ClickedKomaState.Sen, 105, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 106, isSenteTurnState, displayKoma):: Koma(ClickedKomaState.Ni, 107, isSenteTurnState, displayKoma) ::
            (Koma(ClickedKomaState.Fu, 108, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.De, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Su, 110, isSenteTurnState, displayKoma) ::
              realKomas))
        SenteNifuBoard
      }
      case (_, _, true, false) => {
        val GoteNifuboard: Board = Board( //後手二歩です
          Koma(ClickedKomaState.Go, 105, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Te, 106, isSenteTurnState, displayKoma):: Koma(ClickedKomaState.Ni, 107, isSenteTurnState, displayKoma) ::
            (Koma(ClickedKomaState.Fu, 108, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.De, 109, isSenteTurnState, displayKoma) :: Koma(ClickedKomaState.Su, 110, isSenteTurnState, displayKoma) ::
              realKomas))
        GoteNifuboard
      }

      case _ => {
        val normalBoard: Board = Board(realKomas)
        normalBoard
      }
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

    def toMoveBoard: Boolean = clickedIndex <= 80
    def handPlace: Boolean = (clickedIndex >= 81 && clickedIndex <= 134) && (clickedIndex-81) % 6 != 0 && (clickedIndex-81) / 6 != 4

    def fromOnBoard(num: Int): Boolean = num <= 80
    def fromOutOfBoard(num: Int): Boolean = num >= 81 && num <= 134
    def switchTurn(nextTurn: Boolean): Boolean = if (nextTurn) false else true

    /** Cellの描画を定義 */
    val fillColor = if (selectedCellIndex.contains(clickedIndex) && clickedKomaKind != ClickedKomaState.None ) LightBlue
    else if (toMoveBoard || handPlace) Burlywood
    else White

    val grid = {
      val rect = Rectangle(80, 80, fillColor)
      if (toMoveBoard || handPlace) rect.setStroke(Black)
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
      (optClickedKomaKind.contains(ClickedKomaState.Na) && selectedCellIndex.contains(105)) || optClickedKomaKind.contains(ClickedKomaState.Ri)
    }
    def funariChoiceBranch: Boolean = {
      (optClickedKomaKind.contains(ClickedKomaState.Na) && selectedCellIndex.contains(110)) || optClickedKomaKind.contains(ClickedKomaState.Not)
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
      board = board.ownerChangeKoma(clickedIndex, optIsSenteKoma.contains(true)) //相手の駒が自分の駒になる
      board = board.spaceChangeKoma(clickedIndex, optOnBoard.contains(true)) //盤上の取られた駒が持ち駒になる
      board = board.returnNariKoma(clickedIndex)
      pastBoard = board //動かす前に保存
      board = board.moveKoma(clickedIndex, handMove) //取られた駒の情報を書き換えて、最後に持ち駒に移動する
    }

    //持ち駒をどこに置くかを決める, 持ち駒をソートする機能があると見栄えが良い
    def handMove: Int = {
      val senteHand = (111 until 135).toList
      val goteHand = (81 until 105).toList
      isSenteTurnState match {
        case true => {
          senteHand.filter(senteHandsPlace => board.findKoma(senteHandsPlace).isEmpty && (senteHandsPlace - 81) % 6 != 0).last
        }
        case false => {
          goteHand.filter(goteHandsPlace => board.findKoma(goteHandsPlace).isEmpty && (goteHandsPlace - 81) % 6 != 0).head
        }
      }
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

      if (mustNari(num)) {
        board = board.nariKoma(num) //強制的に成り、相手の手番へ
        isSenteTurnState = switchTurn(isSenteTurnState)
      }
      else if (canNari(num)) addNariGomaState //どこにいる駒が成れる状態、という状態を付与
      else isSenteTurnState = switchTurn(isSenteTurnState) //成れない場合は相手の手番へ

      pastBoard = board //動かす前に保存
      board = board.moveKoma(num, clickedIndex)
      if (optOnBoardKomaState.contains(false))
        board = board.spaceChangeKoma(clickedIndex, optOnBoard.contains(false)) //打ち終わった駒は盤上の駒になる

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
        }
        case false => {
          if (optIsSenteKoma.contains(true)) takeKoma(clickedIndex)
          if (!optIsSenteKoma.contains(false)) playAndInitialize(num)
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

    //todo ClickCancelのタイミングを変える
    /** 駒に応じて、動けるかどうかを判定する関数 */
    def canMove(koma: ClickedKomaState, num: Int): Boolean = {
      val canMove: Boolean = {
        if (isSenteTurnState) {
          koma match {
            case ClickedKomaState.Fu => moveDistance == 9 && toMoveBoard
            case ClickedKomaState.Kyo => moveDistance % 9 == 0 && moveDistance > 0 && board.upJumpCheck(num, clickedIndex) && toMoveBoard
            case ClickedKomaState.Kei => moveDistance == 17 || moveDistance == 19 && toMoveBoard
            case ClickedKomaState.Gin => (absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == 9) && toMoveBoard
            case ClickedKomaState.Kin => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard
            case ClickedKomaState.Ou => (absMoveDistance == 1 || absMoveDistance == 9 || absMoveDistance == 8 || absMoveDistance == 10) && toMoveBoard
            case ClickedKomaState.Kaku => ((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex)) //左上から右下方向
              || (absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex)) //右上から左下方向
              && toMoveBoard)
            case ClickedKomaState.Hisha => ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
              || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex)) //横方向
              && toMoveBoard)
            case ClickedKomaState.To => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard
            case ClickedKomaState.NariKyo => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard
            case ClickedKomaState.NariKei => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard
            case ClickedKomaState.NariGin => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard
            case ClickedKomaState.Uma => ((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex))
              || (absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex))
              || absMoveDistance == 1 || absMoveDistance == 9) && toMoveBoard
            case ClickedKomaState.Ryu =>((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
              || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex)) //横方向
              || absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) && toMoveBoard
            case _ => false
          }
        } else {
          koma match {
            case ClickedKomaState.Fu => moveDistance == -9 && toMoveBoard
            case ClickedKomaState.Kyo => moveDistance % 9 == 0 && moveDistance < 0 && board.downJumpCheck(num, clickedIndex) && toMoveBoard
            case ClickedKomaState.Kei => moveDistance == -17 || moveDistance == -19 && toMoveBoard
            case ClickedKomaState.Gin => (absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == -9) && toMoveBoard
            case ClickedKomaState.Kin => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard
            case ClickedKomaState.Ou => (absMoveDistance == 1 || absMoveDistance == 9 || absMoveDistance == 8 || absMoveDistance == 10) && toMoveBoard
            case ClickedKomaState.Kaku => ((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex))
              || (absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex))
              && toMoveBoard)
            case ClickedKomaState.Hisha => ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex))
              || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex))
              && toMoveBoard)
            case ClickedKomaState.To => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard
            case ClickedKomaState.NariKyo => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard
            case ClickedKomaState.NariKei => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard
            case ClickedKomaState.NariGin => (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard
            case ClickedKomaState.Uma => ((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex))
              || (absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex))
              || absMoveDistance == 1 || absMoveDistance == 9) && toMoveBoard
            case ClickedKomaState.Ryu =>((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
              || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex)) //横方向
              || absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) && toMoveBoard
            case _ => false
          }
        }
      }
      canMove
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

    def initializationOrWaitFlow = {
      if (initializationBranch) {
        optClickedKomaKind match { //初期化時の駒配置の選択が可能
          case Some(ClickedKomaState.A) => board = initalBoard
          case Some(ClickedKomaState.B) => board = newUnderFuInitalBoard
          case Some(ClickedKomaState.C) => board = halfRandomBoard
          case Some(ClickedKomaState.D) => board = allRandomBoard
        }
        pastBoard = board //待ったはなし
        isSenteTurnState = true
      }
      else if (waitBranch) {
        board = pastBoard
        isSenteTurnState = !isSenteTurnState
      }
      clickCancel
      isCanNari = false
      stockNariIndex = -1
    }

    /** ここまで駒をクリックした時に使われる関数群 */

    group.setOnMouseClicked(e => {
      selectedCellIndex match {
        case Some(num) => {

          /** 初期化、待ったをクリックした場合の処理 */
          if (initializationBranch) initializationOrWaitFlow
          else if (waitBranch) initializationOrWaitFlow

          /** 駒が成るかどうかの判定をクリックした場合の処理 */
          else if (nariChoiceBranch) {
            board = board.nariKoma(stockNariIndex)
            initializeNariGomaState //状態を元に戻す
          } else if (funariChoiceBranch) initializeNariGomaState

          /** 持ち駒をクリックして盤面に打つ場合の処理 */
          else if (useHandKomaBranch) useHandKomaFlow(num)
          /** 盤面の歩を移動させる場合の処理 */
          else if (inBoardKomaBranch(ClickedKomaState.Fu)) inBordKomaMoveFlow(ClickedKomaState.Fu, num)
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

          // デバッグ用
          println("existSelectedCellIndex:" + existSelectedCellIndex, "selectedCellIndex:" + selectedCellIndex, "stockNariIndex:" + stockNariIndex)
          println("optOnBoard:" + optOnBoard, "optOnBoardKomaState:" + optOnBoardKomaState)
          println("optIsSenteKoma:" + optIsSenteKoma, "optIsSenteKomaState:" + optIsSenteKomaState)
          println("optClickedKomaKind:" + optClickedKomaKind, "clickedKomaKind:" + clickedKomaKind)
          println("isSenteTurnState:" + isSenteTurnState,"num:" + num)
          println("")
        }

        case None => selectedCellIndex = Some(clickedIndex)
      }
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

  def allRandomBoard: Board = { //A
    val Zone = (0 until 80).toList
    val KomaPlace: List[Int] = scala.util.Random.shuffle(Zone)
    val allRandomBoard: Board = Board(List( //歩の下でランダムな初期の駒配置
      Koma(ClickedKomaState.Fu, KomaPlace(0), false, true), Koma(ClickedKomaState.Fu, KomaPlace(1), false, true), Koma(ClickedKomaState.Fu, KomaPlace(2), false, true),
      Koma(ClickedKomaState.Fu, KomaPlace(3), false, true), Koma(ClickedKomaState.Fu, KomaPlace(4), false, true), Koma(ClickedKomaState.Fu, KomaPlace(5), false, true),
      Koma(ClickedKomaState.Fu, KomaPlace(6), false, true), Koma(ClickedKomaState.Fu, KomaPlace(7), false, true), Koma(ClickedKomaState.Fu, KomaPlace(8), false, true),
      Koma(ClickedKomaState.Kyo, KomaPlace(9), false, true), Koma(ClickedKomaState.Kei, KomaPlace(10), false, true),
      Koma(ClickedKomaState.Gin, KomaPlace(11), false, true), Koma(ClickedKomaState.Kin, KomaPlace(12), false, true), Koma(ClickedKomaState.Ou, KomaPlace(13), false, true),
      Koma(ClickedKomaState.Kyo, KomaPlace(14), false, true), Koma(ClickedKomaState.Kei, KomaPlace(15), false, true), Koma(ClickedKomaState.Gin, KomaPlace(16), false, true), Koma(ClickedKomaState.Kin, KomaPlace(30), false, true),
      Koma(ClickedKomaState.Hisha, KomaPlace(17), false, true), Koma(ClickedKomaState.Kaku, KomaPlace(18), false, true),
      Koma(ClickedKomaState.Hisha, KomaPlace(19), true, true), Koma(ClickedKomaState.Kaku, KomaPlace(20), true, true),
      Koma(ClickedKomaState.Kyo, KomaPlace(21), true, true), Koma(ClickedKomaState.Kei, KomaPlace(22), true, true),
      Koma(ClickedKomaState.Gin, KomaPlace(23), true, true), Koma(ClickedKomaState.Kin, KomaPlace(24), true, true), Koma(ClickedKomaState.Ou, KomaPlace(25), true, true),
      Koma(ClickedKomaState.Kyo, KomaPlace(26), true, true), Koma(ClickedKomaState.Kei, KomaPlace(27), true, true), Koma(ClickedKomaState.Gin, KomaPlace(28), true, true), Koma(ClickedKomaState.Kin, KomaPlace(29), true, true),
      Koma(ClickedKomaState.Fu, KomaPlace(31), true, true), Koma(ClickedKomaState.Fu, KomaPlace(32), true, true), Koma(ClickedKomaState.Fu, KomaPlace(33), true, true),
      Koma(ClickedKomaState.Fu, KomaPlace(34), true, true), Koma(ClickedKomaState.Fu, KomaPlace(35), true, true), Koma(ClickedKomaState.Fu, KomaPlace(36), true, true),
      Koma(ClickedKomaState.Fu, KomaPlace(37), true, true), Koma(ClickedKomaState.Fu, KomaPlace(38), true, true), Koma(ClickedKomaState.Fu, KomaPlace(39), true, true)
    ))
    allRandomBoard
  }
  def halfRandomBoard: Board = { //B
    val (senteZone, goteZone) = ((0 until 35).toList, (45 until 80).toList)
    val (senteKomaPlace: List[Int], goteKomaPlace: List[Int]) = (scala.util.Random.shuffle(senteZone), scala.util.Random.shuffle(goteZone))
    val newInitalBoard: Board = Board(List( //歩の下でランダムな初期の駒配置
      Koma(ClickedKomaState.Fu, senteKomaPlace(11), false, true), Koma(ClickedKomaState.Fu, senteKomaPlace(12), false, true), Koma(ClickedKomaState.Fu, senteKomaPlace(13), false, true),
      Koma(ClickedKomaState.Fu, senteKomaPlace(14), false, true), Koma(ClickedKomaState.Fu, senteKomaPlace(15), false, true), Koma(ClickedKomaState.Fu, senteKomaPlace(16), false, true),
      Koma(ClickedKomaState.Fu, senteKomaPlace(17), false, true), Koma(ClickedKomaState.Fu, senteKomaPlace(18), false, true), Koma(ClickedKomaState.Fu, senteKomaPlace(19), false, true),
      Koma(ClickedKomaState.Kyo, senteKomaPlace(0), false, true), Koma(ClickedKomaState.Kei, senteKomaPlace(1), false, true),
      Koma(ClickedKomaState.Gin, senteKomaPlace(2), false, true), Koma(ClickedKomaState.Kin, senteKomaPlace(3), false, true), Koma(ClickedKomaState.Ou, senteKomaPlace(4), false, true),
      Koma(ClickedKomaState.Kyo, senteKomaPlace(5), false, true), Koma(ClickedKomaState.Kei, senteKomaPlace(6), false, true), Koma(ClickedKomaState.Gin, senteKomaPlace(7), false, true), Koma(ClickedKomaState.Kin, senteKomaPlace(8), false, true),
      Koma(ClickedKomaState.Hisha, senteKomaPlace(9), false, true), Koma(ClickedKomaState.Kaku, senteKomaPlace(10), false, true),
      Koma(ClickedKomaState.Hisha, goteKomaPlace(9), true, true), Koma(ClickedKomaState.Kaku, goteKomaPlace(10), true, true),
      Koma(ClickedKomaState.Kyo, goteKomaPlace(0), true, true), Koma(ClickedKomaState.Kei, goteKomaPlace(1), true, true),
      Koma(ClickedKomaState.Gin, goteKomaPlace(2), true, true), Koma(ClickedKomaState.Kin, goteKomaPlace(3), true, true), Koma(ClickedKomaState.Ou, goteKomaPlace(4), true, true),
      Koma(ClickedKomaState.Kyo, goteKomaPlace(5), true, true), Koma(ClickedKomaState.Kei, goteKomaPlace(6), true, true), Koma(ClickedKomaState.Gin, goteKomaPlace(7), true, true), Koma(ClickedKomaState.Kin, goteKomaPlace(8), true, true),
      Koma(ClickedKomaState.Fu, goteKomaPlace(11), true, true), Koma(ClickedKomaState.Fu, goteKomaPlace(12), true, true), Koma(ClickedKomaState.Fu, goteKomaPlace(13), true, true),
      Koma(ClickedKomaState.Fu, goteKomaPlace(14), true, true), Koma(ClickedKomaState.Fu, goteKomaPlace(15), true, true), Koma(ClickedKomaState.Fu, goteKomaPlace(16), true, true),
      Koma(ClickedKomaState.Fu, goteKomaPlace(17), true, true), Koma(ClickedKomaState.Fu, goteKomaPlace(18), true, true), Koma(ClickedKomaState.Fu, goteKomaPlace(19), true, true)
    ))
    newInitalBoard
  }
  def newUnderFuInitalBoard: Board = { //C
    val (senteZone, goteZone) = ((0 until 17).toList, (63 until 80).toList)
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
  def initalBoard: Board = Board(List( //D
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

}
