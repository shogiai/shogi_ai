package com.atsfour.shogiai

import scalafx.application.JFXApp
import scalafx.geometry.Pos
import scalafx.scene.control.Label
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Polygon, Rectangle}
import scalafx.scene.text.Font
import scalafx.scene.{Group, Scene}


// todo 駒の成り不成の選択
/** JFXApp { を使い、traitの設定をしつつ、*/
object ShogiBoard extends JFXApp {

  //駒の初期配置
  var board: Board = Board(List(
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
  var selectedCellIndex: Option[Int] = None
  var optIsSenteKomaState: Option[Boolean] = None
  var optOnBoardKomaState: Option[Boolean] = None
  var isSenteTurnState: Boolean = true

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

    lazy val values = Seq(None, Fu, Kyo, Kei, Gin, Kin, Ou, Kaku, Hisha, To, NariKyo, NariKei, NariGin, Uma, Ryu)
  }

  /* `boardScene` という変数は、Sceneクラスをインスタンス化したもの
  `content` は、Sceneクラスの関数 `content = boardObj(board)`
  boardObj(board)は、 `def boardObj(board: Board) = {` を使っている */
  val boardScene = new Scene {
    fill = White
    content = boardObjPane
  }

  /* `stage` は、trait JFXAppの中で、`null` で定義されてる */
  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Scala Shogi"
    width = 1850
    height = 800
    scene = boardScene
  }

  def boardObjPane = {
    val pane = new GridPane
    board.cells.zipWithIndex.foreach { /** 盤面, cellsを81回呼んでいる */
      case (optKoma, index) => {
        if (index <= 80) {
          val x = index % 9
          val y = index / 9
          pane.add(cellObjGroup(optKoma, index), x, y)
        }
        if (index >= 81 && index <= 134) { /** 持ち駒を置く場所を確保 */
        val x = (index - 81) % 6 + 10
          val y = (index - 81) / 6
          pane.add(cellObjGroup(optKoma, index), x, y) //負の座標は.addできない
        }
      }
    }
    pane
  }

  //セルの描画処理, ここの中で再描画される
  def cellObjGroup(komaOpt: Option[Koma], clickedIndex: Int): Group = {

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

    /** todo ここらのdefは、Koma,Boardクラスで関数定義にもっと移行できるかも */
    /** 駒をクリックした時に使う関数のまとまり */

    /* clickedIndexと一致した場合の駒情報を取得する (komaの中の、ClickedIndexと一致する、という条件が必要(本当は)) */
    val optClickedKoma: Option[ClickedKomaState] = komaOpt.map(koma => koma.kind)
    val optIsSente: Option[Boolean] = komaOpt.map(koma => koma.isSente)
    val optOnBoard: Option[Boolean] = komaOpt.map(koma => koma.onBoard)

    def senteHandKomaBranch: Boolean = {
      ((optOnBoard.contains(false) && clickedKomaKind == clickedKomaStates.None) || optOnBoardKomaState.contains(false)) &&
        (optIsSente.contains(true) || optIsSenteKomaState.contains(true)) && isSenteTurnState && !optOnBoardKomaState.contains(true)
    }
    def goteHandKomaBranch: Boolean = {
      ((optOnBoard.contains(false) && clickedKomaKind == clickedKomaStates.None) || optOnBoardKomaState == Option(false)) &&
        (optIsSente.contains(false) || optIsSenteKomaState.contains(false)) && !isSenteTurnState && !optOnBoardKomaState.contains(true)
    }

    def senteKomaBranch(koma: ClickedKomaState): Boolean = {
      ((optClickedKoma.contains(koma) && clickedKomaKind == clickedKomaStates.None) || clickedKomaKind == koma) &&
        (optIsSente.contains(true) || optIsSenteKomaState.contains(true)) && isSenteTurnState && !optOnBoardKomaState.contains(false)
    }
    def goteKomaBranch(koma: ClickedKomaState): Boolean = {
      ((optClickedKoma.contains(koma) && clickedKomaKind == clickedKomaStates.None) || clickedKomaKind == koma) &&
        (optIsSente.contains(false) || optIsSenteKomaState.contains(false)) && !isSenteTurnState && !optOnBoardKomaState.contains(false)
    }

    def addState(komaKind: ClickedKomaState, komaOwner: Option[Boolean], onBoard: Option[Boolean]) = {
      clickedKomaKind = komaKind
      optIsSenteKomaState = komaOwner
      optOnBoardKomaState = onBoard
    }
    def clickCancel = {
      selectedCellIndex = None
      optIsSenteKomaState = None
      clickedKomaKind = clickedKomaStates.None
      optOnBoardKomaState = None
    }

    //持ち駒をどこに置くかを決める, 持ち駒をソートする機能があると見栄えが良い
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
    def takeKoma(clickedIndex: Int) = { //駒を取った時に行う処理の集まり
      board = board.returnNariKoma(clickedIndex)
      board = board.ownerChangeKoma(clickedIndex, optIsSente.contains(true))
      board = board.spaceChangeKoma(clickedIndex, optOnBoard.contains(true)) //取られた駒は持ち駒になる
      board = board.moveKoma(clickedIndex, handMove)
    }

    def toMoveBoard: Boolean = clickedIndex <= 80
    def switchTurn(nextTurn: Boolean): Boolean = if (nextTurn) false else true

    def canNari(num: Int): Boolean = isSenteTurnState match {
      case true => (clickedIndex / 9) + 1 <= 3 || (num / 9) + 1 <= 3   //3段目以内にいた、もしくは3段目以内に入った
      case false => ((clickedIndex / 9) + 1 >= 7) || (num / 9) + 1 >= 7
    }

    def playAndInitializeAndNari(nariGoma: ClickedKomaState, num: Int) = {
      selectedCellIndex = None
      clickedKomaKind = clickedKomaStates.None
      optIsSenteKomaState = None
      board = board.moveKoma(num, clickedIndex)
      if (canNari(num: Int)) board = board.nariKoma(clickedIndex, nariGoma)
      isSenteTurnState = switchTurn(isSenteTurnState)
      optOnBoardKomaState = None
    }
    def playAndInitialize(num: Int) = {
      selectedCellIndex = None
      clickedKomaKind = clickedKomaStates.None
      optIsSenteKomaState = None
      board = board.moveKoma(num, clickedIndex)
      if (optOnBoardKomaState.contains(false))
        board = board.spaceChangeKoma(clickedIndex, optOnBoard.contains(false)) //打ち終わった駒は盤上の駒になる
      isSenteTurnState = switchTurn(isSenteTurnState)
      optOnBoardKomaState = None
    }
    /** ここまで駒をクリックした時に使われる関数群 */

    /** boardに、selectedCellIndex(選択したセルのIndex)をboard変数に移動させ、repaint関数で 再描画する */
    if (selectedCellIndex.isDefined) { /** クリックされてる場合 */
    val existSelectedCellIndex = selectedCellIndex.getOrElse(-1)
      val absMoveDistance = Math.abs(existSelectedCellIndex - clickedIndex) //駒の移動距離の絶対値を定義
      val moveDistance = existSelectedCellIndex - clickedIndex //駒の移動距離を定義

      //todo group.setOnMouseClickedで増えた重複コードを減らす
      //todo 移動条件がほぼベタ書きなのを変えたい
      group.setOnMouseClicked(e => {
        selectedCellIndex match { //ここで更新されるのは、eの方。selectedCellIndexではない
          case Some(num) => { /** 各々の駒の動く条件に一致する場合はindexへ移動させる */

            // デバッグ用
            println("optClickedKoma:" + optClickedKoma, "clickedKomaFlag:" + clickedKomaKind, "optIsSente:" + optIsSente, "senteKomaFlag:" + optIsSenteKomaState)
            println("selectedCellIndex:" + selectedCellIndex, "nextTurn:" + isSenteTurnState, "clickedIndex:" + clickedIndex)
            println("onBoardKomaFlag:" + optOnBoardKomaState, "optOnBoard:" + optOnBoard)

            if (senteHandKomaBranch) { /** 先手で持ち駒をクリックした、選択していた場合, 移動条件のfalseの範囲に注意 */
              if (clickedIndex <= 80 && optIsSente.isEmpty &&
                (!((clickedKomaKind == clickedKomaStates.Fu || clickedKomaKind == clickedKomaStates.Kyo) && (clickedIndex / 9) + 1 == 1)) && //先手の歩と香車は、1段目に打てない
                !(clickedKomaKind == clickedKomaStates.Kei && (clickedIndex / 9 + 1) <= 2) && //先手の桂馬は、1段目と2段目に打てない
                (clickedKomaKind != clickedKomaStates.Fu || board.nifuCheck(clickedIndex, optIsSenteKomaState.contains(true)))
               ) {
                playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
              if (optOnBoard.contains(false) && clickedKomaKind == clickedKomaStates.None)
                addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
            }
            else if (goteHandKomaBranch) { /** 後手で持ち駒をクリックした、選択していた場合, 移動条件のfalseの範囲に注意 */
              if ( clickedIndex <= 80 && optIsSente.isEmpty &&
                (!((clickedKomaKind == clickedKomaStates.Fu || clickedKomaKind == clickedKomaStates.Kyo) && (clickedIndex / 9) + 1 == 9)) && //後手の歩と香車は、9段目に打てない
                (!(clickedKomaKind == clickedKomaStates.Kei && (clickedIndex / 9 + 1) >= 8)) && //後手の桂馬は、8段目と9段目に打てない
                (clickedKomaKind != clickedKomaStates.Fu || board.nifuCheck(clickedIndex, optIsSenteKomaState.contains(true))) //先手ならtrue, 後手ならfalseを返す
              ) {
                playAndInitialize(num)
              }
              else if (moveDistance != 0) clickCancel
              if (optOnBoard.contains(false) && clickedKomaKind == clickedKomaStates.None)
                addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
            }

            /** 持ち駒でない場合、盤上の駒なので、処理に入る */
            else if (senteKomaBranch(clickedKomaStates.Fu)) { /** 先手の歩の場合 */
              /** クリックした時に一度入ったというフラグを用意(行き先をクリックした時、駒を認識できなくなっている) */
              addState(clickedKomaStates.Fu, optIsSente, optOnBoard)
              if (moveDistance == 9 && toMoveBoard) {
                //駒の移動条件を満たしていて、移動先に駒がない場合に、移動できる
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitializeAndNari(clickedKomaStates.To, num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.Fu)) { /** 後手の歩の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if (moveDistance == -9 && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitializeAndNari(clickedKomaStates.To, num)
              } else if (moveDistance != 0) clickCancel
            }

            else if (senteKomaBranch(clickedKomaStates.Kyo)) { /** 先手の香車の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if (moveDistance % 9 == 0 && moveDistance > 0 && board.upJumpCheck(num, clickedIndex) && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitializeAndNari(clickedKomaStates.NariKyo, num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.Kyo)) { /** 後手の香車の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if (moveDistance % 9 == 0 && moveDistance < 0 && board.downJumpCheck(num, clickedIndex) && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitializeAndNari(clickedKomaStates.NariKyo, num)
              } else if (moveDistance != 0) clickCancel
            }

            else if (senteKomaBranch(clickedKomaStates.Kei)) { /** 先手の桂馬の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if (moveDistance == 17 || moveDistance == 19 && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitializeAndNari(clickedKomaStates.NariKei, num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.Kei)) { /** 後手の桂馬の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if (moveDistance == -17 || moveDistance == -19 && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitializeAndNari(clickedKomaStates.NariKei, num)
              } else if (moveDistance != 0) clickCancel
            }

            else if (senteKomaBranch(clickedKomaStates.Gin)) { /** 先手の銀の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == 9) && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitializeAndNari(clickedKomaStates.NariGin, num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.Gin)) { /** 後手の銀の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == -9) && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitializeAndNari(clickedKomaStates.NariGin, num)
              } else if (moveDistance != 0) clickCancel
            }

            else if (senteKomaBranch(clickedKomaStates.Kin)) { /** 先手の金の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.Kin)) { /** 後手の金の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }

            else if (senteKomaBranch(clickedKomaStates.Ou)) { /** 先手の王の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.Ou)) { /** 後手の王の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }

            else if (senteKomaBranch(clickedKomaStates.Kaku)) { /** 先手の角の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if (((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex)) //左上から右下方向
                || absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex)) //右上から左下方向
                && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitializeAndNari(clickedKomaStates.Uma, num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.Kaku)) { /** 後手の角の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if (((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex))
                || absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex))
                && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitializeAndNari(clickedKomaStates.Uma, num)
              } else if (moveDistance != 0) clickCancel
            }

            else if (senteKomaBranch(clickedKomaStates.Hisha)) { /** 先手の飛車の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
                || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex)) //横方向
                && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitializeAndNari(clickedKomaStates.Ryu, num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.Hisha)) { /** 後手の飛車の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex))
                || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex))
                && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitializeAndNari(clickedKomaStates.Ryu, num)
              } else if (moveDistance != 0) clickCancel
            }

            /** 成り駒 */
            else if (senteKomaBranch(clickedKomaStates.To)) { /** 先手のとの場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.To)) { /** 後手のとの場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (senteKomaBranch(clickedKomaStates.NariKyo)) { /** 先手の成香の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.NariKyo)) { /** 後手の成香の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (senteKomaBranch(clickedKomaStates.NariKei)) { /** 先手の成桂の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.NariKei)) { /** 後手の成桂の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (senteKomaBranch(clickedKomaStates.NariGin)) { /** 先手の成銀の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.NariGin)) { /** 後手の成銀の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if ((absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (senteKomaBranch(clickedKomaStates.Uma)) { /** 先手の馬の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if (((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex)) //左上~右下
                || (absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex)) //左下~右上
                || absMoveDistance == 1 || absMoveDistance == 9) && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.Uma)) { /** 後手の馬の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if (((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex))
                || (absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex))
                || absMoveDistance == 1 || absMoveDistance == 9) && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (senteKomaBranch(clickedKomaStates.Ryu)) { /** 先手の龍の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if (((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
                || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex)) //横方向
                || absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) && toMoveBoard) {
                if (optIsSente.contains(false)) takeKoma(clickedIndex)
                if (!optIsSente.contains(true)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }
            else if (goteKomaBranch(clickedKomaStates.Ryu)) { /** 後手の龍の場合 */
              addState(optClickedKoma.getOrElse(clickedKomaStates.None), optIsSente, optOnBoard)
              if (((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex))
                || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex))
                || absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) && toMoveBoard) {
                if (optIsSente.contains(true)) takeKoma(clickedIndex)
                if (!optIsSente.contains(false)) playAndInitialize(num)
              } else if (moveDistance != 0) clickCancel
            }

          }
          case None => selectedCellIndex = Some(clickedIndex)
        }
        repaint //再描画関数(一番下で定義)
      })
      group

    } else { /** クリックされない場合(初期画面表示) */
      group.setOnMouseClicked(e => {
        selectedCellIndex match {
          case Some(num) => {
            board = board.moveKoma(num, clickedIndex)
            selectedCellIndex = None
          }
          case None => selectedCellIndex = Some(clickedIndex)
        }
        repaint //再描画関数(一番下で定義する)
      })
      group
    }
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

  def repaint: Unit = {
    boardScene.content = boardObjPane
  }

}
