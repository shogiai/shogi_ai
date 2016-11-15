import com.atsfour.shogiai._

import scalafx.application.JFXApp
import scalafx.geometry.Pos
import scalafx.scene.control.Label
import scalafx.scene.text.Font
import scalafx.scene.{Group, Scene}
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Polygon, Rectangle}

/* やること
todo 1.持ち駒を打てる
todo 2.駒の成り不成の選択
*/

/** JFXApp { を使い、traitの設定をしつつ、*/
object ShogiBoard extends JFXApp {

  //持ち駒の場合、ここにルールを設定して置いておくようにしたい
  var board: Board = Board(List( //Koma("歩", 0, false)
    Koma("歩", 18, false, true), Koma("歩", 19, false, true), Koma("歩", 20, false, true),
    Koma("歩", 21, false, true), Koma("歩", 22, false, true), Koma("歩", 23, false, true),
    Koma("歩", 24, false, true), Koma("歩", 25, false, true), Koma("歩", 26, false, true),
    Koma("香", 0, false, true), Koma("桂", 1, false, true),
    Koma("銀", 2, false, true), Koma("金", 3, false, true), Koma("王", 4, false, true),
    Koma("香", 8, false, true), Koma("桂", 7, false, true), Koma("銀", 6, false, true), Koma("金", 5, false, true),
    Koma("飛", 10, false, true), Koma("角", 16, false, true), Koma("飛", 70, true, true), Koma("角", 64, true, true),
    Koma("香", 80, true, true), Koma("桂", 79, true, true),
    Koma("銀", 78, true, true), Koma("金", 77, true, true), Koma("王", 76, true, true),
    Koma("香", 72, true, true), Koma("桂", 73, true, true), Koma("銀", 74, true, true), Koma("金", 75, true, true),
    Koma("歩", 62, true, true), Koma("歩", 61, true, true), Koma("歩", 60, true, true),
    Koma("歩", 59, true, true), Koma("歩", 58, true, true), Koma("歩", 57, true, true),
    Koma("歩", 56, true, true), Koma("歩", 55, true, true), Koma("歩", 54, true, true),
    Koma("歩", 120, true, false), Koma("歩", 131, true, false), Koma("歩", 82, false, false)
  ))

  var selectedCellIndex: Option[Int] = None //複雑さを抑えるため、Noncellに変えたい・・
  var clickedKomaFlag: String = "NonTarget"
  var sengoKomaFlag: String = "NonFlag"
  var nextTurn: String = "Sente"

  /* `boardScene` という変数は、Sceneクラスをインスタンス化したもの
  `content` は、Sceneクラスの関数 `content = boardObj(board)`
  boardObj(board)は、 `def boardObj(board: Board) = {` を使っている */
  val boardScene = new Scene {
    fill = White
    content = boardObj(board)
  }

  /* `stage` は、trait JFXAppの中で、`null` で定義されてる */
  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Scala Shogi"
    width = 1850
    height = 800
    scene = boardScene
  }

  def boardObj(board: Board) = {
    val pane = new GridPane
    board.cells.zipWithIndex.foreach { /** 盤面, cellsを81回呼んでいる */
      case (optKoma, index) => {
        if (index <= 80) {
          val x = index % 9
          val y = index / 9
          pane.add(cellObj(optKoma, index), x, y)
        }
        if (index >= 81 && index <= 134) { /** 持ち駒を置く場所を確保 */
          val x = (index - 81) % 6 + 10
          val y = (index - 81) / 6
          pane.add(cellObj(optKoma, index), x, y) //負の座標は.addできない
        }
      }
    }
    pane
  }

  //セルの描画処理, ここの中で再描画される
  def cellObj(komaOpt: Option[Koma], clickedIndex: Int): Group = {

    //仮にsellIndexが存在する場合はLightBlue色、そうでない場合は、Burlywood色で塗り潰す
    val fillColor = if (selectedCellIndex.contains(clickedIndex) && clickedKomaFlag != "NonTarget" ) {
      LightBlue
    } else if (clickedIndex <= 80 //盤面
      || ((clickedIndex >= 81 && clickedIndex <= 134) && (clickedIndex-81) % 6 != 0 && (clickedIndex-81) / 6 != 4) //持ち駒
    ){ //色をつける場所を指定
      Burlywood
    } else White

    val grid = {
      val rect = Rectangle(80, 80, fillColor)
      if (clickedIndex <= 80 //盤面
        || ((clickedIndex >= 81 && clickedIndex <= 134) && (clickedIndex-81) % 6 != 0 && (clickedIndex-81) / 6 != 4) //持ち駒
      ){ rect.setStroke(Black) }
      rect
    }

    //先後表示のためのインスタンス化の切り分け
    val group = komaOpt match {
      case Some(num) => {
        komaOpt.get.isSente match {
          case true => new Group { children = List(Some(grid), komaOpt.map(senteKomaObj)).flatten }
          case false => new Group { children = List(Some(grid), komaOpt.map(goteKomaObj)).flatten }
        }
      }
      case None => new Group { children = List(Some(grid), komaOpt.map(senteKomaObj)).flatten }
    }

    /** todo ここらのdefは、Koma,Boardクラスで関数定義できるかも */
    /** 駒をクリックした時に使う関数のまとまり */
    //clickedIndexと一致した場合の駒情報を取得する
    val clickedKoma = if (komaOpt != None) { if (komaOpt.get.index == clickedIndex) { komaOpt.get.kind } } else { "noneKoma" }
    val isSenteKoma = if (komaOpt != None) { if (komaOpt.get.index == clickedIndex) { komaOpt.get.isSente } } else { "noneKoma" }

    def senteKomaBranch(koma: String): Boolean = {
      ((clickedKoma == koma && clickedKomaFlag == "NonTarget") || clickedKomaFlag == koma) &&
        (isSenteKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente"
    }
    def goteKomaBranch(koma: String): Boolean = {
      ((clickedKoma == koma && clickedKomaFlag == "NonTarget") || clickedKomaFlag == koma) &&
        (isSenteKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote"
    }
    def addFlag(komaKind: String, komaOwner: String) = {
      clickedKomaFlag = komaKind
      sengoKomaFlag = komaOwner
    }
    def clickCancel = {
      selectedCellIndex = None
      clickedKomaFlag = "NonTarget"
    }

    def handMove: Int = { //持ち駒をどこに置くかを決める
      var (handPlace,count) = (108, 0)
      if (nextTurn == "Sente") {
        for (handsPlace <- 111 until 135) {
          if (board.findKoma(handsPlace) == None && (handsPlace - 81) % 6 != 0 ) handPlace = handsPlace
        }
      } else if (nextTurn == "Gote") {
        for (handsPlace <- 81 until 105) {
          if (board.findKoma(handsPlace) == None && (handsPlace - 81) % 6 != 0 && count == 0) {
            handPlace = handsPlace
            count = count  + 1
          }
        }
      }
      handPlace
    }
    def takeKoma(clickedIndex: Int) = { //駒を取った時に行う処理の集まり
      board = board.returnNariKoma(clickedIndex)
      if (nextTurn == "Sente") board = board.ownerChangeKoma(clickedIndex, false)
      else if (nextTurn == "Gote") board = board.ownerChangeKoma(clickedIndex, true)
      board = board.moveKoma(clickedIndex, handMove)
    }

    def switchTurn(nextTurn: String): String = if (nextTurn == "Sente") {"Gote"} else {"Sente"}
    def canNari: Boolean = {
      var canNari: Boolean = false
      if (nextTurn == "Sente") { canNari = (clickedIndex / 9 < 3) }
      else if(nextTurn == "Gote") { canNari = (clickedIndex / 9 >= 6) }
      canNari
    }

    def playAndInitializeAndNari(nariGoma: String, num: Int) = {
      selectedCellIndex = None
      clickedKomaFlag = "NonTarget"
      sengoKomaFlag = "NonFlag"
      board = board.moveKoma(num, clickedIndex)
      if (canNari) {
        println("CanNariは正しい")
        board = board.nariKoma(clickedIndex, nariGoma)
      }
      nextTurn = switchTurn(nextTurn)
    }
    def playAndInitialize(num: Int) = {
      selectedCellIndex = None
      clickedKomaFlag = "NonTarget"
      sengoKomaFlag = "NonFlag"
      board = board.moveKoma(num, clickedIndex)
      nextTurn = switchTurn(nextTurn)
    }
    /** ここまで駒をクリックした時に使われる関数群 */

    /** boardに、selectedCellIndex(選択したセルのIndex)をboard変数に移動させ、repaint関数で 再描画する */
    if (selectedCellIndex != None) { /** クリックされてる場合 */
      val existSelectedCellIndex = selectedCellIndex.get
      val absMoveDistance = Math.abs(existSelectedCellIndex - clickedIndex) //駒の移動距離の絶対値を定義
      val moveDistance = existSelectedCellIndex - clickedIndex //駒の移動距離を定義

      group.setOnMouseClicked(e => {
        selectedCellIndex match { //ここで更新されるのは、eの方。selectedCellIndexではない
          case Some(num) => { /** 各々の駒の動く条件に一致する場合はindexへ移動させる */

              // デバッグ用
              println("clickedKoma:" + clickedKoma, "clickedKomaFlag:" + clickedKomaFlag, "isSenteKoma:" + isSenteKoma, "senteKomaFlag:" + sengoKomaFlag)
              println("selectedCellIndex:" + selectedCellIndex, "nextTurn:" + nextTurn)

              if (senteKomaBranch("歩")) { /** 先手の歩の場合 */
                /** クリックした時に一度入ったというフラグを用意(行き先をクリックした時、駒を認識できなくなっている) */
                addFlag("歩", "先手")
                if (moveDistance == 9) {
                  //駒の移動条件を満たしていて、移動先に駒がない場合に、移動できる
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitializeAndNari("と", num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("歩")) { /** 後手の歩の場合 */
                addFlag("歩", "後手")
                if (moveDistance == -9) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitializeAndNari("と", num)
                } else if (moveDistance != 0) clickCancel
              }

              else if (senteKomaBranch("香")) { /** 先手の香車の場合 */
                addFlag("香", "先手")
                if (moveDistance % 9 == 0 && moveDistance > 0 && board.upJumpCheck(num, clickedIndex)) {
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitializeAndNari("杏", num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("香")) { /** 後手の香車の場合 */
                addFlag("香", "後手")
                if (moveDistance % 9 == 0 && moveDistance < 0 && board.downJumpCheck(num, clickedIndex)) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitializeAndNari("杏", num)
                } else if (moveDistance != 0) clickCancel
              }

              else if (senteKomaBranch("桂")) { /** 先手の桂馬の場合 */
                addFlag("桂", "先手")
                if (moveDistance == 17 || moveDistance == 19) {
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitializeAndNari("圭", num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("桂")) { /** 後手の桂馬の場合 */
                addFlag("桂", "後手")
                if (moveDistance == -17 || moveDistance == -19) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitializeAndNari("圭", num)
                } else if (moveDistance != 0) clickCancel
              }

              else if (senteKomaBranch("銀")) { /** 先手の銀の場合 */
                addFlag("銀", "先手")
                if (absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == 9) {
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitializeAndNari("全", num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("銀")) { /** 後手の銀の場合 */
                addFlag("銀", "後手")
                if (absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == -9) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitializeAndNari("全", num)
                } else if (moveDistance != 0) clickCancel
              }

              else if (senteKomaBranch("金")) { /** 先手の金の場合 */
                addFlag("金", "先手")
                if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) {
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("金")) { /** 後手の金の場合 */
                addFlag("金", "後手")
                if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }

              else if (senteKomaBranch("王")) { /** 先手の王の場合 */
                addFlag("王", "先手")
                if (absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) {
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("王")) { /** 後手の王の場合 */
                addFlag("王", "後手")
                if (absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }

              else if (senteKomaBranch("角")) { /** 先手の角の場合 */
                addFlag("角", "先手")
                if ((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex)) //左上から右下方向
                  || absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex)) { //右上から左下方向
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitializeAndNari("馬", num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("角")) { /** 後手の角の場合 */
                addFlag("角", "後手")
                if ((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex))
                  || absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex)) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitializeAndNari("馬", num)
                } else if (moveDistance != 0) clickCancel
              }

              else if (senteKomaBranch("飛")) { /** 先手の飛車の場合 */
                addFlag("飛", "先手")
                if ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
                  || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex)) //横方向
                ) {
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitializeAndNari("龍", num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("飛")) { /** 後手の飛車の場合 */
                addFlag("飛", "後手")
                if ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex))
                  || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex))) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitializeAndNari("龍", num)
                } else if (moveDistance != 0) clickCancel
              }

              /** 成り駒 */
              else if (senteKomaBranch("と")) { /** 先手のとの場合 */
                addFlag("と", "先手")
                if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) {
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("と")) { /** 後手のとの場合 */
                addFlag("と", "後手")
                if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (senteKomaBranch("杏")) { /** 先手の成香の場合 */
                addFlag("杏", "先手")
                if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) {
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("杏")) { /** 後手の成香の場合 */
                addFlag("杏", "後手")
                if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (senteKomaBranch("圭")) { /** 先手の成桂の場合 */
                addFlag("圭", "先手")
                if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) {
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("圭")) { /** 後手の成桂の場合 */
                addFlag("圭", "後手")
                if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (senteKomaBranch("全")) { /** 先手の成銀の場合 */
                addFlag("全", "先手")
                if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) {
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("全")) { /** 後手の成銀の場合 */
                addFlag("全", "後手")
                if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (senteKomaBranch("馬")) { /** 先手の馬の場合 */
                addFlag("馬", "先手")
                if ((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex)) //左上~右下
                  || (absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex)) //左下~右上
                  || absMoveDistance == 1 || absMoveDistance == 9) {
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("馬")) { /** 後手の馬の場合 */
                addFlag("馬", "後手")
                if ((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, clickedIndex) && board.rightDownJumpCheck(num, clickedIndex))
                  || (absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, clickedIndex) && board.leftDownJumpCheck(num, clickedIndex))
                  || absMoveDistance == 1 || absMoveDistance == 9) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (senteKomaBranch("龍")) { /** 先手の龍の場合 */
                addFlag("龍", "先手")
                if ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex)) //縦(上下)方向
                  || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex)) //横方向
                  || absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) {
                  if (isSenteKoma == false) takeKoma(clickedIndex)
                  if (isSenteKoma != true) playAndInitialize(num)
                } else if (moveDistance != 0) clickCancel
              }
              else if (goteKomaBranch("龍")) { /** 後手の龍の場合 */
                addFlag("龍", "後手")
                if ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, clickedIndex) && board.downJumpCheck(num, clickedIndex))
                  || (existSelectedCellIndex / 9 == clickedIndex / 9 && board.rightJumpCheck(num, clickedIndex) && board.leftJumpCheck(num, clickedIndex))
                  || absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) {
                  if (isSenteKoma == true) takeKoma(clickedIndex)
                  if (isSenteKoma != false) playAndInitialize(num)
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

  /** 先手駒の描画関数 */
  /* 駒の形、置き場所をセットで定義、その値を返す */
  def senteKomaObj(koma: Koma): Group = {
    val senteKomaShape = { //駒の形を定義している
    val poly = Polygon(40, 10, 60, 20, 70, 70, 10, 70, 20, 20)
      poly.setFill(Sienna)
      poly.setStroke(Black)
      poly
    }
    val komaLabel = { //升ないの駒の置き場所を定義してる
    val label = new Label
      label.setText(koma.kind)
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

  /** 後手駒の描画関数 */
  def goteKomaObj(koma: Koma): Group = {
    val goteKomaShape = { //駒の形を定義している
    val poly = Polygon(20, 20, 60, 70, 70, 20, 10, 60, 10, 40)
      poly.setFill(Sienna)
      poly.setStroke(Black)
      poly
    }
    val komaLabel = { //升ないの駒の置き場所を定義してる
    val label = new Label
      label.setText(koma.kind)
      label.setFont(Font(30))
      label.setMaxSize(30, 30)
      label.setLayoutX(25)
      label.setLayoutY(25)
      label.setAlignment(Pos.Center)
      label
    }
    val obj = new Group(goteKomaShape, komaLabel)
    obj
  }

  def repaint: Unit = {
    boardScene.content = boardObj(board)
  }

}
