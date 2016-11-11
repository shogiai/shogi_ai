import com.atsfour.shogiai.{Koma, Board}

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.geometry.Pos
import scalafx.scene.control.Label
import scalafx.scene.text.Font
import scalafx.scene.{Group, Scene}
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Polygon, Rectangle}

/* やること
todo 1.持ち駒を定義する
todo 2.相手の駒を取れるようにする
todo 3.持ち駒を打てる
todo 4.駒の成り不成の選択
*/

/** JFXApp { を使い、traitの設定をしつつ、*/
object ShogiBoard extends JFXApp {
  var board: Board = Board(List( //Koma("歩", 0, false)
    Koma("歩", 18, false), Koma("歩", 19, false), Koma("歩", 20, false), Koma("歩", 21, false), Koma("歩", 22, false),
    Koma("歩", 23, false), Koma("歩", 24, false), Koma("歩", 25, false), Koma("歩", 26, false),
    Koma("香", 0, false), Koma("桂", 1, false), Koma("銀", 2, false), Koma("金", 3, false), Koma("王", 4, false),
    Koma("香", 8, false), Koma("桂", 7, false), Koma("銀", 6, false), Koma("金", 5, false),
    Koma("飛", 10, false), Koma("角", 16, false), Koma("飛", 70, true), Koma("角", 64, true),
    Koma("香", 80, true), Koma("桂", 79, true), Koma("銀", 78, true), Koma("金", 77, true), Koma("王", 76, true),
    Koma("香", 72, true), Koma("桂", 73, true), Koma("銀", 74, true), Koma("金", 75, true),
    Koma("歩", 62, true), Koma("歩", 61, true), Koma("歩", 60, true), Koma("歩", 59, true), Koma("歩", 58, true),
    Koma("歩", 57, true), Koma("歩", 56, true), Koma("歩", 55, true), Koma("歩", 54, true)
  ))
  var selectedCellIndex: Option[Int] = None //複雑さを抑えるため、Noncellに変えたい…
  var clickedKomaFlag: String = "NonTarget"
  var sengoKomaFlag: String = "NonFlag"
  var nextTurn: String = "Sente"

  val boardScene = new Scene {
    fill = White
    content = boardObj(board)
  }

  /* `stage` は、trait JFXAppの中で、`null` で定義されてる */
  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Scala Shogi"
    width = 800
    height = 800
    scene = boardScene
  }

  def boardObj(board: Board) = {
    val pane = new GridPane
    board.cells.zipWithIndex.foreach { //81回繰り返される
      case (optKoma, index) => {
        val x = index % 9
        val y = index / 9
        pane.add(cellObj(optKoma, index), x, y) //ここでdef cellObjが81回呼ばれる
      }
    }
    pane
  }

  //セルの描画処理, ここの中で再描画される
  def cellObj(komaOpt: Option[Koma], index: Int): Group = {

    val fillColor = if (selectedCellIndex.contains(index) && clickedKomaFlag!= "NonTarget" ) LightBlue else Burlywood
    val grid = {
      val rect = Rectangle(80, 80, fillColor)
      rect.setStroke(Black)
      rect
    }

    /** clickedAtと一致した場合の駒情報を取得する */
    val clickedAt = if (selectedCellIndex != None) { selectedCellIndex.get }
    val clickedKoma = if (komaOpt != None) { if (komaOpt.get.index == clickedAt) { komaOpt.get.kind } } else { "noneKoma" }
    val sengoKoma = if (komaOpt != None) { if (komaOpt.get.index == clickedAt) { komaOpt.get.isSente } } else { "noneKoma" }

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

    if (selectedCellIndex != None) { /** クリックされてる場合 */
      val existSelectedCellIndex = selectedCellIndex.get
      //val index = index.get

      group.setOnMouseClicked(e => {
        selectedCellIndex match { //ここで更新されるのは、eの方。selectedCellIndexではない
          case Some(num) => { /** 各々の駒の動く条件に一致する場合はindexへ移動させる */
            val absMoveDistance = Math.abs(existSelectedCellIndex - index) //駒の移動距離の絶対値を定義
            val moveDistance = existSelectedCellIndex - index //駒の移動距離を定義
            //デバッグ用
            println("clickedKoma:"+clickedKoma,"clickedKomaFlag:"+clickedKomaFlag,"sengoKoma:"+sengoKoma,"senteKomaFlag:"+sengoKomaFlag)
            println("nextTurn:"+nextTurn)
            println("clickedAt:"+clickedAt, "selectedCellIndex:"+selectedCellIndex)

            //行き先をクリックした時点では、すでに駒と認識できなくなっている
            if ( (clickedKoma == "歩" || clickedKomaFlag == "歩") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente" ) { /** 先手の歩の場合 */
              clickedKomaFlag = "歩"  /** クリックした時に一度入ったというフラグを用意 */
              sengoKomaFlag = "先手"
              if (moveDistance == 9) {
                if (sengoKoma != true) { //駒の移動条件を満たしていて、移動先に駒がない場合に、移動できる
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget" //コードを複雑にしないように
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                  if (index /9 < 3) board = board.nariKoma(index, "と") //成り
                }
              }
            }
            else if ( (clickedKoma == "歩" || clickedKomaFlag == "歩") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote" ) { /** 後手の歩の場合 */
              clickedKomaFlag = "歩"  /** クリックした時に一度入ったというフラグを用意 */
              sengoKomaFlag = "後手"
              if (moveDistance == -9) {
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                  if (index /9 >= 6) board = board.nariKoma(index, "と")
                }
              }
            }

            else if ((clickedKoma == "香" || clickedKomaFlag == "香") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente" )  { /** 先手の香車の場合 */
              clickedKomaFlag = "香"
              sengoKomaFlag = "先手"
              if ( moveDistance % 9 == 0 && moveDistance > 0) {
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                  if (index /9 < 3) board = board.nariKoma(index, "杏")
                }
              }
            }
            else if ((clickedKoma == "香" || clickedKomaFlag == "香") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote" )  { /** 後手の香車の場合 */
              clickedKomaFlag = "香"
              sengoKomaFlag = "後手"
              if ( moveDistance % 9 == 0 && moveDistance < 0 && nextTurn == "Sente" ) { //先手の場合は負、後手の場合は正のとき
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                  if (index /9 >= 6) board = board.nariKoma(index, "杏")
                }
              }
            }

            else if ((clickedKoma == "桂" || clickedKomaFlag == "桂") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente" ) { /** 先手の桂馬の場合 */
              clickedKomaFlag = "桂"
              sengoKomaFlag = "先手"
              if (moveDistance == 17 || moveDistance == 19) {
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                  if (index /9 < 3) board = board.nariKoma(index, "圭")
                }
              }
            }
            else if ((clickedKoma == "桂" || clickedKomaFlag == "桂") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote" ) { /** 後手の桂馬の場合 */
              clickedKomaFlag = "桂"
              sengoKomaFlag = "後手"
              if (moveDistance == -17 || moveDistance == -19) {
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                  if (index /9 >= 6) board = board.nariKoma(index, "圭")
                }
              }
            }

            else if ((clickedKoma == "銀" || clickedKomaFlag == "銀") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente") { /** 先手の銀の場合 */
              clickedKomaFlag = "銀"
              sengoKomaFlag = "先手"
              if (absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == 9 && nextTurn == "Gote") { //先手駒の場合
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                  if (index /9 < 3) board = board.nariKoma(index, "全")
                }
              }
            }
            else if ((clickedKoma == "銀" || clickedKomaFlag == "銀") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote") { /** 後手の銀の場合 */
              clickedKomaFlag = "銀"
              sengoKomaFlag = "後手"
              if (absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == -9 && nextTurn == "Sente") { //後手駒の場合
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                  if (index /9 >= 6) board = board.nariKoma(index, "全")
                }
              }
            }

            else if ((clickedKoma == "金" || clickedKomaFlag == "金") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente") { /** 先手の金の場合 */
              clickedKomaFlag = "金"
              sengoKomaFlag = "先手"
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) { //先手駒
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                  if (index /9 < 3) board = board.nariKoma(index, "金")
                }
              }
            }
            else if ((clickedKoma == "金" || clickedKomaFlag == "金") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote") { /** 後手の金の場合 */
              clickedKomaFlag = "金"
              sengoKomaFlag = "後手"
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) { //後手駒
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                  if (index /9 >= 6) board = board.nariKoma(index, "金")
                }
              }
            }

            else if ((clickedKoma == "王" || clickedKomaFlag == "王") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente") { /** 先手の王の場合 */
              clickedKomaFlag = "王"
              sengoKomaFlag = "先手"
              if (absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) {
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                }
              }
            }
            else if ((clickedKoma == "王" || clickedKomaFlag == "王") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote") { /** 後手の王の場合 */
              clickedKomaFlag = "王"
              sengoKomaFlag = "後手"
              if (absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) {
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                }
              }
            }

            else if ((clickedKoma == "角" || clickedKomaFlag == "角") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente") { /** 先手の角の場合*/
              clickedKomaFlag = "角"
              sengoKomaFlag = "先手"
              if (absMoveDistance % 10 == 0 || absMoveDistance % 8 == 0) {
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                  if (index /9 < 3) board = board.nariKoma(index, "馬")
                }
              }
            }
            else if ((clickedKoma == "角" || clickedKomaFlag == "角") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote") { /** 後手の角の場合*/
              clickedKomaFlag = "角"
              sengoKomaFlag = "後手"
              if (absMoveDistance % 10 == 0 || absMoveDistance % 8 == 0) {
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                  if (index /9 >= 6) board = board.nariKoma(index, "馬")
                }
              }
            }

            else if ((clickedKoma == "飛" || clickedKomaFlag == "飛") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente") { /** 先手の飛車の場合 */
              clickedKomaFlag = "飛"
              sengoKomaFlag = "先手"
              if ( absMoveDistance % 9 == 0 || existSelectedCellIndex / 9 == index / 9) {
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                  if (index /9 < 3) board = board.nariKoma(index, "龍")
                }
              }
            }
            else if ((clickedKoma == "飛" || clickedKomaFlag == "飛") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote") { /** 後手の飛車の場合 */
              clickedKomaFlag = "飛"
              sengoKomaFlag = "後手"
              if ( absMoveDistance % 9 == 0 || existSelectedCellIndex / 9 == index / 9) {
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                  if (index /9 >= 6) board = board.nariKoma(index, "龍")
                }
              }
            }
            /** 成り駒 */
            else if ((clickedKoma == "と" || clickedKomaFlag == "と") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente") { /** 先手のとの場合 */
              clickedKomaFlag = "と"
              sengoKomaFlag = "先手"
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) { //先手駒
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                }
              }
            }
            else if ((clickedKoma == "と" || clickedKomaFlag == "と") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote") { /** 後手のとの場合 */
              clickedKomaFlag = "と"
              sengoKomaFlag = "後手"
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) { //後手駒
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                }
              }
            }
            else if ((clickedKoma == "杏" || clickedKomaFlag == "杏") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente") { /** 先手の成香の場合 */
              clickedKomaFlag = "杏"
              sengoKomaFlag = "先手"
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) { //先手駒
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                }
              }
            }
            else if ((clickedKoma == "杏" || clickedKomaFlag == "杏") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote") { /** 後手の成香の場合 */
              clickedKomaFlag = "杏"
              sengoKomaFlag = "後手"
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) { //後手駒
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                }
              }
            }
            else if ((clickedKoma == "圭" || clickedKomaFlag == "圭") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente") { /** 先手の成桂の場合 */
              clickedKomaFlag = "圭"
              sengoKomaFlag = "先手"
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) { //先手駒
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                }
              }
            }
            else if ((clickedKoma == "圭" || clickedKomaFlag == "圭") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote") { /** 後手の成桂の場合 */
              clickedKomaFlag = "圭"
              sengoKomaFlag = "後手"
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) { //後手駒
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                }
              }
            }
            else if ((clickedKoma == "全" || clickedKomaFlag == "全") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente") { /** 先手の成銀の場合 */
              clickedKomaFlag = "全"
              sengoKomaFlag = "先手"
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) { //先手駒
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                }
              }
            }
            else if ((clickedKoma == "全" || clickedKomaFlag == "全") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote") { /** 後手の成銀の場合 */
              clickedKomaFlag = "全"
              sengoKomaFlag = "後手"
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) { //後手駒
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                }
              }
            }
            else if ((clickedKoma == "馬" || clickedKomaFlag == "馬") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente") { /** 先手の馬の場合 */
              clickedKomaFlag = "馬"
              sengoKomaFlag = "先手"
              if (absMoveDistance % 10 == 0 || absMoveDistance % 8 == 0 || absMoveDistance == 1 || absMoveDistance == 9) { //先手
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                }
              }
            }
            else if ((clickedKoma == "馬" || clickedKomaFlag == "馬") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote") { /** 後手の馬の場合 */
              clickedKomaFlag = "馬"
              sengoKomaFlag = "後手"
              if (absMoveDistance % 10 == 0 || absMoveDistance % 8 == 0 || absMoveDistance == 1 || absMoveDistance == 9) { //後手
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                }
              }
            }
            else if ((clickedKoma == "龍" || clickedKomaFlag == "龍") && (sengoKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente") { /** 先手の龍の場合 */
              clickedKomaFlag = "龍"
              sengoKomaFlag = "先手"
              if ( absMoveDistance % 9 == 0 || existSelectedCellIndex / 9 == index / 9 || absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) {
                if (sengoKoma != true) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Gote"
                }
              }
            }
            else if ((clickedKoma == "龍" || clickedKomaFlag == "龍") && (sengoKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote") { /** 後手の龍の場合 */
              clickedKomaFlag = "龍"
              sengoKomaFlag = "後手"
              if ( absMoveDistance % 9 == 0 || existSelectedCellIndex / 9 == index / 9 || absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) {
                if (sengoKoma != false) {
                  board = board.moveKoma(num, index)
                  selectedCellIndex = None
                  clickedKomaFlag = "NonTarget"
                  sengoKomaFlag = "NonFlag"
                  nextTurn = "Sente"
                }
              }
            }

            /** どの場面にも該当しない場合は、選択を外す */
            else { selectedCellIndex = None }

          }
          case None =>
            selectedCellIndex = Some(index)
        }
        repaint //再描画関数(一番下で定義する)
      })
      group
    } else { /** クリックされない場合 */
      group.setOnMouseClicked(e => {
        selectedCellIndex match {
          case Some(num) => {
            board = board.moveKoma(num, index)
            selectedCellIndex = None
          }
          case None => selectedCellIndex = Some(index)
        }
        repaint //再描画関数(一番下で定義)
      })
      group
    }
  }

  /** senteの駒描画関数 */
  /* 駒の形、置き場所をセットで定義してその値を返す */
  def senteKomaObj(koma: Koma): Group = {
    val senteKomaShape = { //駒の形を定義している
    val poly = Polygon(40, 10, 60, 20, 70, 70, 10, 70, 20, 20)
      poly.setFill(Sienna)
      poly.setStroke(Black)
      poly
    }
    val komaLabel = { //升内の駒の置き場所を定義してる
    val label = new Label
      label.setText(koma.kind)
      label.setFont(Font(30))
      label.setMaxSize(30, 30)
      label.setLayoutX(25)
      label.setLayoutY(25)
      label.setAlignment(Pos.Center)
      label
    }
    //駒の形、置き場所のセット
    val obj = new Group(senteKomaShape, komaLabel)
    obj
  }

  /** goteの駒描画関数 */
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
