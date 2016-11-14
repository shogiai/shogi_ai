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
todo 1.相手の駒を取れるようにする
todo 2.持ち駒を打てる
todo 3.駒の成り不成の選択
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
    Koma("歩", 130, true, false), Koma("歩", 131, true, false), Koma("歩", 82, false, false)
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
  def cellObj(komaOpt: Option[Koma], index: Int): Group = {

    //仮にsellIndexが存在する場合はLightBlue色、そうでない場合は、Burlywood色で塗り潰す
    val fillColor = if (selectedCellIndex.contains(index) && clickedKomaFlag != "NonTarget" ) {
      LightBlue
    } else if (index <= 80 //盤面
      || ((index >= 81 && index <= 134) && (index-81) % 6 != 0 && (index-81) / 6 != 4) //持ち駒
    ){ //色をつける場所を指定
      Burlywood
    } else White

    val grid = {
      val rect = Rectangle(80, 80, fillColor)
      if (index <= 80 //盤面
        || ((index >= 81 && index <= 134) && (index-81) % 6 != 0 && (index-81) / 6 != 4) //持ち駒
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

    /** clickedAtと一致した場合の駒情報を取得する */
    val clickedKoma = if (komaOpt != None) { if (komaOpt.get.index == index) { komaOpt.get.kind } } else { "noneKoma" }
    val isSenteKoma = if (komaOpt != None) { if (komaOpt.get.index == index) { komaOpt.get.isSente } } else { "noneKoma" }

    /** boardに、selectedCellIndex(選択したセルのIndex)をboard変数に移動させ、repaint関数で 再描画する */
    if (selectedCellIndex != None) { /** クリックされてる場合 */
      val existSelectedCellIndex = selectedCellIndex.get

      group.setOnMouseClicked(e => {
        selectedCellIndex match { //ここで更新されるのは、eの方。selectedCellIndexではない
          case Some(num) => { /** 各々の駒の動く条件に一致する場合はindexへ移動させる */
            val absMoveDistance = Math.abs(existSelectedCellIndex - index) //駒の移動距離の絶対値を定義
            val moveDistance = existSelectedCellIndex - index //駒の移動距離を定義
            /**todo ここら辺のdefは、Koma,Boardクラスに移動できるかも */
            def sentePlayAndInitializeAndNari(nariGoma: String) = {
              selectedCellIndex = None
              clickedKomaFlag = "NonTarget"
              sengoKomaFlag = "NonFlag"
              board = board.moveKoma(num, index)
              nextTurn = "Gote"
              //todo 成りの結果が、後から書かれている後手の駒に上書きされている(おそらく)
              if (index /9 < 3) board = board.nariKoma(index, nariGoma)
            }
            def gotePlayAndInitializeAndNari(nariGoma: String) = {
              selectedCellIndex = None
              clickedKomaFlag = "NonTarget"
              sengoKomaFlag = "NonFlag"
              board = board.moveKoma(num, index)
              nextTurn = "Sente"
              if (index /9 >= 6) board = board.nariKoma(index, nariGoma)
            }
            def sentePlayAndInitialize = { //成りがない場合
              selectedCellIndex = None
              clickedKomaFlag = "NonTarget"
              sengoKomaFlag = "NonFlag"
              board = board.moveKoma(num, index)
              nextTurn = "Gote"
            }
            def gotePlayAndInitialize = {
              selectedCellIndex = None
              clickedKomaFlag = "NonTarget"
              sengoKomaFlag = "NonFlag"
              board = board.moveKoma(num, index)
              nextTurn = "Sente"
            }
            def senteKomaBranch(koma: String): Boolean = {
              ((clickedKoma == koma && clickedKomaFlag == "NonTarget") || clickedKomaFlag == koma) &&
              (isSenteKoma == true || sengoKomaFlag == "先手") && nextTurn == "Sente"
            }
            def goteKomaBranch(koma: String): Boolean = {
              ((clickedKoma == koma && clickedKomaFlag == "NonTarget") || clickedKomaFlag == koma) &&
              (isSenteKoma == false || sengoKomaFlag == "後手") && nextTurn == "Gote"
            }
            def addFlag(komaKind: String, komaOwner: String) = { //使って書き直すかも
              clickedKomaFlag = komaKind
              sengoKomaFlag = komaOwner
            }
            def clickCancel = {
              selectedCellIndex = None
              clickedKomaFlag = "NonTarget"
            }

            // デバッグ用
            println("clickedKoma:"+clickedKoma,"clickedKomaFlag:"+clickedKomaFlag,"isSenteKoma:"+isSenteKoma,"senteKomaFlag:"+sengoKomaFlag)
            println("selectedCellIndex:"+selectedCellIndex,"nextTurn:"+nextTurn)

            //行き先をクリックした時点では、すでに駒と認識できなくなっている
            if ( senteKomaBranch("歩") ) { /** 先手の歩の場合 */
              addFlag("歩","先手") /** クリックした時に一度入ったというフラグを用意 */
              if (moveDistance == 9) { //駒の移動条件を満たしていて、移動先に駒がない場合に、移動できる
                if (isSenteKoma != true) { sentePlayAndInitializeAndNari("と") }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("歩") ) { /** 後手の歩の場合 */
              addFlag("歩","後手")
              if (moveDistance == -9) {
                if (isSenteKoma != false) { gotePlayAndInitializeAndNari("と") }
              } else if (moveDistance != 0) clickCancel
            }

            else if ( senteKomaBranch("香") ) {
              /** 先手の香車の場合 */
              addFlag("香", "先手")
              if (moveDistance % 9 == 0 && moveDistance > 0 && board.upJumpCheck(num, index)) {
                if (isSenteKoma != true) { sentePlayAndInitializeAndNari("杏") }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("香") ) { /** 後手の香車の場合 */
              addFlag("香","後手")
              if (moveDistance % 9 == 0 && moveDistance < 0 && board.downJumpCheck(num, index)) {
                if (isSenteKoma != false) { gotePlayAndInitializeAndNari("杏") }
              } else if (moveDistance != 0) clickCancel
            }

            else if ( senteKomaBranch("桂") ) { /** 先手の桂馬の場合 */
              addFlag("桂","先手")
              if (moveDistance == 17 || moveDistance == 19) {
                if (isSenteKoma != true) { sentePlayAndInitializeAndNari("圭") }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("桂") ) { /** 後手の桂馬の場合 */
              addFlag("桂","後手")
              if (moveDistance == -17 || moveDistance == -19) {
                if (isSenteKoma != false) { gotePlayAndInitializeAndNari("圭") }
              } else if (moveDistance != 0) clickCancel
            }

            else if ( senteKomaBranch("銀") ) { /** 先手の銀の場合 */
              addFlag("銀","先手")
              if (absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == 9) {
                if (isSenteKoma != true) { sentePlayAndInitializeAndNari("全") }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("銀") ) { /** 後手の銀の場合 */
              addFlag("銀","後手")
              if (absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == -9) {
                if (isSenteKoma != false) { gotePlayAndInitializeAndNari("全") }
              } else if (moveDistance != 0) clickCancel
            }

            else if ( senteKomaBranch("金") ) { /** 先手の金の場合 */
              addFlag("金","先手")
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) {
                if (isSenteKoma != true) { sentePlayAndInitialize } //以下、飛車角以外は成りがない関数を使う
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("金") ) { /** 後手の金の場合 */
              addFlag("金","後手")
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) { //後手駒
                if (isSenteKoma != false) { gotePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }

            else if ( senteKomaBranch("王") ) { /** 先手の王の場合 */
              addFlag("王","先手")
              if (absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) {
                if (isSenteKoma != true) { sentePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("王") ) { /** 後手の金の場合 */
              addFlag("王","後手")
              if (absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) {
                if (isSenteKoma != false) { gotePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }

            else if ( senteKomaBranch("角") ) { /** 先手の角の場合 */
              addFlag("角","先手")
              if ((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, index) && board.rightDownJumpCheck(num, index)) //左上から右下方向
                || absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, index) && board.leftDownJumpCheck(num, index)) { //右上から左下方向
                if (isSenteKoma != true) {
                  sentePlayAndInitializeAndNari("馬")
                }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("角") ) { /** 後手の角の場合 */
              addFlag("角","後手")
              if ((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, index) && board.rightDownJumpCheck(num, index))
                || absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, index) && board.leftDownJumpCheck(num, index)) {
                if (isSenteKoma != false) {
                  gotePlayAndInitializeAndNari("馬")
                }
              } else if (moveDistance != 0) clickCancel
            }

            else if ( senteKomaBranch("飛") ) { /** 先手の飛車の場合 */
              addFlag("飛","先手")
              if (( absMoveDistance % 9 == 0 && board.upJumpCheck(num, index) && board.downJumpCheck(num, index)) //縦(上下)方向
                || (existSelectedCellIndex / 9 == index / 9 && board.rightJumpCheck(num, index) && board.leftJumpCheck(num, index)) //横方向
              ) {
                if (isSenteKoma != true) { sentePlayAndInitializeAndNari("龍") }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("飛") ) { /** 後手の飛車の場合 */
              addFlag("飛","後手")
              if (( absMoveDistance % 9 == 0 && board.upJumpCheck(num, index) && board.downJumpCheck(num, index))
                || (existSelectedCellIndex / 9 == index / 9 && board.rightJumpCheck(num, index) && board.leftJumpCheck(num, index))) {
                if (isSenteKoma != false) { gotePlayAndInitializeAndNari("龍") }
              } else if (moveDistance != 0) clickCancel
            }
            /** 成り駒 */
            else if ( senteKomaBranch("と") ) { /** 先手のとの場合 */
              addFlag("と","先手")
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) {
                if (isSenteKoma != true) { sentePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("と") ) { /** 後手のとの場合 */
              addFlag("と","後手")
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) {
                if (isSenteKoma != false) { gotePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( senteKomaBranch("杏") ) { /** 先手の成香の場合 */
              addFlag("杏","先手")
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) {
                if (isSenteKoma != true) { sentePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("杏") ) { /** 後手の成香の場合 */
              addFlag("杏","後手")
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) {
                if (isSenteKoma != false) { gotePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( senteKomaBranch("圭") ) { /** 先手の成桂の場合 */
              addFlag("圭","先手")
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) {
                if (isSenteKoma != true) { sentePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("圭") ) { /** 後手の成桂の場合 */
              addFlag("圭","後手")
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) { //後手駒
                if (isSenteKoma != false) { gotePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( senteKomaBranch("全") ) { /** 先手の成銀の場合 */
              addFlag("全","先手")
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10) { //先手駒
                if (isSenteKoma != true) { sentePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("全") ) { /** 後手の成銀の場合 */
              addFlag("全","後手")
              if (absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10) { //後手駒
                if (isSenteKoma != false) { gotePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( senteKomaBranch("馬") ) { /** 先手の馬の場合 */
              addFlag("馬","先手")
              if ((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, index) && board.rightDownJumpCheck(num, index)) //左上~右下
                || (absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, index) && board.leftDownJumpCheck(num, index)) //左下~右上
                || absMoveDistance == 1 || absMoveDistance == 9) {
                if (isSenteKoma != true) { sentePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("馬") ) { /** 後手の馬の場合 */
              addFlag("馬","後手")
              if ((absMoveDistance % 10 == 0 && board.leftUpJumpCheck(num, index) && board.rightDownJumpCheck(num, index)) //左上~右下
                || (absMoveDistance % 8 == 0 && board.rightUpJumpCheck(num, index) && board.leftDownJumpCheck(num, index)) //左下~右上
                || absMoveDistance == 1 || absMoveDistance == 9) {
                if (isSenteKoma != false) { gotePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( senteKomaBranch("龍") ) { /** 先手の龍の場合 */
              addFlag("龍","先手")
              if ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, index) && board.downJumpCheck(num, index)) //縦(上下)方向
                || (existSelectedCellIndex / 9 == index / 9 && board.rightJumpCheck(num, index) && board.leftJumpCheck(num, index)) //横方向
                || absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) {
                if (isSenteKoma != true) { sentePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }
            else if ( goteKomaBranch("龍") ) { /** 後手の龍の場合 */
              addFlag("龍","後手")
              if ((absMoveDistance % 9 == 0 && board.upJumpCheck(num, index) && board.downJumpCheck(num, index)) //縦(上下)方向
                || (existSelectedCellIndex / 9 == index / 9 && board.rightJumpCheck(num, index) && board.leftJumpCheck(num, index)) //横方向
                || absMoveDistance == 1 || absMoveDistance == 8 || absMoveDistance == 9 || absMoveDistance == 10) {
                if (isSenteKoma != false) { gotePlayAndInitialize }
              } else if (moveDistance != 0) clickCancel
            }

          }
          case None => selectedCellIndex = Some(index)
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
        repaint //再描画関数(一番下で定義する)
      })
      group
    }
  }

  /** senteの駒描画関数 */
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
