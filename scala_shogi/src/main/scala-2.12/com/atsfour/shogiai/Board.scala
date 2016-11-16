package com.atsfour.shogiai

import scala.collection.mutable.ListBuffer

//komasをArrayにしにて、盤をSetするような方針
case class Board(komas: ListBuffer[Koma]) {
  val cellIndice = (0 until 136).toArray //0~136の場所を、List形式で取得

  /* 場所は0~81をList形式で取得したものから、komas.findし、今の場所を取得する */
  val cells: Array[Option[Koma]] = cellIndice.map { n => komas.find(_.index == n) }

  //indexを指定した時、そこにある駒を返す関数
  def findKoma(place: Int) = komas.zipWithIndex.find(_._1.index == place)

  //駒が取られた時の所有権の変更
  def ownerChangeKoma(place: Int, isSente: Boolean) = komas.zipWithIndex.find(_._1.index == place) match {
    case Some((koma, i)) => komas(i) = koma.ownerChange(isSente)
    case None => this
  }

  //駒が取られた、打った時の所有権の変更
  def spaceChangeKoma(place: Int, onBoard: Boolean) = komas.zipWithIndex.find(_._1.index == place) match {
    case Some((koma, i)) => komas(i) = koma.spaceChange(onBoard) //Board(komas.updated(i, koma.spaceChange(onBoard)))
    case None => this
  }

  //駒の移動
  def moveKoma(from: Int, to: Int) = komas.zipWithIndex.find(_._1.index == from) match {
    case Some((koma, i)) => komas(i) = koma.move(to)//Board(komas.updated(i, koma.move(to)))
    case None => this
  }

  //成り駒を作る
  def nariKoma(place: Int, nariKoma: String) = komas.zipWithIndex.find(_._1.index == place) match {
    case Some((koma, i)) => komas(i) = koma.nari(nariKoma) //(komas.updated(i, koma.nari(nariKoma)))
    case None => this
  }

  //Koma(kind, index, isSente, onBoard)
  def returnNariKoma(place: Int): Board = komas.zipWithIndex.find(_._1.index == place) match {
    case Some((Koma("と", index, isSente, onBoard), i)) => Board(komas.updated(i, Koma("と", index, isSente, onBoard).nari("歩")))
    case Some((Koma("杏", index, isSente, onBoard), i)) => Board(komas.updated(i, Koma("杏", index, isSente, onBoard).nari("香")))
    case Some((Koma("圭", index, isSente, onBoard), i)) => Board(komas.updated(i, Koma("圭", index, isSente, onBoard).nari("桂")))
    case Some((Koma("全", index, isSente, onBoard), i)) => Board(komas.updated(i, Koma("全", index, isSente, onBoard).nari("銀")))
    case Some((Koma("馬", index, isSente, onBoard), i)) => Board(komas.updated(i, Koma("馬", index, isSente, onBoard).nari("角")))
    case Some((Koma("龍", index, isSente, onBoard), i)) => Board(komas.updated(i, Koma("龍", index, isSente, onBoard).nari("飛")))
    case Some((Koma("王", index, true, onBoard), i)) => {
      println("後手の勝ち")
      Board(komas.updated(i, Koma("王", index, true, onBoard).nari("王")))
    }
    case Some((Koma("王", index, false, onBoard), i)) => {
      println("先手の勝ち")
      Board(komas.updated(i, Koma("王", index, false, onBoard).nari("王")))
    }
    case Some((Koma(kind, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(kind, index, isSente, onBoard))) //成り駒ではないときは何もしなくてOK
    case None => this
  }

  /** 歩を打つときの二歩チェック */
  case class NifuCheck(kind: String, isSente: Boolean, onBoard: Boolean)
  def nifuCheck(now: Int, isSente:Boolean): Boolean = {
    var check = true
    val suji = now % 9
    for (dan <- 0 until 8) { //(Koma(kind, index, isSente, onBoard)
      val line = 9 * dan + suji
      val koma: Option[NifuCheck] = komas.zipWithIndex.find(_._1.index == line) match {
        case Some((Koma(kind, index, isSente, onBoard), i)) => Some(NifuCheck(kind, isSente, onBoard))
        case None => None
      }
      if (koma == Some(NifuCheck("歩",isSente,true))) check = false
    }
    check
  }

  /** 上にどれだけ動けるか */
  def upJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowDan, suji) = (now / 9, now % 9)
    var (upMoveDan, kaisu) = (0, 0)

    if (nowDan != 0) { //0段目のとき、上側に移動できない
      for (posibbleDan <- 0 until nowDan - 1) { //減少していく表記はできない
        val line = 9 * ((nowDan - 1) - posibbleDan) + suji
        val koma: Option[Koma] = komas.zipWithIndex.find(_._1.index == line) match {
          case Some((koma, i)) => Option(koma)
          case None => None
        }
        if (koma != None && kaisu == 0) {
          println("upJumpCheck","koma:"+koma,"upMoveDan:"+((nowDan - 1) - posibbleDan))
          upMoveDan = (nowDan - 1) - posibbleDan //最初に駒があった場所を記録
          kaisu = kaisu + 1
        }
      }
      if (kaisu == 0) upMoveDan = 0  //途中何もなければ一番上まで
    } else upMoveDan = 0 //最初から0段目のとき

    val canUpPlace = suji + 9 * upMoveDan //移動可能なindexの特定
    //移動先と比較, canUpPlaceより上の(小さい)場所には移動できない
    if (canUpPlace <= toIndex) true else false
  }

  /** 下にどれだけ動けるか */
  def downJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowDan, suji) = (now / 9, now % 9)
    var (downMoveDan, kaisu) = (8, 0)

    if (nowDan != 8) { //0段目のとき、上側に移動できない
      for (posibbleDan <- nowDan + 1 until 8) {
        val line = 9 * posibbleDan + suji
        val koma: Option[Koma] = komas.zipWithIndex.find(_._1.index == line) match {
          case Some((koma, i)) => Option(koma)
          case None => None
        }
        if (koma != None && kaisu == 0) {
          println("downJumpCheck","koma:"+koma,"downMoveDan:"+posibbleDan)
          downMoveDan = posibbleDan //最初に駒があった場所を記録
          kaisu = kaisu + 1
        }
      }
      if (kaisu == 0) downMoveDan = 8  //途中何もなければ一番下まで
    } else downMoveDan = 8 //最初から8段目のとき

    val canDownPlace = suji + 9 * downMoveDan //移動可能なindexの特定
    //移動先と比較, canUpPlaceより下の(大きい)場所には移動できない
    if (canDownPlace >= toIndex) true else false
  }

  /** 左にどれだけ動けるか */
  def leftJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (dan, nowSuji) = (now / 9, now % 9)
    var (leftMoveSuji, kaisu) = (0, 0)

    if (nowSuji != 0) { //0筋目のとき、左側へは移動できない
      for (posibbleSuji <- 0 until nowSuji - 1) { //減少していく表記はできない
        val line = 9 * dan + ((nowSuji - 1) - posibbleSuji)
        val koma: Option[Koma] = komas.zipWithIndex.find(_._1.index == line) match {
          case Some((koma, i)) => Option(koma)
          case None => None
        }
        if (koma != None && kaisu == 0) {
          println("leftJumpCheck","koma:"+koma,"leftMoveSuji:"+((nowSuji - 1)-posibbleSuji))
          leftMoveSuji = (nowSuji - 1) - posibbleSuji  //最初に駒があった場所を記録
          kaisu = kaisu + 1
        }
      }
      if (kaisu == 0) leftMoveSuji = 0  //途中何もなければ一番左まで
    } else leftMoveSuji = 0 //最初から0筋目のとき

    val canLeftPlace = 9 * dan + leftMoveSuji //移動可能なindexの特定
    //移動先と比較, canLeftPlaceより右の(大きい)場合しか移動できない
    if (canLeftPlace <= toIndex) true else false
  }

  /** 右にどれだけ動けるか, 位置を判定 */
  def rightJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (dan, nowSuji) = (now / 9, now % 9)
    var (rightMoveSuji, kaisu) = (8, 0)

    if (nowSuji != 8) { //8筋目のとき、右側へは移動できない
      for (posibbleSuji <- nowSuji + 1 until 8) {
        val line = 9 * dan + posibbleSuji
        val koma: Option[Koma] = komas.zipWithIndex.find(_._1.index == line) match {
          case Some((koma, i)) => Option(koma)
          case None => None
        }
        if (koma != None && kaisu == 0) { //駒が存在する場所をストックする, 一度止まったら更新しない
          println("rightJumpCheck","koma:"+koma,"rightMoveSuji" + ":"+posibbleSuji)
          rightMoveSuji = posibbleSuji //最初に駒があった場所を記録
          kaisu = kaisu + 1
        }
      }
      if (kaisu == 0) rightMoveSuji = 8  //途中駒がなければ一番右まで
    } else rightMoveSuji = 8 //最初から8筋目のとき

    val canRightPlace = 9 * dan + rightMoveSuji //移動可能なindexの特定
    if (canRightPlace >= toIndex) true else false //移動先と比較
  }

  /** 右上方向にどれだけ動けるか。 以下4方向では、上下左右と取得方法が違うのに注意 */
  def rightUpJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowDan, nowSuji) = (now / 9, now % 9)
    var (rightUpMoveDistance, kaisu) = (0, 0)

    if ( nowSuji != 8 || nowDan != 0 ) { //8筋目のときは右、0段目のときは上側へ移動できない

      for (posibbleRightUp <- 1 until Math.min(nowDan, 8 - nowSuji)) {
        val line = now - 8 * posibbleRightUp
        val koma: Option[Koma] = komas.zipWithIndex.find(_._1.index == line) match {
          case Some((koma, i)) => Option(koma)
          case None => None
        }
        if (koma != None && kaisu == 0) {
          println("rightUpJumpCheck","koma:"+koma,"rightUpMoveSuji:"+rightUpMoveDistance)
          rightUpMoveDistance = posibbleRightUp //最初に駒があった時の移動距離を記録
          kaisu = kaisu + 1
        }
      }
      if (kaisu == 0) rightUpMoveDistance = Math.min(nowDan, 8 - nowSuji) //途中何もなければ動ける一番端まで
    } else rightUpMoveDistance = 0 //端にいる時は動かない

    val canRightUpPlace = now - 8 * rightUpMoveDistance //移動可能なindexの特定
    if (canRightUpPlace <= toIndex) true else false //移動先と比較, canRightUpPlaceは下限
  }

  /** 左下方向にどれだけ動けるか */
  def leftDownJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowDan, nowSuji) = (now / 9, now % 9)
    var (leftDownMoveDistance, kaisu) = (0, 0)

    if ( nowSuji != 0 || nowDan != 8 ) { //0筋目のときは左側、8段目のときは下側へ移動できない

      for ( posibbleLeftDown <- 1 until Math.min(8 - nowDan, nowSuji)) {
        val line = now + 8 * posibbleLeftDown
        val koma: Option[Koma] = komas.zipWithIndex.find(_._1.index == line) match {
          case Some((koma, i)) => Option(koma)
          case None => None
        }
        if (koma != None && kaisu == 0) { //駒が存在する場所をストックする, 一度止まったら更新しない
          println("leftDownJumpCheck","koma:"+koma,"leftDownMoveDistance:"+leftDownMoveDistance)
          leftDownMoveDistance = posibbleLeftDown //最初に駒があった時の移動距離を記録
          kaisu = kaisu + 1
        }
      }
      if (kaisu == 0) leftDownMoveDistance = Math.min(8 - nowDan, nowSuji) //途中何もなければ動ける一番端まで
    } else leftDownMoveDistance = 0 //端にいる時は動かない

    val canLeftDownPlace = now + 8 * leftDownMoveDistance //移動可能なindexの特定
    if (canLeftDownPlace >= toIndex) true else false //移動先と比較, canLeftDownPlaceは上限
  }

  /** 左上方向にどれだけ動けるか */
  def leftUpJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowDan, nowSuji) = (now / 9, now % 9)
    var (leftUpMoveDistance, kaisu) = (0, 0)

    if ( nowSuji != 0 || nowDan != 0 ) { //0筋目のときは左側、0段目のときは上側へ移動できない

      for ( posibbleLeftUp <- 1 until Math.min(nowDan, nowDan) ) {
        val line = now - 10 * posibbleLeftUp
        val koma: Option[Koma] = komas.zipWithIndex.find(_._1.index == line) match {
          case Some((koma, i)) => Option(koma)
          case None => None
        }
        if (koma != None && kaisu == 0) { //駒が存在する場所をストックする, 一度止まったら更新しない
          println("leftUpJumpCheck","koma:"+koma,"leftUpMoveDistance:"+leftUpMoveDistance)
          leftUpMoveDistance = posibbleLeftUp //最初に駒があった時の移動距離を記録
          kaisu = kaisu + 1
        }
      }
      if (kaisu == 0) leftUpMoveDistance = Math.min(nowDan, nowSuji) //途中何もなければ動ける一番端まで
    } else leftUpMoveDistance = 0

    val canLeftUpPlace = now - 10 * leftUpMoveDistance //移動可能なindexの特定
    if (canLeftUpPlace <= toIndex) true else false //移動先と比較
  }

  /** 右下方向にどれだけ動けるか */
  def rightDownJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowDan, nowSuji) = (now / 9, now % 9)
    var (rightDownMoveDistance, kaisu) = (0, 0)

    if ( nowSuji != 8 || nowDan != 8 ) { //8筋目or8段目のときは、右上側へ移動できない

      for (posibbleLeftUp <- 1 until Math.min(8 - nowDan, 8 - nowDan)) {
        val line = now + 10 * posibbleLeftUp
        val koma: Option[Koma] = komas.zipWithIndex.find(_._1.index == line) match {
          case Some((koma, i)) => Option(koma)
          case None => None
        }
        if (koma != None && kaisu == 0) { //駒が存在する場所をストックする, 一度止まったら更新しない
          println("leftUpJumpCheck","koma:"+koma,"leftUpMoveDistance:"+rightDownMoveDistance)
          rightDownMoveDistance = posibbleLeftUp //最初に駒があった時の移動距離を記録
          kaisu = kaisu + 1
        }
      }
      if (kaisu == 0) rightDownMoveDistance = Math.min(8 - nowDan, 8 - nowDan) //途中何もなければ動ける一番端まで
    } else rightDownMoveDistance = 0

    val canRightDownPlace = now + 10 * rightDownMoveDistance //移動可能なindexの特定
    if (canRightDownPlace >= toIndex) true else false //移動先と比較
  }

}
