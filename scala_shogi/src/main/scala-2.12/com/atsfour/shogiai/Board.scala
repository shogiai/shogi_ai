package com.atsfour.shogiai

case class Board(komas: List[Koma]) {
  val cellIndice = (0 until 136).toList //0~136の場所を、List形式で取得

  /* 場所は0~81をList形式で取得したものから、komas.findし、今の場所を取得する */
  val cells: List[Option[Koma]] = cellIndice.map { n => komas.find(_.index == n) }

  def row(index: Int): Int = index / 9
  def column(index: Int): Int = index % 9

  //indexを指定した時、そこにある駒を返す関数
  def findKoma(place: Int) = komas.zipWithIndex.find(_._1.index == place)

  //駒が取られた時の所有権の変更
  def ownerChangeKoma(place: Int, isSente: Boolean): Board = komas.zipWithIndex.find(_._1.index == place) match {
    case Some((koma, i)) => Board(komas.updated(i, koma.ownerChange(isSente)))
    case None => this
  }

  //駒が取られた、打った時の場所の変更
  def spaceChangeKoma(place: Int, onBoard: Boolean): Board = komas.zipWithIndex.find(_._1.index == place) match {
    case Some((koma, i)) => Board(komas.updated(i, koma.spaceChange(onBoard)))
    case None => this
  }

  //駒の移動
  def moveKoma(from: Int, to: Int): Board = komas.zipWithIndex.find(_._1.index == from) match {
    case Some((koma, i)) => Board(komas.updated(i, koma.move(to)))
    case None => this
  }

  //成り駒を作る
  def nariKoma(place: Int, nariKoma: String): Board = komas.zipWithIndex.find(_._1.index == place) match {
    case Some((koma, i)) => Board(komas.updated(i, koma.nari(nariKoma)))
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
  def nifuCheck(now: Int, isSenteInput: Boolean): Boolean = {
    val dan = now % 9
    komas.forall(koma => column(koma.index) != dan || koma.kind != "歩" || !koma.onBoard || koma.isSente != isSenteInput)
  }

  /** 上にどれだけ動けるか */
  def upJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) != nowColumn || row(koma.index) <= toRow || row(koma.index) >= nowRow || !koma.onBoard)
  }

  /** 下にどれだけ動けるか */
  def downJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) != nowColumn || row(koma.index) >= toRow || row(koma.index) <= nowRow || !koma.onBoard)
  }

  /** 左にどれだけ動けるか */
  def leftJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => row(koma.index) != nowRow || column(koma.index) <= toColumn || column(koma.index) >= nowColumn || !koma.onBoard)
  }

  /** 右にどれだけ動けるか, 位置を判定 */
  def rightJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => row(koma.index) != nowRow || column(koma.index) >= toColumn || column(koma.index) <= nowColumn || !koma.onBoard)
  }

  /** 右上方向にどれだけ動けるか */
  def rightUpJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) + row(koma.index) != nowColumn + nowRow ||
      column(koma.index) - row(koma.index) <= toColumn - toRow ||
      column(koma.index) - row(koma.index) >= nowColumn - nowRow || !koma.onBoard)
  }

  /** 左下方向にどれだけ動けるか */
  def leftDownJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) + row(koma.index) != nowColumn + nowRow ||
      column(koma.index) - row(koma.index) >= toColumn - toRow ||
      column(koma.index) - row(koma.index) <= nowColumn - nowRow || !koma.onBoard)
  }

  /** 左上方向にどれだけ動けるか */
  def leftUpJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) - row(koma.index) != nowColumn - nowRow ||
      column(koma.index) + row(koma.index) <= toColumn + toRow ||
      column(koma.index) + row(koma.index) >= nowColumn + nowRow || !koma.onBoard)
  }

  /** 右下方向にどれだけ動けるか */
  def rightDownJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) - row(koma.index) != nowColumn - nowRow ||
      column(koma.index) + row(koma.index) >= toColumn + toRow ||
      column(koma.index) + row(koma.index) <= nowColumn + nowRow || !koma.onBoard)
  }

}
