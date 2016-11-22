package com.atsfour.shogiai

import ShogiBoard.ClickedKomaState

case class Board(komas: List[Koma]) {

  //todo 棋譜を読み込めるように、将棋盤の表示形式を工夫したい
  //todo お互いの駒の点数チェックを行える関数作成
  /* 場所をList形式で取得したものから、komas.findし、今の場所を取得する */
  val cellIndice = (0 until 136).toList
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
  def nariKoma(place: Int): Board = komas.zipWithIndex.find(_._1.index == place) match {
    case Some((Koma(ClickedKomaState.Fu, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(ClickedKomaState.To, index, isSente, onBoard).nari(ClickedKomaState.To)))
    case Some((Koma(ClickedKomaState.Kyo, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(ClickedKomaState.NariKyo, index, isSente, onBoard).nari(ClickedKomaState.NariKyo)))
    case Some((Koma(ClickedKomaState.Kei, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(ClickedKomaState.NariKei, index, isSente, onBoard).nari(ClickedKomaState.NariKei)))
    case Some((Koma(ClickedKomaState.Gin, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(ClickedKomaState.NariGin, index, isSente, onBoard).nari(ClickedKomaState.NariGin)))
    case Some((Koma(ClickedKomaState.Kaku, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(ClickedKomaState.Uma, index, isSente, onBoard).nari(ClickedKomaState.Uma)))
    case Some((Koma(ClickedKomaState.Hisha, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(ClickedKomaState.Ryu, index, isSente, onBoard).nari(ClickedKomaState.Ryu)))

    case Some((Koma(kind, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(kind, index, isSente, onBoard))) //成り駒ではないときは何もしなくてOK
    case None => this
  }

  //成っていた駒が取られて持ち駒になるときは、再度成る前の状態に戻す
  def returnNariKoma(place: Int): Board = komas.zipWithIndex.find(_._1.index == place) match {
    case Some((Koma(ClickedKomaState.To, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(ClickedKomaState.To, index, isSente, onBoard).nari(ClickedKomaState.Fu)))
    case Some((Koma(ClickedKomaState.NariKyo, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(ClickedKomaState.NariKyo, index, isSente, onBoard).nari(ClickedKomaState.Kyo)))
    case Some((Koma(ClickedKomaState.NariKei, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(ClickedKomaState.NariKei, index, isSente, onBoard).nari(ClickedKomaState.Kei)))
    case Some((Koma(ClickedKomaState.NariGin, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(ClickedKomaState.NariGin, index, isSente, onBoard).nari(ClickedKomaState.Gin)))
    case Some((Koma(ClickedKomaState.Uma, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(ClickedKomaState.Uma, index, isSente, onBoard).nari(ClickedKomaState.Kaku)))
    case Some((Koma(ClickedKomaState.Ryu, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(ClickedKomaState.Ryu, index, isSente, onBoard).nari(ClickedKomaState.Hisha)))
    case Some((Koma(kind, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(kind, index, isSente, onBoard))) //成り駒ではないときは何もしなくてOK
    case None => this
  }

  /** 歩を打つときの二歩チェック */
  case class NifuCheck(kind: String, isSente: Boolean, onBoard: Boolean)
  def nifuCheck(now: Int, isSenteInput: Boolean): Boolean = {
    komas.forall(koma => column(koma.index) != column(now) || koma.kind != ClickedKomaState.Fu || !koma.onBoard || (koma.isSente != isSenteInput))
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
      nowColumn >= column(koma.index) ||
      toColumn <= column(koma.index) || !koma.onBoard)
  }

  /** 左下方向にどれだけ動けるか */
  def leftDownJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) + row(koma.index) != nowColumn + nowRow ||
      toColumn >= column(koma.index) ||
      nowColumn <= column(koma.index) || !koma.onBoard)
  }

  /** 左上方向にどれだけ動けるか */
  def leftUpJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) - row(koma.index) != nowColumn - nowRow ||
      toColumn >= column(koma.index) ||
      nowColumn <= column(koma.index) || !koma.onBoard)
  }

  /** 右下方向にどれだけ動けるか */
  def rightDownJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) - row(koma.index) != nowColumn - nowRow ||
      nowColumn >= column(koma.index) ||
      toColumn <= column(koma.index) || !koma.onBoard)
  }

}
