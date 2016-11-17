package com.atsfour.shogiai

import ShogiBoard.{clickedKomaStates, ClickedKomaState}

case class Board(komas: List[Koma]) {

  //todo 棋譜を読み込めるように、将棋盤の表示形式を工夫したい
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
  def nariKoma(place: Int, nariKoma: ClickedKomaState): Board = komas.zipWithIndex.find(_._1.index == place) match {
    case Some((koma, i)) => Board(komas.updated(i, koma.nari(nariKoma)))
    case None => this
  }

  //成っていた駒が取られて持ち駒になるときは、再度成る前の状態に戻す
  def returnNariKoma(place: Int): Board = komas.zipWithIndex.find(_._1.index == place) match {
    case Some((Koma(clickedKomaStates.To, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(clickedKomaStates.To, index, isSente, onBoard).nari(clickedKomaStates.Fu)))
    case Some((Koma(clickedKomaStates.NariKyo, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(clickedKomaStates.NariKyo, index, isSente, onBoard).nari(clickedKomaStates.Kyo)))
    case Some((Koma(clickedKomaStates.NariKei, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(clickedKomaStates.NariKei, index, isSente, onBoard).nari(clickedKomaStates.Kei)))
    case Some((Koma(clickedKomaStates.NariGin, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(clickedKomaStates.NariGin, index, isSente, onBoard).nari(clickedKomaStates.Gin)))
    case Some((Koma(clickedKomaStates.Uma, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(clickedKomaStates.Uma, index, isSente, onBoard).nari(clickedKomaStates.Kaku)))
    case Some((Koma(clickedKomaStates.Ryu, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(clickedKomaStates.Ryu, index, isSente, onBoard).nari(clickedKomaStates.Hisha)))
    case Some((Koma(clickedKomaStates.Ou, index, true, onBoard), i)) => {
      println("後手の勝ち") //todo scalaFXで描画
      Board(komas.updated(i, Koma(clickedKomaStates.Ou, index, true, onBoard).nari(clickedKomaStates.Ou)))
    }
    case Some((Koma(clickedKomaStates.Ou, index, false, onBoard), i)) => {
      println("先手の勝ち")
      Board(komas.updated(i, Koma(clickedKomaStates.Ou, index, false, onBoard).nari(clickedKomaStates.Ou)))
    }
    case Some((Koma(kind, index, isSente, onBoard), i)) => Board(komas.updated(i, Koma(kind, index, isSente, onBoard))) //成り駒ではないときは何もしなくてOK
    case None => this
  }

  /** 歩を打つときの二歩チェック */
  case class NifuCheck(kind: String, isSente: Boolean, onBoard: Boolean)
  def nifuCheck(now: Int, isSenteInput: Boolean): Boolean = {
    komas.forall(koma => column(koma.index) != column(now) || koma.kind != clickedKomaStates.Fu || !koma.onBoard || (koma.isSente != isSenteInput))
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
