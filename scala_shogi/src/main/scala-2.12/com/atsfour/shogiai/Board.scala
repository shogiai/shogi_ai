package com.atsfour.shogiai

case class Board(komas: List[Koma]) {
  val cellIndice = (0 until 81).toList  //場所は0~81の間で、List形式で取得

  /* 場所は0~81をList形式で取得したものから、 `komas.find` し、今の場所を取得する */
  val cells: List[Option[Koma]] = cellIndice.map { n => komas.find(_.index == n) }

  //駒の移動
  def moveKoma(from: Int, to: Int): Board = komas.zipWithIndex.find(_._1.index == from) match {
      case Some((koma, i)) => Board(komas.updated(i, koma.move(to)))
      case None => this
  }

  //成り駒を作る
  def nariKoma(to: Int, nariKoma: String): Board = komas.zipWithIndex.find(_._1.index == to) match {
      case Some((Koma(kind, index, isSente), i)) => Board(komas.updated(i, Koma(kind, index, isSente).nari(nariKoma)))
      case None => this
  }

}
