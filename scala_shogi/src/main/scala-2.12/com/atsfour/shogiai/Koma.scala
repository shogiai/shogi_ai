package com.atsfour.shogiai

case class Koma(kind: String, index: Int, isSente: Boolean) {
  val xPos = index % 9
  val yPos = index / 9

  //自クラスのフィールド・メソッド
  def move(to: Int): Koma = this.copy(index = to)  //駒が動く関数
  def nari(nariKoma: String): Koma = this.copy(kind = nariKoma) //駒が成る関数
}
