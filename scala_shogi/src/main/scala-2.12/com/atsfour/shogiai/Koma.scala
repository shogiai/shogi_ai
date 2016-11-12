package com.atsfour.shogiai

case class Koma(kind: String, index: Int, isSente: Boolean) {
  val xPos = index % 9
  val yPos = index / 9

  //自クラスのフィールド・メソッド
  def getKoma(now: Int): Koma = this  //場所を指定した時、その駒型を見つける関数
  def find(now: Int): String = this.kind  //場所を指定した時、駒を見つける関数

  def move(to: Int): Koma = this.copy(index = to)  //駒が動く関数
  def nari(nariKoma: String): Koma = this.copy(kind = nariKoma) //駒が成る関数
}
