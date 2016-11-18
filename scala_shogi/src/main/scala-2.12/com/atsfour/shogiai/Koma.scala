package com.atsfour.shogiai

import ShogiBoard.ClickedKomaState

case class Koma(kind: ClickedKomaState, index: Int, isSente: Boolean, onBoard: Boolean) {
  val xPos = index % 9
  val yPos = index / 9

  //自クラスのフィールド・メソッド
  def reverse(boolean: Boolean):Boolean = if (boolean) {false} else {true}

  def optionReverse(optBoolean: Option[Boolean]): Option[Boolean] = {
    if (optBoolean.contains(true)) Option(false)
    else if (optBoolean.contains(false)) Option(true)
    else  None
  }

  def ownerChange(isSente: Boolean): Koma = this.copy(isSente = reverse(isSente)) //駒の所有権を変える関数
  def spaceChange(onBoard: Boolean): Koma = this.copy(onBoard = reverse(onBoard)) //駒が盤上か手持ちかの登録を変える関数

  def move(to: Int): Koma = this.copy(index = to)  //駒が動く関数
  def nari(nariKoma: ClickedKomaState): Koma = this.copy(kind = nariKoma) //駒が成る関数
}
