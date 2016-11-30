package shogi

import shogi.ShogiBoard.ClickedKomaState

case class Koma(kind: ClickedKomaState, index: Int, isSente: Boolean, onBoard: Boolean) {
  val xPos = index % 9
  val yPos = index / 9

  //自クラスのフィールド・メソッド
  def ownerChange(isSente: Boolean): Koma = this.copy(isSente = !isSente) //駒の所有権を変える関数
  def spaceChange(onBoard: Boolean): Koma = this.copy(onBoard = !onBoard) //駒が盤上か手持ちかの登録を変える関数

  def move(to: Int): Koma = this.copy(index = to)  //駒が動く関数
  def nari(nariKoma: ClickedKomaState): Koma = this.copy(kind = nariKoma) //駒が成る関数
}
