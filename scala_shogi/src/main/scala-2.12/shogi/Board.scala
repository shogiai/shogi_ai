package shogi

import ShogiBoard.ClickedKomaState

case class Board(komas: List[Koma]) {

  //todo 棋譜を読み込めるように、将棋盤の表示形式を工夫したい
  val cellIndice = (0 until 136).toList
  val cells: List[Option[Koma]] = cellIndice.map { n => komas.find(_.index == n) }

  def row(index: Int): Int = index / 9
  def column(index: Int): Int = index % 9

  //indexを指定した時、そこにある駒を返す関数
  def findKoma(place: Int) = komas.zipWithIndex.find(_._1.index == place)

  def filterKomaKind(komaKind: ClickedKomaState, isSenteKoma: Boolean): List[Int] = {
    komas.filter(_.isSente == isSenteKoma).filter(_.kind == komaKind).map { koma =>
      koma match {
      case Koma(kind, index, isSente, onBoard) => index //index
      case _ => -1
    }}
  }

  def findOuGyoku(isSenteKoma: Boolean): Int = {
    komas.filter(_.isSente == isSenteKoma).find(komas => komas.kind == ClickedKomaState.Ou || komas.kind == ClickedKomaState.Gyoku) match {
      case Some(Koma(kind, index, isSente, onBoard)) => index
      case None => -1
    }
  }

  def findPlaceKomaKind(place: Int): ClickedKomaState = {
    komas.find(_.index == place) match {
      case Some(Koma(kind, index, isSente, onBoard)) => kind
      case None => ClickedKomaState.Blank //起こりえない
    }
  }

  def findPlaceKomaisSente(place: Int): Option[Boolean] = {
    komas.find(_.index == place) match {
      case Some(Koma(kind, index, isSente, onBoard)) => Some(isSente)
      case _ => None
    }
  }

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
      (nowColumn >= column(koma.index) && row(koma.index) >= nowRow) || (toColumn <= column(koma.index) && row(koma.index) <= toRow) ||
      !koma.onBoard)
  }

  /** 左下方向にどれだけ動けるか */
  def leftDownJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) + row(koma.index) != nowColumn + nowRow ||
      (toColumn >= column(koma.index) && row(koma.index) >= toRow) || (nowColumn <= column(koma.index) && row(koma.index) <= nowRow) ||
      !koma.onBoard)
  }

  /** 左上方向にどれだけ動けるか */
  def leftUpJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) - row(koma.index) != nowColumn - nowRow ||
      (toColumn >= column(koma.index) && toRow >= row(koma.index)) || (nowColumn <= column(koma.index) && nowRow <= row(koma.index)) ||
      !koma.onBoard)
  }

  /** 右下方向にどれだけ動けるか */
  def rightDownJumpCheck(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) - row(koma.index) != nowColumn - nowRow ||
      (nowColumn >= column(koma.index) && nowRow >= row(koma.index)) || (toColumn <= column(koma.index) && toRow <= row(koma.index)) ||
      !koma.onBoard)
  }

  /** 詰みが有る時、無い時を調べる時用, 王をすり抜け条件から抜いている */
  //上にどれだけ動けるか
  def checkMateUpJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) != nowColumn || row(koma.index) <= toRow || row(koma.index) >= nowRow || !koma.onBoard ||
      ((koma.kind == ClickedKomaState.Ou || koma.kind == ClickedKomaState.Gyoku) && koma.isSente == isSenteTurnState))
  }

  //下にどれだけ動けるか
  def checkMateDownJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) != nowColumn || row(koma.index) >= toRow || row(koma.index) <= nowRow || !koma.onBoard ||
      ((koma.kind == ClickedKomaState.Ou || koma.kind == ClickedKomaState.Gyoku) && koma.isSente == isSenteTurnState))
  }

  //左にどれだけ動けるか
  def checkMateLeftJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => row(koma.index) != nowRow || column(koma.index) <= toColumn || column(koma.index) >= nowColumn || !koma.onBoard ||
      ((koma.kind == ClickedKomaState.Ou || koma.kind == ClickedKomaState.Gyoku) && koma.isSente == isSenteTurnState))
  }

  //右にどれだけ動けるか
  def checkMateRightJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => row(koma.index) != nowRow || column(koma.index) >= toColumn || column(koma.index) <= nowColumn || !koma.onBoard ||
      ((koma.kind == ClickedKomaState.Ou || koma.kind == ClickedKomaState.Gyoku) && koma.isSente == isSenteTurnState))
  }

  //右上方向にどれだけ動けるか
  def checkMateRightUpJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) + row(koma.index) != nowColumn + nowRow ||
      (nowColumn >= column(koma.index) && row(koma.index) >= nowRow) || (toColumn <= column(koma.index) && row(koma.index) <= toRow) ||
      !koma.onBoard || ((koma.kind == ClickedKomaState.Ou || koma.kind == ClickedKomaState.Gyoku) && koma.isSente == isSenteTurnState))
  }

  //左下方向にどれだけ動けるか
  def checkMateLeftDownJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) + row(koma.index) != nowColumn + nowRow ||
      (toColumn >= column(koma.index) && row(koma.index) >= toRow) || (nowColumn <= column(koma.index) && row(koma.index) <= nowRow) ||
      !koma.onBoard || ((koma.kind == ClickedKomaState.Ou || koma.kind == ClickedKomaState.Gyoku) && koma.isSente == isSenteTurnState))
  }

  //左上方向にどれだけ動けるか
  def checkMateLeftUpJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) - row(koma.index) != nowColumn - nowRow ||
      (toColumn >= column(koma.index) && toRow >= row(koma.index)) || (nowColumn <= column(koma.index) && nowRow <= row(koma.index)) ||
      !koma.onBoard || ((koma.kind == ClickedKomaState.Ou || koma.kind == ClickedKomaState.Gyoku) && koma.isSente == isSenteTurnState))
  }

  //右下方向にどれだけ動けるか
  def checkMateRightDownJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) - row(koma.index) != nowColumn - nowRow ||
      (nowColumn >= column(koma.index) && nowRow >= row(koma.index)) || (toColumn <= column(koma.index) && toRow <= row(koma.index)) ||
      !koma.onBoard || ((koma.kind == ClickedKomaState.Ou || koma.kind == ClickedKomaState.Gyoku) && koma.isSente == isSenteTurnState))
  }

  def notOwn(now: Int, toIndex: Int): Boolean = now != toIndex
  def isSameColumnDistance(distance: Int): Boolean = distance % 9 == 0

  val fuMoves = Seq(9)
  val keiMoves = Seq(17, 19)
  val ginMoves = Seq(-10, -8, 8, 9, 10)
  val kinMoves = Seq(-9, -1, 1, 8, 9, 10)
  val ouMoves = Seq(-10, -9, -8, -1, 1, 8, 9, 10)
  val nariKinMoves = Seq(-9, -1, 1, 8, 9, 10)

  def abstractCanMove(now: Int, toIndex: Int, isSenteTurnState: Boolean, moves: Seq[Int]): Boolean = {
    val moveDistance = if (isSenteTurnState) now - toIndex else toIndex - now
    moves.contains(moveDistance)
  }

  /** 歩の動きの定義 */
  def fuCanMove(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = abstractCanMove(now, toIndex, isSenteTurnState, fuMoves)

  /** 桂の動きの定義 */
  def keiCanMove(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = abstractCanMove(now, toIndex, isSenteTurnState, keiMoves)

  /** 銀の動きの定義 */
  def ginCanMove(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = abstractCanMove(now, toIndex, isSenteTurnState, ginMoves)

  /** 金の動きの定義 */
  def kinCanMove(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = abstractCanMove(now, toIndex, isSenteTurnState, kinMoves)

  /** 王の動きの定義 */
  def ouCanMove(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = abstractCanMove(now, toIndex, isSenteTurnState, ouMoves)

  /** 成り駒の動きの定義 */
  def nariKinCanMoves(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = abstractCanMove(now, toIndex, isSenteTurnState, nariKinMoves)

  /** 香の動きの定義 */
  def kyoCanMove(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val moveDistance = now - toIndex
    isSenteTurnState match {
      case true => isSameColumnDistance(moveDistance) && moveDistance > 0
      case false => isSameColumnDistance(moveDistance) && moveDistance < 0
    }
  }

  /** 飛車の動きの定義 */
  def leftRightMove(now: Int, toIndex: Int): Boolean = {
    val (nowRow, toRow) = (row(now), row(toIndex))
    nowRow == toRow
  }

  def upDownMove(now: Int, toIndex: Int): Boolean = {
    val absMoveDistance = Math.abs(now - toIndex)
    isSameColumnDistance(absMoveDistance)
  }

  def ryuMove(now: Int, toIndex: Int): Boolean = {
    val absMoveDistance = Math.abs(now - toIndex)
    absMoveDistance == 8 || absMoveDistance == 10
  }

  /** 角の動きの定義 */
  def rightUpLeftDownMove(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => toRow + toColumn == nowColumn + nowRow)
  }

  def leftUpRightDownMove(now: Int, toIndex: Int): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => toRow - toColumn == nowRow - nowColumn)
  }

  def umaMove(now: Int, toIndex: Int): Boolean = {
    val absMoveDistance = Math.abs(now - toIndex)
    absMoveDistance == 1 || absMoveDistance == 9
  }

  /** 盤面内を動いているか */
  def fromToMoveBoard(now: Int, toIndex: Int): Boolean = {
    now <= 80 && toIndex <= 80
  }

  /** 盤面内を横切っていないか */
  def notCrossOnBoard(now: Int, toIndex: Int): Boolean = {
    val moveDistance = now - toIndex
    if ((now % 9) + 1 == 9) { //9筋の時
      if (moveDistance == 8 || moveDistance == -1 || moveDistance == -10 || moveDistance == -19 || moveDistance == 17) false
      else true
    } else if ((now % 9) + 1 == 1) { //1筋の時
      if (moveDistance == -8 || moveDistance == 1 || moveDistance == 10 || moveDistance == -17 || moveDistance == 19) false
      else true
    } else true
  }

  /** 評価関数
    * 1. 駒得の評価値
    * 2. 王との距離の評価値
    * */
  def realKomas = komas.takeRight(40)
  def senteKomaKind: List[ClickedKomaState] = realKomas.filter(_.isSente == true).map(koma => koma.kind)
  def goteKomaKind: List[ClickedKomaState] = realKomas.filter(_.isSente == false).map(koma => koma.kind)
  def senteKomaIndex: List[Int] = realKomas.filter(_.isSente == true).map(koma => koma.index)
  def goteKomaIndex: List[Int] = realKomas.filter(_.isSente == false).map(koma => koma.index)

  //駒を点数に変換
  def senteKomaPoint: List[Double] = {
    val evalMaps: Map[ClickedKomaState, Double] = Map(
      ClickedKomaState.Fu -> 1,
      ClickedKomaState.Kyo -> 3,
      ClickedKomaState.Kei -> 4,
      ClickedKomaState.Gin -> 6,
      ClickedKomaState.Kin -> 8,
      ClickedKomaState.Kaku -> 8,
      ClickedKomaState.Hisha -> 12,
      ClickedKomaState.To -> 9,
      ClickedKomaState.NariKyo -> 8,
      ClickedKomaState.NariKei -> 8,
      ClickedKomaState.NariGin -> 8,
      ClickedKomaState.Uma -> 13,
      ClickedKomaState.Ryu -> 15,
      ClickedKomaState.Ou -> 0,
      ClickedKomaState.Gyoku -> 0
    )
    senteKomaKind.foldLeft(Nil: List[Double])((result, koma) => evalMaps.getOrElse(koma, 0.0) :: result)
  }

  def goteKomaPoint: List[Double] = {
    goteKomaKind.foldLeft(Nil: List[Double]) {
      case (result, koma) =>
        val komaPoint = koma match {
          case ClickedKomaState.Fu => 1
          case ClickedKomaState.Kyo => 3
          case ClickedKomaState.Kei => 4
          case ClickedKomaState.Gin => 6
          case ClickedKomaState.Kin => 8
          case ClickedKomaState.Kaku => 8
          case ClickedKomaState.Hisha => 12
          case ClickedKomaState.To => 9
          case ClickedKomaState.NariKyo => 8
          case ClickedKomaState.NariKei => 8
          case ClickedKomaState.NariGin => 8
          case ClickedKomaState.Uma => 13
          case ClickedKomaState.Ryu => 15
          case ClickedKomaState.Ou => 0
          case ClickedKomaState.Gyoku => 0
          case _ => 0
        }
        komaPoint :: result
    }
  }

  /** 王との距離の評価値 */
  def senteOuDistanceEvaluation: Double = {
    var senteDistanceEvaluationPoint: Double = 0
    val (senteOuRow, senteOuColumn) = (row(findOuGyoku(true)), column(findOuGyoku(true)))

    //先手の駒と先手の王の距離
    for (i <- senteKomaPoint.indices) {
      if (senteKomaIndex(i) < 80) { //盤上の場合
        val nowRow: Int = row(senteKomaIndex(i))
        val nowColumn: Int = column(senteKomaIndex(i))
        val distanceToOu = 0.8 * Math.abs(nowRow - senteOuRow) + 1.2 * Math.abs(nowColumn - senteOuColumn)
        senteDistanceEvaluationPoint = senteDistanceEvaluationPoint + (16 - distanceToOu) * (16 - distanceToOu) * senteKomaPoint(i) / 8 //評価点 = (距離) * (駒の価値)
      } else {
        senteDistanceEvaluationPoint = senteDistanceEvaluationPoint + 12 * 1.5 * senteKomaPoint(i) //持ち駒の場合
      }
    }
    //後手の駒と先手の王の距離
    for (i <- goteKomaPoint.indices) {
      if (goteKomaIndex(i) < 80) { //盤上の場合
      val nowRow: Int = row(goteKomaIndex(i))
        val nowColumn: Int = column(goteKomaIndex(i))
        val distanceToOu = 0.8 * Math.abs(nowRow - senteOuRow) + 1.2 * Math.abs(nowColumn - senteOuColumn)
        senteDistanceEvaluationPoint = senteDistanceEvaluationPoint - (16 - distanceToOu) * (16 - distanceToOu) * goteKomaPoint(i) / 8
      } else {
        senteDistanceEvaluationPoint = senteDistanceEvaluationPoint - 12 * 1.5 * goteKomaPoint(i)
      }
    }
    senteDistanceEvaluationPoint
  }

  def goteOuDistanceEvaluation: Double = {
    var goteDistanceEvaluationPoint: Double = 0
    val (goteOuRow, goteOuColumn) = (row(findOuGyoku(false)), column(findOuGyoku(false)))

    //後手の駒と後手の王の距離
    for (i <- goteKomaPoint.indices) {
      if (goteKomaIndex(i) < 80) { //盤上の場合
        val nowRow: Int = row(goteKomaIndex(i))
        val nowColumn: Int = column(goteKomaIndex(i))
        val distanceToOu = 0.8 * Math.abs(nowRow - goteOuRow) + 1.2 * Math.abs(nowColumn - goteOuColumn)
        goteDistanceEvaluationPoint = goteDistanceEvaluationPoint + (16 - distanceToOu) * (16 - distanceToOu) * goteKomaPoint(i) / 8 //評価点 = (距離) * (駒の価値)
      } else {
        goteDistanceEvaluationPoint = goteDistanceEvaluationPoint + 12 * 1.5 * goteKomaPoint(i) //持ち駒の場合
      }
    }
    //先手の駒と後手の王の距離
    for (i <- senteKomaPoint.indices) {
      if (senteKomaIndex(i) < 80) {
        val nowRow: Int = row(senteKomaIndex(i))
        val nowColumn: Int = column(senteKomaIndex(i))
        val distanceToOu = 0.8 * Math.abs(nowRow - goteOuRow) + 1.2 * Math.abs(nowColumn - goteOuColumn)
        goteDistanceEvaluationPoint =  goteDistanceEvaluationPoint - (16 - distanceToOu) * (16 - distanceToOu) * senteKomaPoint(i) / 8
      } else {
        goteDistanceEvaluationPoint = goteDistanceEvaluationPoint - 12 * 1.5 * senteKomaPoint(i)
      }
    }
    goteDistanceEvaluationPoint
  }

  /** 駒得の評価値 */
  val evalMap: Map[ClickedKomaState, Int] = Map(
    ClickedKomaState.Fu -> 1,
    ClickedKomaState.Kyo -> 3,
    ClickedKomaState.Kei -> 4,
    ClickedKomaState.Gin -> 5,
    ClickedKomaState.Kin -> 6,
    ClickedKomaState.Kaku -> 10,
    ClickedKomaState.Hisha -> 12,
    ClickedKomaState.To -> 6,
    ClickedKomaState.NariKyo -> 5,
    ClickedKomaState.NariKei -> 5,
    ClickedKomaState.NariGin -> 5,
    ClickedKomaState.Uma -> 13,
    ClickedKomaState.Ryu -> 15
  )
  def senteAmountEvaluation: Int = {
    senteKomaKind.foldLeft(0)((result, koma) => result + evalMap.getOrElse(koma, 0))
  }

  def goteAmountEvaluation: Int = {
    goteKomaKind.foldLeft(0) {
      case (result, koma) =>
        val komaPoint = koma match {
          case ClickedKomaState.Fu => 1
          case ClickedKomaState.Kyo => 3
          case ClickedKomaState.Kei => 4
          case ClickedKomaState.Gin => 5
          case ClickedKomaState.Kin => 6
          case ClickedKomaState.Kaku => 10
          case ClickedKomaState.Hisha => 12
          case ClickedKomaState.To => 6
          case ClickedKomaState.NariKyo => 5
          case ClickedKomaState.NariKei => 5
          case ClickedKomaState.NariGin => 5
          case ClickedKomaState.Uma => 13
          case ClickedKomaState.Ryu => 15
          case _ => 0
        }
        result + komaPoint
    }
  }

  //駒得1, 王様との距離1ぐらいを想定
  def senteEvaluation = 15 * senteAmountEvaluation + senteOuDistanceEvaluation
  def goteEvaluation = 15 * goteAmountEvaluation + goteOuDistanceEvaluation
  def evaluationFunction: Int = (senteEvaluation - goteEvaluation).toInt

}
