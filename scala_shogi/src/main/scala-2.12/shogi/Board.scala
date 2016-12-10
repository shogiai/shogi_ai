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

  def findKomaKind(komaKind: ClickedKomaState, isSenteKoma: Boolean): Int = {
    komas.filter(_.kind == komaKind).filter(_.isSente == isSenteKoma) match {
      case List(Koma(kind, index, isSente, onBoard)) => index
      case _ => -1
    }
  }

  def findPlaceKomaKind(place: Int): ClickedKomaState = {
    komas.filter(_.index == place) match {
      case List(Koma(kind, index, isSente, onBoard)) => kind
      case _ => ClickedKomaState.Blank //起こりえない
    }
  }

  def findPlaceKoma(place: Int): Option[Koma] = {
    komas.filter(_.index == place) match {
      case List(koma) => Some(koma)
      case _ => None
    }
  }

  //stockNariIndex
  def findPlaceKomaisSente(place: Int): Option[Boolean] = {
    komas.filter(_.index == place) match {
      case List(Koma(kind, index, isSente, onBoard)) => Some(isSente)
      case _ => None //起こりえない
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
      (koma.kind == ClickedKomaState.Ou && koma.isSente == isSenteTurnState))
  }

  //下にどれだけ動けるか
  def checkMateDownJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) != nowColumn || row(koma.index) >= toRow || row(koma.index) <= nowRow || !koma.onBoard ||
      (koma.kind == ClickedKomaState.Ou && koma.isSente == isSenteTurnState))
  }

  //左にどれだけ動けるか
  def checkMateLeftJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => row(koma.index) != nowRow || column(koma.index) <= toColumn || column(koma.index) >= nowColumn || !koma.onBoard ||
      (koma.kind == ClickedKomaState.Ou && koma.isSente == isSenteTurnState))
  }

  //右にどれだけ動けるか, 位置を判定
  def checkMateRightJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => row(koma.index) != nowRow || column(koma.index) >= toColumn || column(koma.index) <= nowColumn || !koma.onBoard ||
      (koma.kind == ClickedKomaState.Ou && koma.isSente == isSenteTurnState))
  }

  //右上方向にどれだけ動けるか
  def checkMateRightUpJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) + row(koma.index) != nowColumn + nowRow ||
      (nowColumn >= column(koma.index) && row(koma.index) >= nowRow) || (toColumn <= column(koma.index) && row(koma.index) <= toRow) ||
      !koma.onBoard || (koma.kind == ClickedKomaState.Ou && koma.isSente == isSenteTurnState))
  }

  //左下方向にどれだけ動けるか
  def checkMateLeftDownJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) + row(koma.index) != nowColumn + nowRow ||
      (toColumn >= column(koma.index) && row(koma.index) >= toRow) || (nowColumn <= column(koma.index) && row(koma.index) <= nowRow) ||
      !koma.onBoard || (koma.kind == ClickedKomaState.Ou && koma.isSente == isSenteTurnState))
  }

  //左上方向にどれだけ動けるか
  def checkMateLeftUpJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) - row(koma.index) != nowColumn - nowRow ||
      (toColumn >= column(koma.index) && toRow >= row(koma.index)) || (nowColumn <= column(koma.index) && nowRow <= row(koma.index)) ||
      !koma.onBoard || (koma.kind == ClickedKomaState.Ou && koma.isSente == isSenteTurnState))
  }

  //右下方向にどれだけ動けるか
  def checkMateRightDownJumpCheck(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val (nowRow, nowColumn) = (row(now), column(now))
    val (toRow, toColumn) = (row(toIndex), column(toIndex))
    komas.forall(koma => column(koma.index) - row(koma.index) != nowColumn - nowRow ||
      (nowColumn >= column(koma.index) && nowRow >= row(koma.index)) || (toColumn <= column(koma.index) && toRow <= row(koma.index)) ||
      !koma.onBoard || (koma.kind == ClickedKomaState.Ou && koma.isSente == isSenteTurnState))
  }

  //同じ場所をクリックしていない
  def notOwn(now: Int, toIndex: Int): Boolean = now != toIndex

  /** 歩の動きの定義 */
  def fuCanMove(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    isSenteTurnState match {
      case true => now - toIndex == 9
      case false => now - toIndex == -9
    }
  }

  /** 香の動きの定義 */
  def kyoCanMove(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val moveDistance = now - toIndex
    isSenteTurnState match {
      case true => moveDistance % 9 == 0 && moveDistance > 0
      case false => moveDistance % 9 == 0 && moveDistance < 0
    }
  }

  /** 桂の動きの定義 */
  def keiCanMove(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val moveDistance = now - toIndex
    isSenteTurnState match {
      case true => moveDistance == 17 || moveDistance == 19
      case false => moveDistance == -17 || moveDistance == -19
    }
  }

  /** 銀の動きの定義 */
  def ginCanMove(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val moveDistance = now - toIndex
    val absMoveDistance = Math.abs(now - toIndex)
    isSenteTurnState match {
      case true => absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == 9
      case false => absMoveDistance == 8 || absMoveDistance == 10 || moveDistance == -9
    }
  }

  /** 金の動きの定義 */
  def kinCanMove(now: Int, toIndex: Int, isSenteTurnState: Boolean): Boolean = {
    val moveDistance = now - toIndex
    val absMoveDistance = Math.abs(now - toIndex)
    isSenteTurnState match {
      case true => absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10
      case false => absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10
    }
  }

  /** 王の動きの定義 */
  def ouCanMove(now: Int, toIndex: Int): Boolean = {
    val absMoveDistance = Math.abs(now - toIndex)
    absMoveDistance == 1 || absMoveDistance == 9 || absMoveDistance == 8 || absMoveDistance == 10
  }

  /** 飛車の動きの定義 */
  def leftRightMove(now: Int, toIndex: Int): Boolean = {
    val (nowRow, toRow) = (row(now), row(toIndex))
    nowRow == toRow
  }

  def upDownMove(now: Int, toIndex: Int): Boolean = {
    val absMoveDistance = Math.abs(now - toIndex)
    absMoveDistance % 9 == 0
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

  /** 成り駒の動きの定義 */
  def nariKinCanMove(now: Int, toIndex: Int, isSenteTurnState:Boolean): Boolean = {
    val moveDistance = now - toIndex
    val absMoveDistance = Math.abs(now - toIndex)
    isSenteTurnState match {
      case true => absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == 8 || moveDistance == 10
      case false => absMoveDistance == 1 || absMoveDistance == 9 || moveDistance == -8 || moveDistance == -10
    }
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
  def senteKomaPoint: List[Int] = {
    var senteKomaPoint: List[Int] = Nil
    senteKomaKind.foreach(komaKind => {
      senteKomaPoint = komaKind match {
        case ClickedKomaState.Fu => 1 :: senteKomaPoint
        case ClickedKomaState.Kyo => 3 :: senteKomaPoint
        case ClickedKomaState.Kei => 4 :: senteKomaPoint
        case ClickedKomaState.Gin => 5 :: senteKomaPoint
        case ClickedKomaState.Kin => 6 :: senteKomaPoint
        case ClickedKomaState.Kaku => 10 :: senteKomaPoint
        case ClickedKomaState.Hisha => 12 :: senteKomaPoint
        case ClickedKomaState.To => 9 :: senteKomaPoint
        case ClickedKomaState.NariKyo => 8 :: senteKomaPoint
        case ClickedKomaState.NariKei => 8 :: senteKomaPoint
        case ClickedKomaState.NariGin => 7 :: senteKomaPoint
        case ClickedKomaState.Uma => 16 :: senteKomaPoint
        case ClickedKomaState.Ryu => 18 :: senteKomaPoint
        case ClickedKomaState.Ou => 0 :: senteKomaPoint
        case _ => senteKomaPoint
      }
    })
    senteKomaPoint = senteKomaPoint.reverse
    senteKomaPoint
  }

  def goteKomaPoint: List[Int] = {
    var goteKomaPoint: List[Int] = Nil
    goteKomaKind.foreach(komaKind => {
      goteKomaPoint = komaKind match {
        case ClickedKomaState.Fu => 1 :: goteKomaPoint
        case ClickedKomaState.Kyo => 3 :: goteKomaPoint
        case ClickedKomaState.Kei => 4 :: goteKomaPoint
        case ClickedKomaState.Gin => 5 :: goteKomaPoint
        case ClickedKomaState.Kin => 6 :: goteKomaPoint
        case ClickedKomaState.Kaku => 10 :: goteKomaPoint
        case ClickedKomaState.Hisha => 12 :: goteKomaPoint
        case ClickedKomaState.To => 9 :: goteKomaPoint
        case ClickedKomaState.NariKyo => 8 :: goteKomaPoint
        case ClickedKomaState.NariKei => 8 :: goteKomaPoint
        case ClickedKomaState.NariGin => 7 :: goteKomaPoint
        case ClickedKomaState.Uma => 16 :: goteKomaPoint
        case ClickedKomaState.Ryu => 18 :: goteKomaPoint
        case ClickedKomaState.Ou => 0 :: goteKomaPoint
        case _ => goteKomaPoint
      }
    })
    goteKomaPoint = goteKomaPoint.reverse
    goteKomaPoint
  }

  /** 王との距離の評価値 */
  def senteOuDistanceEvaluation: Double = {
    var senteDistanceEvaluationPoint: Double = 0
    val (senteOuRow, senteOuColumn) = (row(findKomaKind(ClickedKomaState.Ou, true)), column(findKomaKind(ClickedKomaState.Ou, true)))

    //先手の駒と先手の王の距離
    for (i <- 0 to senteKomaPoint.length - 1) {
      if (senteKomaIndex(i) < 80) {
        //盤上の場合
        val nowRow: Int = row(senteKomaIndex(i))
        val nowColumn: Int = column(senteKomaIndex(i))
        val distanceToOu = 0.8 * Math.abs(nowRow - senteOuRow) + 1.2 * Math.abs(nowColumn - senteOuColumn)
        senteDistanceEvaluationPoint = senteDistanceEvaluationPoint + (16 - distanceToOu) * senteKomaPoint(i) //評価点 = (距離) * (駒の価値)
      } else {
        senteDistanceEvaluationPoint = senteDistanceEvaluationPoint + 12 * senteKomaPoint(i) //持ち駒の場合
      }
    }
    //後手の駒と先手の王の距離
    for (i <- 0 to goteKomaPoint.length - 1) {
      if (goteKomaIndex(i) < 80) { //盤上の場合
      val nowRow: Int = row(goteKomaIndex(i))
        val nowColumn: Int = column(goteKomaIndex(i))
        val distanceToOu = 0.8 * Math.abs(nowRow - senteOuRow) + 1.2 * Math.abs(nowColumn - senteOuColumn)
        senteDistanceEvaluationPoint = (senteDistanceEvaluationPoint - (16 - distanceToOu) * goteKomaPoint(i)).toInt
      } else {
        senteDistanceEvaluationPoint = senteDistanceEvaluationPoint - 12 * senteKomaPoint(i)
      }
    }
    senteDistanceEvaluationPoint
  }

  def goteOuDistanceEvaluation: Double = {
    var goteDistanceEvaluationPoint: Double = 0
    val (goteOuRow, goteOuColumn) = (row(findKomaKind(ClickedKomaState.Ou, false)), column(findKomaKind(ClickedKomaState.Ou, false)))

    //後手の駒と後手の王の距離
    for (i <- 0 to goteKomaPoint.length - 1) {
      if (goteKomaIndex(i) < 80) { //盤上の場合
      val nowRow: Int = row(goteKomaIndex(i))
        val nowColumn: Int = column(goteKomaIndex(i))
        val distanceToOu = 0.8 * Math.abs(nowRow - goteOuRow) + 1.2 * Math.abs(nowColumn - goteOuColumn)
        goteDistanceEvaluationPoint = goteDistanceEvaluationPoint + (16 - distanceToOu) * goteKomaPoint(i) //評価点 = (距離) * (駒の価値)
      } else {
        goteDistanceEvaluationPoint = goteDistanceEvaluationPoint + 12 * goteKomaPoint(i) //持ち駒の場合
      }
    }
    //先手の駒と後手の王の距離
    for (i <- 0 to senteKomaPoint.length - 1) {
      if (senteKomaIndex(i) < 80) {
        val nowRow: Int = row(senteKomaIndex(i))
        val nowColumn: Int = column(senteKomaIndex(i))
        val distanceToOu = 0.8 * Math.abs(nowRow - goteOuRow) + 1.2 * Math.abs(nowColumn - goteOuColumn)
        goteDistanceEvaluationPoint =  goteDistanceEvaluationPoint - (16 - distanceToOu) * senteKomaPoint(i)
      } else {
        goteDistanceEvaluationPoint = goteDistanceEvaluationPoint - 12 * goteKomaPoint(i)
      }
    }
    goteDistanceEvaluationPoint
  }

  /** 駒得の評価値 */
  def senteAmountEvaluation: Int = {
    var senteAmountEvaluationPoint: Int = 0
    senteKomaKind.foreach(komaKind =>
      komaKind match {
        case ClickedKomaState.Fu => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 1
        case ClickedKomaState.Kyo => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 3
        case ClickedKomaState.Kei => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 4
        case ClickedKomaState.Gin => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 5
        case ClickedKomaState.Kin => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 6
        case ClickedKomaState.Kaku => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 10
        case ClickedKomaState.Hisha => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 12
        case ClickedKomaState.To => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 9
        case ClickedKomaState.NariKyo => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 8
        case ClickedKomaState.NariKei => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 8
        case ClickedKomaState.NariGin => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 7
        case ClickedKomaState.Uma => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 16
        case ClickedKomaState.Ryu => senteAmountEvaluationPoint = senteAmountEvaluationPoint + 18
        case _ =>
      }
    )
    senteAmountEvaluationPoint
  }

  def goteAmountEvaluation: Int = {
    var goteAmountEvaluationPoint: Int = 0
    goteKomaKind.foreach(komaKind =>
      komaKind match {
        case ClickedKomaState.Fu => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 1
        case ClickedKomaState.Kyo => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 3
        case ClickedKomaState.Kei => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 4
        case ClickedKomaState.Gin => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 5
        case ClickedKomaState.Kin => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 6
        case ClickedKomaState.Kaku => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 10
        case ClickedKomaState.Hisha => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 12
        case ClickedKomaState.To => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 9
        case ClickedKomaState.NariKyo => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 8
        case ClickedKomaState.NariKei => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 8
        case ClickedKomaState.NariGin => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 7
        case ClickedKomaState.Uma => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 16
        case ClickedKomaState.Ryu => goteAmountEvaluationPoint = goteAmountEvaluationPoint + 18
        case _ =>
      }
    )
    goteAmountEvaluationPoint
  }

  //駒得8, 王様との距離2ぐらいを想定
  def senteEvaluation = 20 * senteAmountEvaluation + senteOuDistanceEvaluation
  def goteEvaluation = 20 * goteAmountEvaluation + goteOuDistanceEvaluation
  def evaluationFunction: Int = (senteEvaluation - goteEvaluation).toInt

}
