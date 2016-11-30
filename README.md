# 将棋アプリDocs------------## 基本動作### 実行まで- takashi-shogiappフォルダ直下のscala_shogiフォルダで、sbt comoile/runをすることで実行が可能です### 駒を動かすとき- 選択したい駒をクリックして下さい	- (自分の手番では、相手の駒を動かすことはできない)	- (頑張って同じ場所のクリックを連打すると、駒を動かす前のクリックが遅れて画面で反映される可能性がありますが、その場合は、関係ない他の場所をクリックすれば選択を外れます)	- 駒の移動条件を満たす場合、そこに駒が移動して相手の手番になる	- (移動条件を満たさない場合は、駒の選択自体がキャンセルされる)	### 表示機能について- "先手番""後手番"、"二歩です"、"先手の勝ち""後手の勝ち"、"成り/不成"があります- A,B,C,D,待った があります- 後手の駒でも、見づらいため、先手向きの文字で表示が出るようにしました### 成りの条件- 必ず成らなければならない場合	- 強制的に駒が成ります- 成る成らないを選択できる場合	- 手番側の駒で、成り/不成と出るので、		- 成る場合は、"成り"のどちらかをクリック		- 成らない場合は、"不成"のどちらかをクリックして下さい	- 選択されたら、その駒の成り不成が確定し、相手の手番になります### 勝敗の決定- 王が取られた側の負けとなります- "先手の勝ち""後手の勝ち"という表示が出て、その後駒の選択が出来なくなります### 持ち駒について- 持ち駒のスペースには駒ごとに重ねて置かれます------------## 追加機能(今後さらに追加予定)#### 盤面の初期化- "ABCD"と書かれた駒をクリックすると、4種類の初期局面から対局がまたスタートとなります	- "A" 通常の将棋のルール通りの駒配置で、局面がスタートします	- "B" 歩の下の先手は8,9段目、後手は1,2段目に歩以外の駒がランダムに配置されます	- "C" 先手は6~9段目、後手は1~4段目に駒がランダムに配置されます	- "D" 9×9マス中にランダムに駒が配置されます##### 詰み判定- "詰"と書いた駒のいずれかをクリックすると、現局面において、以下の表示が出ます    - 勝ちがある場合、"勝ちあり"    - 自玉が積んでいる場合、"先手詰み""後手詰み"    - 上記二つに当てはまらない場合、"詰みなし"- 頑張って連打を繰り返しているとMacが少し重くなるリスクがあるのでご注意下さい    	 	 ##### 待った- "待った"と書いた駒のいずれかをクリックすると、1手だけ前へ巻き戻すことができます------------## 設計### ShogiBoard.scala- 基本的な実行を担う### Board.scala- 盤面を定義しているクラス- ShogiBoardオブジェクトのcellObjGroup関数の中で使われる関数を多く含む### Koma.scala- 駒の定義されているクラス、基本的にはBoard.scalaにて使われる### 描画- ScalaFXを使っています