### 9.1 関数型

* if 0 then else false


### 9.4 Curry-Howard 対応

型構築子に伴う規則

* 導入規則 (T-ABS) : 型の要素がどのように作られるか
* 除去規則 (T-APP) : 型の要素がどのように使われるか

* 演習 9.4.1
  * 8-1
    * T-TRUE, T-FALSE が導入規則
    * T-IF が除去規則
  * 8-2
    * T-ZERO が導入規則 -> 間違い T-ZERO と T-PRED が導入規則
      * T-SUCC は導入規則?
    * T-ISZERO が除去規則


* 導入形式・除去形式は型理論と論理のつながりに由来
  * Curry-Howard 対応という

### 9.5 型消去と型付け可能性

* プログラムは評価される前に型無しの形式に戻される
  * 単純型付き項を型無しの項へ移す消去関数によって形式化される

* 評価は型消去と可換である
  * 評価してから型消去しても、型消去してから評価しても同じ項が得られる

### 9.6 Curry スタイル 対 Church スタイル

* 単純型付き計算の構文の上に直接定義された評価関係
* 型無し計算へのコンパイル + 型無しの項の上の評価関係

* Curry スタイル
  * 項の定義・それらの振る舞いを定義し、望まぬ振る舞いを拒否する型システムを与える

* Church スタイル
  * 項を定義し、正しく型付けされた項を識別し、それらに対してのみ意味を与えることである
