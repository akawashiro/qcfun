# 高階関数をまともな関数でQuickCheckする

## 高階関数をQuickCheckでテストしてみる
QuickCheckを知っていますか?  
QuickCheckと言うのはHaskellのデータ駆動型のテスト用ライブラリで  
テストしたい関数を指定するとその引数に合わせて適当なテストデータを生成してくれます。  

しかし、高階関数(関数を引数に取る関数)をQuickCheckでテストすると  
どんなテストデータ(関数)を生成してくれるのでしょうか。  
ちょっと確認してみましょう。  
```haskell
-- QCTest.hs
import Test.QuickCheck
import Test.QuickCheck.Function

prop :: Fun Integer Integer -> Bool
prop f = apply f 10 == apply f 20

main = quickCheck prop
```
```bash
% stack runghc QCTest.hs
*** Failed! Falsifiable (after 2 tests and 12 shrinks):
{20->0, _->1}
```
...なんだこの関数は?  
僕の思ってる関数と違うんですが。  
map (+2)とかtailとかそういうやつを生成してほしいなぁ。  

## まともなテスト用関数を生成する
QuickCheckのテストデータが気に入らないので、まともな(主観)テストデータを生成してみました。  
今回生成するテストデータは[Int] -> [Int]の関数に限定します。

そもそも関数を生成するってどうやればいいんでしょうか?  
QuickCheckのように入力と出力の対を列挙するのは筋が悪い気がします。  
mapや(+)、tailといった基本的な関数を合成して関数を生成したいのです。  
我々がプログラミングするときってそうやりますよね。  
入力と出力の組を列挙する人はあんまりいないと思います。  

[Int] -> [Int]型の関数を列挙してみます。  
```haskell
tail
reverse
map (* 10)
map (+ 10)
```
tailやreverseはそもそも[Int] -> [Int] 型です。  
mapは(Int -> Int) -> [Int] -> [Int] 型なので  
(+ 10) :: Int -> Intを渡すと[Int] -> [Int]型になります。  

こんな風にどの関数(例 map)にどの関数(例 (+ 10))を適応すれば  
どんな型(例 [Int] -> [Int])になるかを規則として書き出します。  
型付け規則っぽく書くとこんな感じです。  
![Imgur](https://i.imgur.com/O1jucx2.png)    
ちょっとわかりにくいかも知れません。  

[Int] -> [Int]型の関数を2つ作ってみました。  
![Imgur](https://i.imgur.com/dxFxpxj.png)   
左はmap (+ 10)のような関数、右はmap (+ 10 (* 5))のような関数を表しています。  
一番下の型にある関数の規則を持ってきて、上に必要な型を書いて  
さらにその必要な型をもつ関数の規則を持ってきて...という感じです。  

とりあえずこの規則を使って関数を生成できるようになりました。  
[Int] -> [Int]型の関数を生成した例がこちらです。  
```shell
% stack exec qcfun-exe
> Test datum are following.
> ["tail","reverse","(map (* 75))","tail","reverse"]
...
```

今見せた例はこのようなデータ型で表現されています。
```haskell
data QProg = QMap1 QProg | QMap2 QProg QProg 
             | QTail1 QProg | QTail | QRev | QMult QProg | QAdd QProg | QRand Int deriving (Eq)

instance Show QProg where
  show (QMap1 p) = "(map " ++ show p ++ ")"
  show (QMap2 p1 p2) = "(map " ++ show p1 ++ " " ++ show p2 ++ ")"
  show (QTail1 p) = "(tail " ++ show p ++ ")"
  show (QTail) = "tail"
  show QRev = "reverse"
  show (QMult p) = "(* " ++ show p ++ ")"
  show (QAdd p) = "(* " ++ show p ++ ")"
  show (QRand i) = show i

```
QProg型がプログラム(関数)を表す型です。  
このQProg型に対して適当なShowインスタンスを定義してあげると関数っぽく見えるようになります。  

## 作ったテストデータ用の関数で高階関数をテストする
次に作った関数を使って高階関数をテストしてみましょう。  
しかし作ったテストデータ用の関数はすべてQProg型です。  
つまりQProg型をなんとかして[Int] -> [Int]型に変換したうえで  
テストしたい高階関数に適用する必要があります。  

つまりプログラムの中でプログラムを生成しなければなりません。  
このようなプログラムの生成は一般にメタプログラミングと言われています。  

Haskellでメタプログラミングを行うには2つの方法があります(僕調べ)。  
ひとつ目はプログラムを表す文字列を生成してevalする方法、  
もうひとつはTemplate Haskellを使う方法です。  

evalは使いません。  
なぜならevalの結果はIntやBoolなどのTypableクラスのインスタンスでなければならず  
[Int] -> [Int]型の関数をTypableクラスのインスタンスにする方法がわからなかったからです。  
(そもそもTypableってなんなんだ...)

今回はTemplate Haskellを使います。  
Template Haskellとはコンパイル時に展開されるメタプログラミング手法です。  
実行時ではなくコンパイル時に展開されるのでIOを扱うことはできません(多分)。  

このためテストデータはコンパイル時にすべて決定されることになります。  
これはテストデータをランダムに生成できないことを意味しますが  
今回はStateモナドを用いた擬似乱数でお茶を濁しています。  

動作例がこちらです。
```shell
% stack exec qcfun-exe
...
Test results are following.
[(False,"tail"),(True,"reverse"),(False,"(map (* 75))"),(False,"tail"),(True,"reverse")]
```
擬似乱数でお茶を濁したので何度実行しても同じテストデータでテストされます。  
テストデータを変更するには擬似乱数のシード値を変更したうえで再コンパイルする必要があります。  

## まとめ
高階関数をまともな(主観)関数でテストできるようにしました。  
しかし技術的な制約のためテストデータをコンパイル時に生成するので  
テストデータを完全にランダムに生成することはできませんでした。  

## 参考
<http://haskell.g.hatena.ne.jp/mr_konn/20111218/1324220725>  

## 付録
githubへのリンクです  
<https://github.com/akawashiro/qcfun>  
以下のコマンドで実行できます。  
```shell
git clone https://github.com/akawashiro/qcfun.git
stack build
stack exec qcfun-exe
```
ソースコードはTemplate Haskell的な事情で2つにわかれています。  

