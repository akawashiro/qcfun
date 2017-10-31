# 高階関数をまともな関数でQuickCheckする

## 高階関数をQuickCheckでテストしてみる
QuickCheckを知っていますか?  
QuickCheckと言うのはHaskellのデータ駆動型のテスト用ライブラリで  
テストしたい関数を指定するとその引数に合わせて適当なテストデータを生成してくれます。  

しかし、高階関数(関数を引数に取る関数)をQuickCheckでテストすると  
どんなテストデータ(関数)を生成してくれるのでしょうか。  
ちょっと確認してみましょう。  
```haskell
```

...なんだこの関数は(驚愕)  
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
