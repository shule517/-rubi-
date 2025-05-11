# (rubi)
Common Lisp風のインタプリタをRubyで作ってみる。
Rubiesp(RubiestのためのLisp) の略 + Lispっぽく括弧で (rubi)

# 使い方
```
ruby irubi.rb
```
でREPLが立ち上がります。

```
ruby irubi.rb
lisp:001> (+ 1 2)
3
```

```exit```でインタプリタを終了します。

# 処理の流れ
```(+ 1 2)```の処理の流れ

## 1. 字句解析：トークナイザー(```Rubi::Tokenizer```)
Lispのコードを記号ごとに分割します。
```rb
Rubi::Tokenizer.new.split_tokens("(+ 1 2)")
=> [:"(", :+, 1, 2, :")"]
```

## 2. 構文解析：パーサー(```Rubi::Parser```)
記号の集まりから、()を認識して、配列に変換します。
この状態のことを抽象構文木(AST)って言うらしいです。
```rb
Rubi::Parser.new.parse([:"(", :+, 1, 2, :")"])
=> [[:+, 1, 2]]
```

## 3. 評価:エバリュエーター(```Rubi::Evaluator```)
抽象構文木を評価（実行）します。
※パーサーは複数行のコードを入力する前提になっているので、１行ごとに分けてevalで評価していきます。
```rb
Rubi::Evaluator.new.eval([:+, 1, 2], {}, 0)
=> 3
```

## まとめ
```rb
lisp_code = "(+ 1 2)"
tokens = Rubi::Tokenizer.new.split_tokens(lisp_code) # => [:"(", :+, 1, 2, :")"]
ast = Rubi::Parser.new.parse(tokens) # => [[:+, 1, 2]]
ast.map { |code| Rubi::Evaluator.new.eval(code, {}, 0) } # => [3]
```
