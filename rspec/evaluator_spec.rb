require_relative '../rubi.rb'

describe Rubi::Evaluator do
  describe '#eval' do
    subject { ast.map { |code| puts "-----------"; puts "lisp: #{code}"; evaluator.eval(code, {}, 0) }.last }

    let(:evaluator) { Rubi::Evaluator.new(debug: true) }
    let(:ast) do
      parser = Rubi::Parser.new
      ast = parser.parse(tokens)
      parser.expand_syntactic_sugar(ast)
    end
    let(:tokens) { Rubi::Tokenizer.new.split_tokens(str) }

    describe '#let' do
      context '変数１つ宣言' do
        let(:str) do
          <<~LISP
            (let
              ((x 2))
              x)
          LISP
        end
        it { is_expected.to eq 2 }
      end

      context '式が２つ' do
        let(:str) do
          <<~LISP
            (let
              ((x 2))
              (+ 1 x)
              (+ 2 x))
          LISP
        end
        it { is_expected.to eq 4 }
      end

      context 'グローバルと同じ変数を宣言する' do
        let(:str) do
          <<~LISP
            (setq x 3)
            (let
              ((x 2))
              x)
            x
          LISP
        end
        it { is_expected.to eq 3 }
      end

      context '変数２つ宣言' do
        let(:str) do
          <<~LISP
            (let
              ((x 2) (y 3))
              (+ x y))
          LISP
        end
        it { is_expected.to eq 5 }
      end

      context 'グローバル変数 と ローカル変数で 同じ名前の変数を定義する' do
        let(:str) do
          <<~LISP
            (setq x 3)
            (let
              ((x 2) (y 3))
              (+ x y))
            x
          LISP
        end
        it { is_expected.to eq 3 }
      end

      context 'letをネストして、2階層目の変数を評価' do
        let(:str) do
          <<~LISP
            (let ((x 1))
              (let ((x 2))
                x))
          LISP
        end
        it { is_expected.to eq 2 }
      end

      context 'letをネストして、1階層目の変数を評価' do
        let(:str) do
          <<~LISP
            (let ((x 1))
              (let ((x 2))
                x)
              x)
          LISP
        end
        it { is_expected.to eq 1 }
      end
    end

    describe '#setq' do
      context '変数を定義するだけ' do
        let(:str) do
          <<~LISP
            (setq x 10)
          LISP
        end
        it { is_expected.to eq 10 }
      end

      context '変数を定義して、評価' do
        let(:str) do
          <<~LISP
            (setq x 10)
            x
          LISP
        end
        it { is_expected.to eq 10 }
      end

      context '変数に計算結果を保存する' do
        let(:str) do
          <<~LISP
            (setq x (+ 1 2))
            x
          LISP
        end
        it { is_expected.to eq 3 }
      end
    end

    describe '#lambda' do
      context '引数なし' do
        context '引数なしの関数定義' do
          let(:str) do
            <<~LISP
              (lambda () (+ 1 2))
            LISP
          end

          it do
            proc = subject
            expect(proc).to be_instance_of(Proc)
            expect(proc.call).to eq 3
          end
        end

        context '引数なしの関数定義＆実行①' do
          let(:str) do
            <<~LISP
              ((lambda () (+ 1 2)))
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context '引数なしの関数定義＆実行②' do
          let(:str) do
            <<~LISP
              ((lambda () (+ 2 3)))
            LISP
          end
          it { is_expected.to eq 5 }
        end
      end

      context '引数あり' do
        context '引数ありの関数定義' do
          let(:str) do
            <<~LISP
              (lambda (x) (+ x 2))
            LISP
          end

          it do
            proc = subject
            expect(proc).to be_instance_of(Proc)
            expect(proc.call(3)).to eq 5
          end
        end

        context '引数ありの関数定義＆実行①' do
          let(:str) do
            <<~LISP
              ((lambda (x) (+ x 2)) 4)
            LISP
          end
          it { is_expected.to eq 6 }
        end

        context '引数ありの関数定義＆実行②' do
          let(:str) do
            <<~LISP
              ((lambda (x) (+ 1 x)) 4)
            LISP
          end
          it { is_expected.to eq 5 }
        end

        context '引数複数の関数定義＆実行' do
          let(:str) do
            <<~LISP
              ((lambda (x y) (* x y)) 3 4)
            LISP
          end
          it { is_expected.to eq 12 }
        end
      end
    end

    describe '#defun' do
      context '関数の定義のみ' do
        let(:str) { "(defun double (x) (* x 2))" }
        it { is_expected.to eq :double }
      end

      context '引数なしの関数定義' do
        let(:str) do
          <<~LISP
            (defun san () (+ 1 2))
            (san)
          LISP
        end
        it { is_expected.to eq 3 }
      end

      context '引数ありの関数（引数を未使用）' do
        let(:str) do
          <<~LISP
            (defun san (x) (+ 1 2))
            (san 1)
          LISP
        end
        it { is_expected.to eq 3 }
      end

      context '関数を定義して実行する' do
        let(:str) do
          <<~LISP
            (defun double (x) (* x 2))
            (double 1)
          LISP
        end
        it { is_expected.to eq 2 }
      end

      context '関数を定義して実行する' do
        let(:str) do
          <<~LISP
            (defun square (x) (* x x))
            (square 3)
          LISP
        end
        it { is_expected.to eq 9 }
      end

      context '関数は参照しない' do
        let(:str) do
          <<~LISP
            (defun double (x) (* x 2))
            double
          LISP
        end
        it { expect { subject }.to raise_error }
      end

      context '関数を評価する(function)' do
        let(:str) do
          <<~LISP
            (defun double (x) (* x 2))
            (function double)
          LISP
        end

        it do
          proc = subject
          expect(proc).to be_instance_of(Proc)
          expect(proc.call(3)).to eq 6
        end
      end

      context '関数を評価する(function)＆実行' do
        let(:str) do
          <<~LISP
            (defun double (x) (* x 2))
            (funcall (function double) 3)
          LISP
        end
        it { is_expected.to eq 6 }
      end

      context "関数を評価する(#')" do
        let(:str) do
          <<~LISP
            (defun double (x) (* x 2))
            #'double
          LISP
        end

        it do
          proc = subject
          expect(proc).to be_instance_of(Proc)
          expect(proc.call(3)).to eq 6
        end
      end

      context '関数と変数を区別する' do
        let(:str) do
          <<~LISP
            (setq double 3)
            (defun double (x) (* x 2))
            (double double)
          LISP
        end
        it { is_expected.to eq 6 }
      end
    end

    describe '#list' do
      context '数値の配列' do
        let(:str) do
          <<~LISP
            (list 1 2 3)
          LISP
        end
        it { is_expected.to eq [1, 2, 3] }
      end

      context '関数呼び出しあり' do
        let(:str) do
          <<~LISP
            (list 1 2 (+ 3 4))
          LISP
        end
        it { is_expected.to eq [1, 2, 7] }
      end
    end

    describe '#car' do
      let(:str) do
        <<~LISP
          (car '(1 2 3))
        LISP
      end
      it { is_expected.to eq 1 }
    end

    describe '#cdr' do
      let(:str) do
        <<~LISP
          (cdr '(1 2 3))
        LISP
      end
      it { is_expected.to eq [2, 3] }
    end

    describe '#cons' do
      context '(cons atom atom) ペアを作る' do
        let(:str) do
          <<~LISP
            (cons 'a 'b)
          LISP
        end
        it do
          result = subject
          expect(result).to be_instance_of(Rubi::Cons)
          expect(result.car).to eq :a
          expect(result.cdr).to eq :b
        end
      end

      context '(cons atom list)' do
        let(:str) do
          <<~LISP
            (cons 0 '(1 2 3))
          LISP
        end
        it { is_expected.to eq [0, 1, 2, 3] }
      end

      context '(cons atom nil)' do
        let(:str) do
          <<~LISP
            (cons 0 nil)
          LISP
        end
        it { is_expected.to eq [0] }
      end

      context '(cons list atom)' do
        let(:str) do
          <<~LISP
            (cons '(1 2 3) 0)
          LISP
        end
        it do
          result = subject
          expect(result).to be_instance_of(Rubi::Cons)
          expect(result.car).to eq [1, 2, 3]
          expect(result.cdr).to eq 0
        end
      end

      context '(cons list list)' do
        let(:str) do
          <<~LISP
            (cons '(1 2 3) '(4 5 6))
          LISP
        end
        it { is_expected.to eq [1, 2, 3, 4, 5, 6] }
      end

      context '(cons list nil)' do
        let(:str) do
          <<~LISP
            (cons '(1 2 3) nil)
          LISP
        end
        it { is_expected.to eq [[1, 2, 3]] }
      end

      context 'リストを作る' do
        let(:str) do
          <<~LISP
            (cons 1 (cons 2 (cons 3 nil)))
          LISP
        end
        it { is_expected.to eq [1, 2, 3] }
      end
    end

    describe '#null' do
      context 'リストが空の場合' do
        let(:str) do
          <<~LISP
            (null '())
          LISP
        end
        it { is_expected.to eq true }
      end

      context 'リストが空でない場合' do
        let(:str) do
          <<~LISP
            (null '(1 2 3))
          LISP
        end
        it { is_expected.to eq nil }
      end
    end

    describe '#atom' do
      context 'listの場合' do
        let(:str) do
          <<~LISP
            (atom (list 1 2))
          LISP
        end
        it { is_expected.to eq nil }
      end

      context 'atomの場合' do
        context '数値の場合' do
          let(:str) do
            <<~LISP
              (atom 1)
            LISP
          end
          it { is_expected.to eq true }
        end

        context 'シンボルの場合' do
          let(:str) do
            <<~LISP
              (atom a)
            LISP
          end
          it { is_expected.to eq true }
        end

        context '文字列の場合' do
          let(:str) do
            <<~LISP
              (atom "あいうえお")
            LISP
          end
          it { is_expected.to eq true }
        end

        context 'nilの場合' do
          let(:str) do
            <<~LISP
              (atom nil)
            LISP
          end
          it { is_expected.to eq true }
        end
      end
    end

    describe '#quote' do
      context 'シンボル' do
        let(:str) do
          <<~LISP
            (quote a)
          LISP
        end
        it { is_expected.to eq :a }
      end

      context '数値' do
        let(:str) do
          <<~LISP
            (quote 1)
          LISP
        end
        it { is_expected.to eq 1 }
      end

      context 'リスト' do
        let(:str) do
          <<~LISP
            (quote (list 1 2 a))
          LISP
        end
        it { is_expected.to eq [:list, 1, 2, :a] }
      end

      context 'quoteがネストしてる' do
        let(:str) do
          <<~LISP
            (quote (quote a))
          LISP
        end
        it { is_expected.to eq [:quote, :a] }
      end

      context '糖衣構文①：シンボル' do
        let(:str) do
          <<~LISP
            'a
          LISP
        end
        it { is_expected.to eq :a }
      end

      context '糖衣構文②：配列' do
        let(:str) do
          <<~LISP
            '(1 2 a)
          LISP
        end
        it { is_expected.to eq [1, 2, :a] }
      end
    end

    describe '#funcall' do
      context '(quote +)' do
        let(:str) do
          <<~LISP
            (funcall (quote +) 1 2)
          LISP
        end
        it { is_expected.to eq 3 }
      end

      context "'+" do
        let(:str) do
          <<~LISP
            (funcall '+ 1 2)
          LISP
        end
        it { is_expected.to eq 3 }
      end
    end

    describe '#defmacro' do
      context 'マクロの定義のみ' do
        let(:str) do
          <<~LISP
            (defmacro nil! (var) (list 'setq var nil))
          LISP
        end
        it { is_expected.to eq :nil! }
      end

      context 'マクロの定義＆実行①' do
        let(:str) do
          <<~LISP
            (setq x 1)
            (defmacro nil! (var) (list 'setq var nil))
            (nil! x)
          LISP
        end
        it { is_expected.to eq nil }
      end

      context 'マクロの定義＆実行②' do
        let(:str) do
          <<~LISP
            (setq x 0)
            (defmacro 1! (var) (list 'setq var 1))
            (1! x)
          LISP
        end
        it { is_expected.to eq 1 }
      end

      context '(quote +)' do
        let(:str) do
          <<~LISP
            (setq x 1)
            (defmacro nil! (var) (list 'setq var nil))
            (nil! x)
            x
          LISP
        end
        it { is_expected.to eq nil }
      end
    end

    describe '#値' do
      context '整数' do
        let(:str) { "1" }
        it { is_expected.to eq 1 }
      end

      context '小数' do
        let(:str) { "1.1" }
        it { is_expected.to eq 1.1 }
      end

      context '文字列' do
        context 'quoteの場合' do
          let(:str) { "`あいうえお" }
          it { is_expected.to eq :"あいうえお" }
        end

        context '文字列そのままの場合' do
          let(:str) { "あいうえお" }
          it { expect { subject }.to raise_error }
        end
      end

      context '()' do
        let(:str) { "()" }
        it { is_expected.to eq nil }
      end

      context 't' do
        let(:str) { "t" }
        it { is_expected.to eq true }
      end

      context 'nil' do
        let(:str) { "nil" }
        it { is_expected.to eq nil }
      end
    end

    describe '#+' do
      context '()１つ' do
        let(:str) { "(+ 1 2)" }
        it { is_expected.to eq 3 }
      end

      context '()２つ' do
        let(:str) { "(+ (+ 1 2) 3)" }
        it { is_expected.to eq 6 }
      end

      context '()３つ' do
        let(:str) { "(+ (+ 1 (+ 1 2)) 3)" }
        it { is_expected.to eq 7 }
      end

      context '引数が３つ以上' do
        let(:str) { "(+ 1 2 3 4 5 (+ 2 4) 7 8 9 10)" }
        it { is_expected.to eq 55 }
      end

      context '引数なし' do
        let(:str) { "(+)" }
        it { is_expected.to eq 0 }
      end

      context '小数' do
        let(:str) { "(+ 1.2 2.3)" }
        it { is_expected.to eq 3.5 }
      end
    end

    describe '#-' do
      context '()１つ' do
        let(:str) { "(- 2 1)" }
        it { is_expected.to eq 1 }
      end

      context '()２つ' do
        let(:str) { "(- (+ 2 3) 3)" }
        it { is_expected.to eq 2 }
      end

      context '引数が３つ以上' do
        let(:str) { "(- 1 2 3 4 5 6 7 8 9 10)" }
        it { is_expected.to eq -53 }
      end

      context '小数' do
        let(:str) { "(- 4.5 1.1)" }
        it { is_expected.to eq 3.4 }
      end
    end

    describe '#*' do
      context '()１つ' do
        let(:str) { "(* 2 3)" }
        it { is_expected.to eq 6 }
      end

      context '()２つ' do
        let(:str) { "(* (* 2 3) 3)" }
        it { is_expected.to eq 18 }
      end

      context '引数が３つ以上' do
        let(:str) { "(* 2 3 4)" }
        it { is_expected.to eq 24 }
      end

      context '小数' do
        let(:str) { "(* 1.2 3.4)" }
        it { is_expected.to eq 4.08 }
      end
    end

    describe '#/' do
      context '()１つ' do
        let(:str) { "(/ 4 2)" }
        it { is_expected.to eq 2 }
      end

      context '()２つ' do
        let(:str) { "(/ 4 (/ 4 2))" }
        it { is_expected.to eq 2 }
      end

      context '引数が３つ以上' do
        let(:str) { "(/ 32 4 4)" }
        it { is_expected.to eq 2 }
      end

      context '小数' do
        let(:str) { "(/ 1.2 2)" }
        it { is_expected.to eq 0.6 }
      end
    end

    describe '#=' do
      context '整数同士' do
        context '一致する場合' do
          let(:str) do
            <<~LISP
              (= 1 1)
            LISP
          end
          it { is_expected.to eq true }
        end

        context '一致しない場合' do
          let(:str) do
            <<~LISP
              (= 1 2)
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '小数同士' do
        context '一致する場合' do
          let(:str) do
            <<~LISP
              (= 1.2 1.2)
            LISP
          end
          it { is_expected.to eq true }
        end

        context '一致しない場合' do
          let(:str) do
            <<~LISP
              (= 1.2 1.3)
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '整数と小数' do
        context '一致する場合' do
          let(:str) do
            <<~LISP
              (= 1 1.0)
            LISP
          end
          it { is_expected.to eq true }
        end

        context '一致しない場合' do
          let(:str) do
            <<~LISP
              (= 1 1.1)
            LISP
          end
          it { is_expected.to eq nil }
        end
      end
    end

    describe '!=' do
      context '整数同士' do
        context '一致する場合' do
          let(:str) do
            <<~LISP
              (/= 1 1)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context '一致しない場合' do
          let(:str) do
            <<~LISP
              (/= 1 2)
            LISP
          end
          it { is_expected.to eq true }
        end
      end

      context '小数同士' do
        context '一致する場合' do
          let(:str) do
            <<~LISP
              (/= 1.2 1.2)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context '一致しない場合' do
          let(:str) do
            <<~LISP
              (/= 1.2 1.3)
            LISP
          end
          it { is_expected.to eq true }
        end
      end

      context '整数と小数' do
        context '一致する場合' do
          let(:str) do
            <<~LISP
              (/= 1 1.0)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context '一致しない場合' do
          let(:str) do
            <<~LISP
              (/= 1 1.1)
            LISP
          end
          it { is_expected.to eq true }
        end
      end
    end

    describe '#<' do
      context 'aの方が大きい場合' do
        let(:str) do
          <<~LISP
            (< 2 1)
          LISP
        end
        it { is_expected.to eq nil }
      end

      context 'aとbが一致している場合' do
        let(:str) do
          <<~LISP
            (< 1 1)
          LISP
        end
        it { is_expected.to eq nil }
      end

      context 'bの方が大きい場合' do
        let(:str) do
          <<~LISP
            (< 1 2)
          LISP
        end
        it { is_expected.to eq true }
      end
    end

    describe '#>' do
      context 'aの方が大きい場合' do
        let(:str) do
          <<~LISP
            (> 2 1)
          LISP
        end
        it { is_expected.to eq true }
      end

      context 'aとbが一致している場合' do
        let(:str) do
          <<~LISP
            (> 1 1)
          LISP
        end
        it { is_expected.to eq nil }
      end

      context 'bの方が大きい場合' do
        let(:str) do
          <<~LISP
            (> 1 2)
          LISP
        end
        it { is_expected.to eq nil }
      end
    end

    describe '#<=' do
      context 'aの方が大きい場合' do
        let(:str) do
          <<~LISP
            (<= 2 1)
          LISP
        end
        it { is_expected.to eq nil }
      end

      context 'aとbが一致している場合' do
        let(:str) do
          <<~LISP
            (<= 1 1)
          LISP
        end
        it { is_expected.to eq true }
      end

      context 'bの方が大きい場合' do
        let(:str) do
          <<~LISP
            (<= 1 2)
          LISP
        end
        it { is_expected.to eq true }
      end
    end

    describe '#>=' do
      context 'aの方が大きい場合' do
        let(:str) do
          <<~LISP
            (>= 2 1)
          LISP
        end
        it { is_expected.to eq true }
      end

      context 'aとbが一致している場合' do
        let(:str) do
          <<~LISP
            (>= 1 1)
          LISP
        end
        it { is_expected.to eq true }
      end

      context 'bの方が大きい場合' do
        let(:str) do
          <<~LISP
            (>= 1 2)
          LISP
        end
        it { is_expected.to eq nil }
      end
    end

    describe '#if' do
      context '固定値の場合' do
        context 'trueの場合' do
          let(:str) do
            <<~LISP
              (if t 1 2)
            LISP
          end
          it { is_expected.to eq 1 }
        end

        context 'falseの場合' do
          let(:str) do
            <<~LISP
              (if nil 1 2)
            LISP
          end
          it { is_expected.to eq 2 }
        end

        context '数値の場合' do
          let(:str) do
            <<~LISP
              (if 1 1 2)
            LISP
          end
          it { is_expected.to eq 1 }
        end
      end

      context '式の場合' do
        context 'trueの場合' do
          let(:str) do
            <<~LISP
              (if (= 1 1) (+ 2 3) (* 2 3))
            LISP
          end
          it { is_expected.to eq 5 }
        end

        context 'falseの場合' do
          let(:str) do
            <<~LISP
              (if (= 1 2) (+ 2 3) (* 2 3))
            LISP
          end
          it { is_expected.to eq 6 }
        end

        context '数値の場合' do
          let(:str) do
            <<~LISP
              (if (+ 1 2) (+ 2 3) (* 2 3))
            LISP
          end
          it { is_expected.to eq 5 }
        end
      end
    end

    describe '#cond' do
      context '固定値の場合' do
        context '1つ目に一致した場合' do
          let(:str) do
            <<~LISP
              (cond (t 1) (t 2) (t 3))
            LISP
          end
          it { is_expected.to eq 1 }
        end

        context '2つ目に一致した場合' do
          let(:str) do
            <<~LISP
              (cond (nil 1) (t 2) (t 3))
            LISP
          end
          it { is_expected.to eq 2 }
        end

        context '3つ目に一致した場合' do
          let(:str) do
            <<~LISP
              (cond (nil 1) (nil 2) (t 3))
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context 'すべてに一致しない場合' do
          let(:str) do
            <<~LISP
              (cond (nil 1) (nil 2) (nil 3))
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '式の場合' do
        context '1つ目に一致した場合' do
          let(:str) do
            <<~LISP
              (cond ((= 1 1) (+ 1 1)) ((= 2 2) (+ 2 2)) ((= 3 3) (+ 3 3)))
            LISP
          end
          it { is_expected.to eq 2 }
        end

        context '2つ目に一致した場合' do
          let(:str) do
            <<~LISP
              (cond ((= 1 2) (+ 1 1)) ((= 2 2) (+ 2 2)) ((= 3 3) (+ 3 3)))
            LISP
          end
          it { is_expected.to eq 4 }
        end

        context '3つ目に一致した場合' do
          let(:str) do
            <<~LISP
              (cond ((= 1 2) (+ 1 1)) ((= 2 3) (+ 2 2)) ((= 3 3) (+ 3 3)))
            LISP
          end
          it { is_expected.to eq 6 }
        end

        context 'すべてに一致しない場合' do
          let(:str) do
            <<~LISP
              (cond ((= 1 2) (+ 1 1)) ((= 2 3) (+ 2 2)) ((= 3 4) (+ 3 3)))
            LISP
          end
          it { is_expected.to eq nil }
        end
      end
    end

    describe '#not' do
      context '固定値の場合' do
        context 'nilの場合' do
          let(:str) do
            <<~LISP
              (not nil)
            LISP
          end
          it { is_expected.to eq true }
        end

        context 'trueの場合' do
          let(:str) do
            <<~LISP
              (not t)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context '数値の場合' do
          let(:str) do
            <<~LISP
              (not 1)
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '式の場合' do
        context 'nilの場合' do
          let(:str) do
            <<~LISP
              (not (= 1 2))
            LISP
          end
          it { is_expected.to eq true }
        end

        context 'trueの場合' do
          let(:str) do
            <<~LISP
              (not (= 1 1))
            LISP
          end
          it { is_expected.to eq nil }
        end

        context '数値の場合' do
          let(:str) do
            <<~LISP
              (not (+ 1 2))
            LISP
          end
          it { is_expected.to eq nil }
        end
      end
    end

    describe '#and' do
      context '固定値の場合' do
        context 'すべてtrueの場合' do
          let(:str) do
            <<~LISP
              (and t t t)
            LISP
          end
          it { is_expected.to eq true }
        end

        context 'trueとfalseが混ざっている場合' do
          let(:str) do
            <<~LISP
              (and t nil t)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context 'すべてがfalseの場合' do
          let(:str) do
            <<~LISP
              (and nil nil nil)
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '式の場合' do
        context 'すべてtrueの場合' do
          let(:str) do
            <<~LISP
              (and (= 1 1) (= 2 2) (= 3 3))
            LISP
          end
          it { is_expected.to eq true }
        end

        context 'trueとfalseが混ざっている場合' do
          let(:str) do
            <<~LISP
              (and (= 1 1) (= 2 3) (= 3 3))
            LISP
          end
          it { is_expected.to eq nil }
        end

        context 'すべてがfalseの場合' do
          let(:str) do
            <<~LISP
              (and (= 1 2) (= 2 3) (= 3 4))
            LISP
          end
          it { is_expected.to eq nil }
        end
      end
    end

    describe '#or' do
      context '固定値の場合' do
        context 'すべてtrueの場合' do
          let(:str) do
            <<~LISP
              (or t t t)
            LISP
          end
          it { is_expected.to eq true }
        end

        context 'trueとfalseが混ざっている場合' do
          let(:str) do
            <<~LISP
              (or t nil t)
            LISP
          end
          it { is_expected.to eq true }
        end

        context 'すべてがfalseの場合' do
          let(:str) do
            <<~LISP
              (or nil nil nil)
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '式の場合' do
        context 'すべてtrueの場合' do
          let(:str) do
            <<~LISP
              (or (= 1 1) (= 2 2) (= 3 3))
            LISP
          end
          it { is_expected.to eq true }
        end

        context 'trueとfalseが混ざっている場合' do
          let(:str) do
            <<~LISP
              (or (= 1 1) (= 2 3) (= 3 3))
            LISP
          end
          it { is_expected.to eq true }
        end

        context 'すべてがfalseの場合' do
          let(:str) do
            <<~LISP
              (or (= 1 2) (= 2 3) (= 3 4))
            LISP
          end
          it { is_expected.to eq nil }
        end
      end
    end
  end
end
