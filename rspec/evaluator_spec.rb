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

      context 'スコープの確認' do
        context 'ローカル変数に固定値を代入 ＆ xで評価' do
          let(:str) do
            <<~LISP
              (setq x 0)
              (let ((x 1))
                (setq x 99))
              x
            LISP
          end
          it { is_expected.to eq 0 }
        end

        context 'ローカル変数に式を代入 ＆ xで評価' do
          let(:str) do
            <<~LISP
              (setq x 0)
              (let ((x 1))
                (setq x (+ 2 3)))
              x
            LISP
          end
          it { is_expected.to eq 0 }
        end

        context 'ローカル変数に代入 ＆ symbol-valueで評価' do
          let(:str) do
            <<~LISP
              (setq x 0)
              (let ((x 1))
                (setq x 99))
              (symbol-value 'x)
            LISP
          end
          it { is_expected.to eq 0 }
        end

        context 'ローカル変数に代入 ＆ ローカル変数で計算' do
          let(:str) do
            <<~LISP
              (setq x 0)
              (let ((x 1))
                (setq x 99)
                (+ x 1))
            LISP
          end
          it { is_expected.to eq 100 }
        end

        context 'ローカル変数に存在しない変数をsetqする ＆ yで参照する' do
          let(:str) do
            <<~LISP
              (setq x 0)
              (let ((x 1))
                (setq y 99))
              y
            LISP
          end
          it { is_expected.to eq 99 } # グローバル変数yが作成される
        end

        context 'ローカル変数に存在しない変数をsetqする ＆ symbol-valueで参照する' do
          let(:str) do
            <<~LISP
              (setq x 0)
              (let ((x 1)) (setq y 99))
              (symbol-value `y)
            LISP
          end
          it { is_expected.to eq 99 } # グローバル変数yが作成される
        end
      end
    end

    describe '#setf' do
      context '変数代入' do
        context 'setqの場合' do
          let(:str) do
            <<~LISP
              (setq x 10)
              x
            LISP
          end
          it { is_expected.to eq 10}
        end

        context 'setfの場合' do
          let(:str) do
            <<~LISP
              (setf x 10)
              x
            LISP
          end
          it { is_expected.to eq 10}
        end
      end

      # TODO: 未実装
      xcontext 'listのcarに代入' do
        context 'setqの場合' do
          let(:str) do
            <<~LISP
              (setq my-cons (cons 1 2))
              (setq (car my-cons) 42)
            LISP
          end
          it { expect { subject }.to raise_error }
        end

        context 'setfの場合' do
          let(:str) do
            <<~LISP
              (setq my-cons (cons 1 2))
              (setf (car my-cons) 42)
              my-cons
            LISP
          end
          it do
            cons = subject
            expect(cons).to be_instance_of Rubi::Cons
            expect(cons.car).to eq 42
            expect(cons.cdr).to eq 2
          end
        end
      end

      # TODO: 未実装
      xcontext 'listの要素に代入' do
        context 'setqの場合' do
          let(:str) do
            <<~LISP
              (setq arr (make-array 3))
              (setq (aref arr 0) 99)
            LISP
          end
          it { expect { subject }.to raise_error }
        end

        context 'setfの場合' do
          let(:str) do
            <<~LISP
              (setq arr (make-array 3))
              (setf (aref arr 0) 99)
              arr
            LISP
          end
          # it {}
        end
      end

      # TODO: 未実装
      xcontext '' do
        # (setf (gethash 'foo table) 123)  ; ハッシュテーブルのキーに代入
        # (setf (nth 1 my-list) 'apple)    ; リストの2番目の要素に代入
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
            expect(proc.call(proc_params: [], lexical_hash: {})).to eq 3
          end
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
            expect(proc.call(proc_params: [3], lexical_hash: {})).to eq 5
          end
        end
      end
    end

    describe "#lambdaを実行 / function.instance_of?(Array)" do
      context '引数なし' do
        context '関数定義＆実行①' do
          let(:str) do
            <<~LISP
              ((lambda () (+ 1 2)))
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context '関数定義＆実行②' do
          let(:str) do
            <<~LISP
              ((lambda () (+ 2 3)))
            LISP
          end
          it { is_expected.to eq 5 }
        end
      end

      context '引数あり' do
        context '関数定義＆実行①' do
          let(:str) do
            <<~LISP
              ((lambda (x) (+ x 2)) 4)
            LISP
          end
          it { is_expected.to eq 6 }
        end

        context '関数定義＆実行②' do
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

      context '関数を定義して実行する(double)' do
        let(:str) do
          <<~LISP
            (defun double (x) (* x 2))
            (double 1)
          LISP
        end
        it { is_expected.to eq 2 }
      end

      context '関数を定義して実行する(square)' do
        let(:str) do
          <<~LISP
            (defun square (x) (* x x))
            (square 3)
          LISP
        end
        it { is_expected.to eq 9 }
      end

      context '関数を2回実行する(double)' do
        let(:str) do
          <<~LISP
            (defun double (x) (* x 2))
            (double 1)
            (double 1)
          LISP
        end
        it { is_expected.to eq 2 }
      end

      context '関数名を評価しても、参照できない' do
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
          expect(proc.call(proc_params: [3], lexical_hash: {})).to eq 6
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
          expect(proc.call(proc_params: [3], lexical_hash: {})).to eq 6
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

    describe "#function / #'(糖衣構文)" do
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
          expect(proc.call(proc_params: [3], lexical_hash: {})).to eq 6
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
          expect(proc.call(proc_params: [3], lexical_hash: {})).to eq 6
        end
      end

      context "#'関数オブジェクト" do
        context "#'(lambda (x) (* x 2))" do
          let(:str) do
            <<~LISP
              #'(lambda (x) (* x 2))
            LISP
          end
          it { is_expected.to be_instance_of Proc }
        end
      end

      context '組み込み関数の場合' do
        %w(+ - * /).each do |func|
          context "#'#{func}の場合" do
            let(:str) do
              <<~LISP
                #'#{func}
              LISP
            end

            it { is_expected.to be_instance_of(Proc) }
          end
        end

        # %w(car cdr apply).each do |func|
        %w(list cons null atom funcall = /= < > <= >= not).each do |func|
          xcontext "#'#{func}の場合" do
            let(:str) do
              <<~LISP
                #'#{func}
              LISP
            end

            it { is_expected.to be_instance_of(Proc) }
          end
        end
      end
    end

    describe '#symbol-function' do
      # TODO: functionとの違いがよく分かってない。参照できるスコープの違いなどがあるのか？
      let(:str) do
        <<~LISP
          (defun double (x) (* x 2))
          (symbol-function 'double)
        LISP
      end
      it { is_expected.to be_instance_of(Proc) }
    end

    describe '#symbol-value' do
      context "グローバル変数が取得できること" do
        let(:str) do
          <<~LISP
            (setq double 2)
            (symbol-value 'double)
          LISP
        end
        it { is_expected.to eq 2 }
      end

      context 'スコープの確認' do
        context 'グローバル変数が取得できること → ローカル変数に代入 ＆ symbol-valueで評価' do
          let(:str) do
            <<~LISP
              (setq x 0)
              (let ((x 1))
                (setq x 99))
              (symbol-value 'x)
            LISP
          end
          it { is_expected.to eq 0 }
        end

        context 'グローバル変数が取得できること → ローカル変数に存在しない変数をsetqする ＆ symbol-valueで参照する' do
          let(:str) do
            <<~LISP
              (setq x 0)
              (let ((x 1)) (setq y 99))
              (symbol-value `y)
            LISP
          end
          it { is_expected.to eq 99 } # グローバル変数yが作成される
        end
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

    describe '#append' do
      context 'list + list' do
        let(:str) do
          <<~LISP
            (append '(1 2) '(3 4))
          LISP
        end
        it { is_expected.to eq [1, 2, 3, 4] }
      end

      context 'list + list + list' do
        let(:str) do
          <<~LISP
            (append '(1 2) '(3 4) '(5 6))
          LISP
        end
        it { is_expected.to eq [1, 2, 3, 4, 5, 6] }
      end

      context 'list + atom' do
        let(:str) do
          <<~LISP
            (append '(1 2) 3)
          LISP
        end
        it do
          cons = subject
          expect(cons).to be_instance_of Rubi::Cons
          expect(cons.car).to eq [1, 2]
          expect(cons.cdr).to eq 3
        end
      end

      context 'list + list + atom' do
        let(:str) do
          <<~LISP
            (append '(1 2) '(3 4) 5)
          LISP
        end
        it do
          cons = subject
          expect(cons).to be_instance_of Rubi::Cons
          expect(cons.car).to eq [1, 2, 3, 4]
          expect(cons.cdr).to eq 5
        end
      end

      context 'list + atom + atom' do
        let(:str) do
          <<~LISP
            append '(1 2) 3 4)
          LISP
        end
        it { expect { subject }.to raise_error }
      end

      context 'list + nil' do
        let(:str) do
          <<~LISP
            (append '(1 2) nil)
          LISP
        end
        it { is_expected.to eq [1, 2] }
      end

      context 'nil + list' do
        let(:str) do
          <<~LISP
            (append nil '(1 2))
          LISP
        end
        it { is_expected.to eq [1, 2] }
      end

      context 'nil + nil' do
        let(:str) do
          <<~LISP
            (append nil nil)
          LISP
        end
        it { is_expected.to eq nil }
      end

      context 'atom + list' do
        let(:str) do
          <<~LISP
            (append 1 '(2 3))
          LISP
        end
        it { expect { subject }.to raise_error }
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
              (atom 'a)
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

        context '関数オブジェクトの場合' do
          let(:str) do
            <<~LISP
              (atom #'+)
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
        context 'ダブルクォートの場合' do
          let(:str) do
            <<~LISP
              "あいうえお"
            LISP
          end
          it { is_expected.to eq "あいうえお" }
        end

        context 'ダブルクォートの空文字の場合' do
          let(:str) do
            <<~LISP
              ""
            LISP
          end
          it { is_expected.to eq "" }
        end

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

    describe '#mod' do
      context '(mod 0 2)' do
        let(:str) { "(mod 0 2)" }
        it { is_expected.to eq 0 }
      end

      context '(mod 1 2)' do
        let(:str) { "(mod 1 2)" }
        it { is_expected.to eq 1 }
      end

      context '(mod 2 2)' do
        let(:str) { "(mod 2 2)" }
        it { is_expected.to eq 0 }
      end
    end

    describe '#eq (オブジェクトが一致しているか。ポインタが一致しているか)' do
      context '同じ値の場合' do
        context "整数と整数の場合" do
          let(:str) do
            <<~LISP
              (eq 1 1)
            LISP
          end
          it { is_expected.to eq true }
        end

        context "小数と小数の場合" do
          let(:str) do
            <<~LISP
              (eq 1.1 1.1)
            LISP
          end
          it { is_expected.to eq true } # TODO: Common Lispはnilらしい
        end

        context '整数と小数の場合' do
          let(:str) do
            <<~LISP
              (eq 1 1.0)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "シンボルとシンボル" do
          let(:str) do
            <<~LISP
              (eq 'a 'a)
            LISP
          end
          it { is_expected.to eq true }
        end

        context "文字列と文字列" do
          let(:str) do
            <<~LISP
              (eq "あ" "あ")
            LISP
          end
          it { is_expected.to eq nil } # 文字列だけ 同じ文字列でも、別オブジェクト
        end

        context "配列と配列" do
          let(:str) do
            <<~LISP
              (eq '(1 2) '(1 2))
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '違う値の場合' do
        context "数値と数値" do
          let(:str) do
            <<~LISP
              (eq 1 2)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "シンボルとシンボル" do
          let(:str) do
            <<~LISP
              (eq 'a 'b)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "文字列と文字列" do
          let(:str) do
            <<~LISP
              (eq "あ" "い")
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '変数の場合' do
        context '変数と数値の場合' do
          let(:str) do
            <<~LISP
              (setq x 1)
              (eq x 1)
            LISP
          end
          it { is_expected.to eq true }
        end

        context '変数とシンボルの場合' do
          let(:str) do
            <<~LISP
              (setq x 'a)
              (eq x 'a)
            LISP
          end
          it { is_expected.to eq true }
        end

        context '変数と文字列の場合' do
          let(:str) do
            <<~LISP
              (setq x "あ")
              (eq x "あ")
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '変数と変数の場合' do
        context "文字列と文字列" do
          let(:str) do
            <<~LISP
              (setq x "あ")
              (eq x x)
            LISP
          end
          it { is_expected.to eq true }
        end
      end

      context "(eq #'double (car (list #'double)))" do
        let(:str) do
          <<~LISP
            (defun double (x) (* x 2))
            (eq #'double (car (list #'double)))
          LISP
        end
        it { is_expected.to eq true }
      end
    end

    describe '#eql (？？？？)' do
      context '同じ値の場合' do
        context "整数と整数の場合" do
          let(:str) do
            <<~LISP
              (eql 1 1)
            LISP
          end
          it { is_expected.to eq true }
        end

        context "小数と小数の場合" do
          let(:str) do
            <<~LISP
              (eql 1.1 1.1)
            LISP
          end
          it { is_expected.to eq true }
        end

        context '整数と小数の場合' do
          let(:str) do
            <<~LISP
              (eql 1 1.0)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "シンボルとシンボル" do
          let(:str) do
            <<~LISP
              (eql 'a 'a)
            LISP
          end
          it { is_expected.to eq true }
        end

        context "文字列と文字列" do
          let(:str) do
            <<~LISP
              (eql "あ" "あ")
            LISP
          end
          it { is_expected.to eq nil } # 文字列だけ 同じ文字列でも、別オブジェクト
        end

        context "配列と配列" do
          let(:str) do
            <<~LISP
              (eql '(1 2) '(1 2))
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '違う値の場合' do
        context "数値と数値" do
          let(:str) do
            <<~LISP
              (eql 1 2)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "シンボルとシンボル" do
          let(:str) do
            <<~LISP
              (eql 'a 'b)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "文字列と文字列" do
          let(:str) do
            <<~LISP
              (eql "あ" "い")
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '変数の場合' do
        context '変数と数値の場合' do
          let(:str) do
            <<~LISP
              (setq x 1)
              (eql x 1)
            LISP
          end
          it { is_expected.to eq true }
        end

        context '変数とシンボルの場合' do
          let(:str) do
            <<~LISP
              (setq x 'a)
              (eql x 'a)
            LISP
          end
          it { is_expected.to eq true }
        end

        context '変数と文字列の場合' do
          let(:str) do
            <<~LISP
              (setq x "あ")
              (eql x "あ")
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '変数と変数の場合' do
        context "文字列と文字列" do
          let(:str) do
            <<~LISP
              (setq x "あ")
              (eql x x)
            LISP
          end
          it { is_expected.to eq true }
        end
      end
    end

    describe '#equal (値が一致しているか)' do
      context '同じ値の場合' do
        context "整数と整数の場合" do
          let(:str) do
            <<~LISP
              (equal 1 1)
            LISP
          end
          it { is_expected.to eq true }
        end

        context "小数と小数の場合" do
          let(:str) do
            <<~LISP
              (equal 1.1 1.1)
            LISP
          end
          it { is_expected.to eq true }
        end

        context '整数と小数の場合' do
          let(:str) do
            <<~LISP
              (equal 1 1.0)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "シンボルとシンボル" do
          let(:str) do
            <<~LISP
              (equal 'a 'a)
            LISP
          end
          it { is_expected.to eq true }
        end

        context "文字列と文字列" do
          let(:str) do
            <<~LISP
              (equal "あ" "あ")
            LISP
          end
          it { is_expected.to eq true } # 文字列だけ 同じ文字列でも、別オブジェクト
        end

        context "配列と配列" do
          let(:str) do
            <<~LISP
              (equal '(1 2) '(1 2))
            LISP
          end
          it { is_expected.to eq true }
        end
      end

      context '違う値の場合' do
        context "数値と数値" do
          let(:str) do
            <<~LISP
              (equal 1 2)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "シンボルとシンボル" do
          let(:str) do
            <<~LISP
              (equal 'a 'b)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "文字列と文字列" do
          let(:str) do
            <<~LISP
              (equal "あ" "い")
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context '変数の場合' do
        context '変数と数値の場合' do
          let(:str) do
            <<~LISP
              (setq x 1)
              (equal x 1)
            LISP
          end
          it { is_expected.to eq true }
        end

        context '変数とシンボルの場合' do
          let(:str) do
            <<~LISP
              (setq x 'a)
              (equal x 'a)
            LISP
          end
          it { is_expected.to eq true }
        end

        context '変数と文字列の場合' do
          let(:str) do
            <<~LISP
              (setq x "あ")
              (equal x "あ")
            LISP
          end
          it { is_expected.to eq true }
        end
      end

      context '変数と変数の場合' do
        context "文字列と文字列" do
          let(:str) do
            <<~LISP
              (setq x "あ")
              (equal x x)
            LISP
          end
          it { is_expected.to eq true }
        end
      end
    end

    describe '#=' do
      context '式同士' do
        context '一致する場合' do
          let(:str) do
            <<~LISP
              (= (+ 3 5) (* 2 4))
            LISP
          end
          it { is_expected.to eq true }
        end

        context '一致しない場合' do
          let(:str) do
            <<~LISP
              (= (+ 3 5) (* 2 5))
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

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

    describe '#progn' do
      context 'prognの戻り値を確認' do
        let(:str) do
          <<~LISP
            (setq x 0)
            (progn
              (setq x (+ 1 x))
              (setq x (+ 2 x))
              (setq x (+ 3 x)))
          LISP
        end
        it { is_expected.to eq 6 }
      end

      context 'prognの実行結果を確認' do
        let(:str) do
          <<~LISP
            (setq x 0)
            (progn
              (setq x (+ 1 x))
              (setq x (+ 2 x))
              (setq x (+ 3 x)))
            x
          LISP
        end
        it { is_expected.to eq 6 }
      end
    end

    describe '#apply' do
      context '定義した関数の場合' do
        let(:str) do
          <<~LISP
            (defun add (x y z) (+ x y z))
            (apply #'add '(1 2 3))
          LISP
        end
        it { is_expected.to eq 6 }
      end

      context '組み込み関数の場合' do
        let(:str) do
          <<~LISP
            (apply #'+ '(1 2 3))
          LISP
        end
        it { is_expected.to eq 6 }
      end
    end

    describe '#evenp' do
      context '(evenp 0)' do
        let(:str) { '(evenp 0)' }
        it { is_expected.to eq true }
      end

      context '(evenp 1)' do
        let(:str) { '(evenp 1)' }
        it { is_expected.to eq nil }
      end

      context '(evenp 2)' do
        let(:str) { '(evenp 2)' }
        it { is_expected.to eq true }
      end

      context '(evenp 3)' do
        let(:str) { '(evenp 3)' }
        it { is_expected.to eq nil }
      end
    end

    describe '#1+' do
      context '数値の場合' do
        let(:str) do
          <<~LISP
            (1+ 2)
          LISP
        end
        it { is_expected.to eq 3 }
      end

      context '変数の場合' do
        let(:str) do
          <<~LISP
            (setq x 3)
            (1+ x)
          LISP
        end
        it { is_expected.to eq 4 }
      end

      context '式の場合' do
        let(:str) do
          <<~LISP
            (1+ (+ 1 2))
          LISP
        end
        it { is_expected.to eq 4 }
      end
    end

    describe '#remove-if' do
      context '最終的な確認' do
        let(:str) { "(remove-if #'evenp '(1 2 3 4 5))" }
        it { is_expected.to eq [1, 3, 5] }
      end
      context 'carの確認' do
        let(:str) { "(car '(1 2 3 4 5))" }
        it { is_expected.to eq 1 }
      end
      context 'cdrの確認' do
        let(:str) { "(cdr '(1 2 3 4 5))" }
        it { is_expected.to eq [2, 3, 4, 5] }
      end
      context 'consの確認' do
        let(:str) do
          <<~LISP
            (setq lst '(1 2 3 4 5))
            (cons (car lst) (cdr lst))
          LISP
        end
        it { is_expected.to eq [1, 2, 3, 4, 5] }
      end
      context 'ifの確認①' do
        let(:str) do
          <<~LISP
            (setq lst '(1 2 3 4 5))
            (if (evenp (car lst))
              (car lst)
              (cdr lst)
            )
          LISP
        end
        it { is_expected.to eq [2, 3, 4, 5] }
      end
      context 'ifの確認②' do
        let(:str) do
          <<~LISP
            (setq lst '(2 3 4 5))
            (if (evenp (car lst))
              (car lst)
              (cdr lst)
            )
          LISP
        end
        it { is_expected.to eq 2 }
      end
      context 'ifの二重の確認①' do
        let(:str) do
          <<~LISP
            (setq lst '(1 2 3 4 5))
            (if (null lst)
              nil
              (if (evenp (car lst))
                (car lst)
                (cdr lst)
              )
            )
          LISP
        end
        it { is_expected.to eq [2, 3, 4, 5] }
      end
      context 'ifの二重の確認②' do
        let(:str) do
          <<~LISP
            (setq lst '(2 3 4 5))
            (if (null lst)
              nil
              (if (evenp (car lst))
                (car lst)
                (cdr lst)
              )
            )
          LISP
        end
        it { is_expected.to eq 2 }
      end

      # TODO: 再帰
    end
  end

  describe 'On Lispのサンプルコード' do
    subject { ast.map { |code| puts "-----------"; puts "lisp: #{code}"; evaluator.eval(code, {}, 0) }.last }

    let(:evaluator) { Rubi::Evaluator.new(debug: true) }
    let(:ast) do
      parser = Rubi::Parser.new
      ast = parser.parse(tokens)
      parser.expand_syntactic_sugar(ast)
    end
    let(:tokens) { Rubi::Tokenizer.new.split_tokens(str) }

    context '1. 拡張可能なプログラミング言語: https://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/extensibleLanguage.html' do
      # Common Lispで動くコードはなかった
    end

    context '2. 関数: https://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/functions.html' do
      context '関数の定義' do
        context '(defun double (x) (* x 2))' do
          let(:str) do
            <<~LISP
              (defun double (x) (* x 2))
            LISP
          end
          it { is_expected.to eq :double }
        end

        context '(double 1)' do
          let(:str) do
            <<~LISP
              (defun double (x) (* x 2))
              (double 1)
            LISP
          end
          it { is_expected.to eq 2 }
        end

        context "#'double" do
          let(:str) do
            <<~LISP
              (defun double (x) (* x 2))
              #'double
            LISP
          end
          it { is_expected.to be_instance_of Proc }
        end

        context "(eq #'double (car (list #'double)))" do
          let(:str) do
            <<~LISP
              (defun double (x) (* x 2))
              (eq #'double (car (list #'double)))
            LISP
          end
          it { is_expected.to eq true }
        end

        context "(lambda (x) (* x 2))" do
          let(:str) do
            <<~LISP
              (lambda (x) (* x 2))
            LISP
          end
          it { is_expected.to be_instance_of Proc }
        end

        context "#'(lambda (x) (* x 2))" do
          let(:str) do
            <<~LISP
              #'(lambda (x) (* x 2))
            LISP
          end
          it { is_expected.to be_instance_of Proc }
        end

        context "(double 3)" do
          let(:str) do
            <<~LISP
              (defun double (x) (* x 2))
              (double 3)
            LISP
          end
          it { is_expected.to eq 6 }
        end

        context "((lambda (x) (* x 2)) 3)" do
          let(:str) do
            <<~LISP
              ((lambda (x) (* x 2)) 3)
            LISP
          end
          it { is_expected.to eq 6 }
        end

        context "(double double)" do
          let(:str) do
            <<~LISP
              (defun double (x) (* x 2))
              (setq double 2)
              (double double)
            LISP
          end
          it { is_expected.to eq 4 }
        end

        context "(symbol-value 'double)" do
          let(:str) do
            <<~LISP
              (setq double 2)
              (symbol-value 'double)
            LISP
          end
          it { is_expected.to eq 2 }
        end

        context "(symbol-function 'double)" do
          let(:str) do
            <<~LISP
              (defun double (x) (* x 2))
              (setq double 2)
              (symbol-function 'double)
            LISP
          end
          it { is_expected.to be_instance_of Proc }
        end

        context "(setq x #'append)" do
          let(:str) do
            <<~LISP
              (setq x #'append)
            LISP
          end
          it { is_expected.to be_instance_of Proc }
        end

        context "(eq (symbol-value 'x) (symbol-function 'append))" do
          let(:str) do
            <<~LISP
              (setq x #'append)
              (eq (symbol-value 'x) (symbol-function 'append))
            LISP
          end
          it { is_expected.to eq true }
        end

        context "(setf (symbol-function 'double) #'(lambda (x) (* x 2)))" do
          let(:str) do
            <<~LISP
              (setf (symbol-function 'double)
                #'(lambda (x) (* x 2)))
              (double 2)
            LISP
          end
          it { is_expected.to eq 4 }
        end
      end

      context '関数を引数にする' do
        context "(+ 1 2)" do
          let(:str) do
            <<~LISP
              (+ 1 2)
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context "(apply #'+ '(1 2))" do
          let(:str) do
            <<~LISP
              (apply #'+ '(1 2))
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context "(apply (symbol-function '+) '(1 2))" do
          let(:str) do
            <<~LISP
              (apply (symbol-function '+) '(1 2))
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context "(apply #'(lambda (x y) (+ x y)) '(1 2))" do
          let(:str) do
            <<~LISP
              (apply #'(lambda (x y) (+ x y)) '(1 2))
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context "(apply #'+ 1 '(2))" do
          let(:str) do
            <<~LISP
              (apply #'+ 1 '(2))
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context "(funcall #'+ 1 2)" do
          let(:str) do
            <<~LISP
              (funcall #'+ 1 2)
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context "(mapcar #'(lambda (x) (+ x 10)) '(1 2 3))" do
          let(:str) do
            <<~LISP
              (mapcar #'(lambda (x) (+ x 10))
                '(1 2 3))
            LISP
          end
          it { is_expected.to eq [11, 12, 13] }
        end

        context "(mapcar #'+ '(1 2 3) '(10 100 1000))" do
          let(:str) do
            <<~LISP
              (mapcar #'+
                '(1 2 3)
                '(10 100 1000))
            LISP
          end
          it { is_expected.to eq [11, 102, 1003] }
        end

        context "(sort '(1 4 2 5 6 7 3) #'<)" do
          let(:str) do
            <<~LISP
              (sort '(1 4 2 5 6 7 3) #'<)
            LISP
          end
          it { is_expected.to eq [1, 2, 3, 4, 5, 6, 7] }
        end

        context "(remove-if #'evenp '(1 2 3 4 5 6 7))" do
          let(:str) do
            # TODO:
            # (defun remove-if (func lst) (funcall func (car lst)))
            <<~LISP
              (remove-if #'evenp '(1 2 3 4 5 6 7))
            LISP
          end
          it { is_expected.to eq [1, 3, 5, 7] }
        end

        context "(defun our-remove-if (fn lst)" do
          let(:str) do
            <<~LISP
              (defun our-remove-if (fn lst)
                (if (null lst)
                    nil
                    (if (funcall fn (car lst))
                        (our-remove-if fn (cdr lst))
                        (cons (car lst) (our-remove-if fn (cdr lst))))))
            LISP
          end
          it { is_expected.to eq :"our-remove-if" }
        end
      end

      context '属性としての関数' do
        context "(defun behave (animal)" do
          let(:str) do
            <<~LISP
              (defun behave (animal)
                (case animal
                  (dog (wag-tail)
                    (bark))
                  (rat (scurry)
                    (squeak))
                  (cat (rub-legs)
                    (scratch-carpet))))
            LISP
          end
          it { is_expected.to eq :behave }
        end

        context "(defun behave (animal) (funcall (get animal 'behavior)))" do
          let(:str) do
            <<~LISP
              (defun behave (animal)
                (funcall (get animal 'behavior)))
            LISP
          end
          it { is_expected.to eq :behave }
        end

        context "(setf (get 'dog 'behavior)" do
          let(:str) do
            <<~LISP
              (setf (get 'dog 'behavior)
                #'(lambda ()
                    (wag-tail)
                      (bark)))
            LISP
          end
          it { is_expected.to be_instance_of Proc }
        end
      end

      # schemeの仕様のため、対応しない
      xcontext 'スコープ' do
        context "(let ((y 7))" do
          let(:str) do
            <<~LISP
              (let ((y 7))
                (defun scope-test (x)
                  (list x y)))
            LISP
          end
          it { is_expected.to eq :"scope-test" }
        end

        context "(let ((y 7))" do
          let(:str) do
            <<~LISP
              (let ((y 7))
                (defun scope-test (x)
                  (list x y)))
              (let ((y 5))
                (scope-test 3))
                  (3 5)
            LISP
          end
          it { is_expected.to eq :"scope-test" }
        end
      end

      context 'クロージャ' do
        context "(defun list+ (lst n)" do
          let(:str) do
            <<~LISP
              (defun list+ (lst n)
                (mapcar #'(lambda (x) (+ x n))
                        lst))
            LISP
          end
          it { is_expected.to eq :"list+" }
        end

        context "(defun list+ (lst n)" do
          let(:str) do
            <<~LISP
              (defun list+ (lst n)
                (mapcar #'(lambda (x) (+ x n))
                        lst))
              (list+ '(1 2 3) 10)
            LISP
          end
          it { is_expected.to eq [11, 12, 13] }
        end

        context "(let ((counter 0))" do
          let(:str) do
            <<~LISP
              (let ((counter 0))
                (defun new-id () (incf counter))
                (defun reset-id () (setq counter 0)))
            LISP
          end
          it { is_expected.to eq :"reset-id" }
        end

        context "(defun make-adder (n)" do
          let(:str) do
            <<~LISP
              (defun make-adder (n)
                #'(lambda (x) (+ x n)))
            LISP
          end
          it { is_expected.to eq :"make-adder" }
        end

        context "(setq add2 (make-adder 2) add10 (make-adder 10))" do
          let(:str) do
            <<~LISP
              (defun make-adder (n)
                #'(lambda (x) (+ x n)))
              (setq add2 (make-adder 2)
                add10 (make-adder 10))
            LISP
          end
          it { is_expected.to be_instance_of Proc }
        end

        context "(funcall add2 5)" do
          let(:str) do
            <<~LISP
              (defun make-adder (n)
                #'(lambda (x) (+ x n)))
              (setq add2 (make-adder 2)
                add10 (make-adder 10))
              (funcall add2 5)
            LISP
          end
          it { is_expected.to eq 7 }
        end

        context "(funcall add10 3)" do
          let(:str) do
            <<~LISP
              (defun make-adder (n)
                #'(lambda (x) (+ x n)))
              (setq add2 (make-adder 2)
                add10 (make-adder 10))
              (funcall add10 3)
            LISP
          end
          it { is_expected.to eq 13 }
        end

        context "(defun make-adderb (n)" do
          let(:str) do
            <<~LISP
              (defun make-adderb (n)
                #'(lambda (x &optional change)
                    (if change
                        (setq n x)
                        (+ x n))))
            LISP
          end
          it { is_expected.to eq :"make-adderb" }
        end

        context "(setq addx (make-adderb 1))" do
          let(:str) do
            <<~LISP
              (defun make-adderb (n)
                #'(lambda (x &optional change)
                    (if change
                        (setq n x)
                        (+ x n))))
              (setq addx (make-adderb 1))
            LISP
          end
          it { is_expected.to be_instance_of Proc }
        end

        context "(funcall addx 3)" do
          let(:str) do
            <<~LISP
              (defun make-adderb (n)
                #'(lambda (x &optional change)
                    (if change
                        (setq n x)
                        (+ x n))))
              (setq addx (make-adderb 1))
              (funcall addx 3)
            LISP
          end
          it { is_expected.to eq 4 }
        end

        context "(funcall addx 100 t)" do
          let(:str) do
            <<~LISP
              (defun make-adderb (n)
                #'(lambda (x &optional change)
                    (if change
                        (setq n x)
                        (+ x n))))
              (setq addx (make-adderb 1))
              (funcall addx 3)
              (funcall addx 100 t)
            LISP
          end
          it { is_expected.to eq 100 }
        end

        context "(funcall addx 3)" do
          let(:str) do
            <<~LISP
              (defun make-adderb (n)
                #'(lambda (x &optional change)
                    (if change
                        (setq n x)
                        (+ x n))))
              (setq addx (make-adderb 1))
              (funcall addx 3)
              (funcall addx 100 t)
              (funcall addx 3)
            LISP
          end
          it { is_expected.to eq 103 }
        end

        context "(setq cities (make-dbms '((boston . us) (paris . france))))" do
          let(:str) do
            <<~LISP
              (defun make-dbms (db)
                (list
                  #'(lambda (key)
                      (cdr (assoc key db)))
                  #'(lambda (key val)
                      (push (cons key val) db)
                      key)
                  #'(lambda (key)
                      (setf db (delete key db :key #'car))
                      key)))
              (setq cities (make-dbms '((boston . us) (paris . france))))
            LISP
          end
          it { is_expected.to eq 999999 }
        end

        context "(funcall (car cities) 'boston)" do
          let(:str) do
            <<~LISP
              (defun make-dbms (db)
                (list
                  #'(lambda (key)
                      (cdr (assoc key db)))
                  #'(lambda (key val)
                      (push (cons key val) db)
                      key)
                  #'(lambda (key)
                      (setf db (delete key db :key #'car))
                      key)))
              (setq cities (make-dbms '((boston . us) (paris . france))))
              (funcall (car cities) 'boston)
            LISP
          end
          it { is_expected.to eq 999999 }
        end

        context "(funcall (second cities) 'london 'england)" do
          let(:str) do
            <<~LISP
              (defun make-dbms (db)
                (list
                  #'(lambda (key)
                      (cdr (assoc key db)))
                  #'(lambda (key val)
                      (push (cons key val) db)
                      key)
                  #'(lambda (key)
                      (setf db (delete key db :key #'car))
                      key)))
              (setq cities (make-dbms '((boston . us) (paris . france))))
              (funcall (car cities) 'boston)
              (funcall (second cities) 'london 'england)
            LISP
          end
          it { is_expected.to eq 999999 }
        end

        context "(funcall (car cities) 'london)" do
          let(:str) do
            <<~LISP
              (defun make-dbms (db)
                (list
                  #'(lambda (key)
                      (cdr (assoc key db)))
                  #'(lambda (key val)
                      (push (cons key val) db)
                      key)
                  #'(lambda (key)
                      (setf db (delete key db :key #'car))
                      key)))
              (setq cities (make-dbms '((boston . us) (paris . france))))
              (funcall (car cities) 'boston)
              (funcall (second cities) 'london 'england)
              (funcall (car cities) 'london)
            LISP
          end
          it { is_expected.to eq 999999 }
        end

        context "(funcall (car cities) 'london)" do
          let(:str) do
            <<~LISP
              (defun lookup (key db)
                (funcall (car db) key))
            LISP
          end
          it { is_expected.to eq 999999 }
        end
      end

      context 'ローカル関数' do
        context "(funcall (car cities) 'london)" do
          let(:str) do
            <<~LISP
              (mapcar #'(lambda (x) (+ 2 x))
                '(2 5 7 3))
            LISP
          end
          it { is_expected.to eq [4, 7, 9, 5] }
        end

        context "(mapcar #'copy-tree '((a b) (c d e)))" do
          let(:str) do
            <<~LISP
              (mapcar #'copy-tree '((a b) (c d e)))
            LISP
          end
          it { is_expected.to eq [4, 7, 9, 5] }
        end

        context "(defun list+ (lst n) (mapcar #'(lambda (x) (+ x n)) lst))" do
          let(:str) do
            <<~LISP
              (defun list+ (lst n)
                (mapcar #'(lambda (x) (+ x n))
                        lst))
            LISP
          end
          it { is_expected.to eq [4, 7, 9, 5] }
        end

        context "(labels ((inc (x) (1+ x))) (inc 3))" do
          let(:str) do
            <<~LISP
              (labels ((inc (x) (1+ x)))
                      (inc 3))
            LISP
          end
          it { is_expected.to eq [4, 7, 9, 5] }
        end

        context "(defun count-instances (obj lsts)" do
          let(:str) do
            <<~LISP
              (defun count-instances (obj lsts)
                (labels ((instances-in (lst)
                           (if (consp lst)
                               (+ (if (eq (car lst) obj) 1 0)
                                  (instances-in (cdr lst)))
                               0)))
                  (mapcar #'instances-in lsts)))
            LISP
          end
          it { is_expected.to eq [4, 7, 9, 5] }
        end

        context "(count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))" do
          let(:str) do
            <<~LISP
              (defun count-instances (obj lsts)
                (labels ((instances-in (lst)
                           (if (consp lst)
                               (+ (if (eq (car lst) obj) 1 0)
                                  (instances-in (cdr lst)))
                               0)))
                  (mapcar #'instances-in lsts)))
              (count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))
            LISP
          end
          it { is_expected.to eq [4, 7, 9, 5] }
        end
      end

      context '末尾再帰' do
        context "(defun our-length (lst)" do
          # TODO: 再帰すると壊れるっぽい気がする
          let(:str) do
            <<~LISP
              (defun our-length (lst)
                (if (null lst)
                    0
                    (1+ (our-length (cdr lst)))))
              (our-length '(1 2 3 4 5))
            LISP
          end
          it { is_expected.to eq 5 }
        end

        context "(defun our-find-if (fn lst)" do
          let(:str) do
            <<~LISP
              (defun our-find-if (fn lst)
                (if (funcall fn (car lst))
                    (car lst)
                    (our-find-if fn (cdr lst))))
            LISP
          end
          it { is_expected.to eq 5 }
        end

        context "(defun our-length (lst)" do
          let(:str) do
            <<~LISP
              (defun our-length (lst)
                (labels ((rec (lst acc)
                         (if (null lst)
                             acc
                             (rec (cdr lst) (1+ acc)))))
                  (rec lst 0)))
            LISP
          end
          it { is_expected.to eq 5 }
        end

        context "(proclaim '(optimize speed))" do
          let(:str) do
            <<~LISP
              (proclaim '(optimize speed))
            LISP
          end
          it { is_expected.to eq 5 }
        end

        context "(defun triangle (n)" do
          let(:str) do
            <<~LISP
              (defun triangle (n)
                (labels ((tri (c n)
                              (declare (type fixnum n c))
                              (if (zerop n)
                                  c
                                  (tri (the fixnum (+ n c))
                                       (the fixnum (- n 1))))))
                  (tri 0 n)))
            LISP
          end
          it { is_expected.to eq 5 }
        end
      end

      # コンパイルは未実装なので、スキップ
      context 'コンパイル' do
        context "(defun foo (x) (1+ x))" do
          let(:str) do
            <<~LISP
              (defun foo (x) (1+ x))
            LISP
          end
          it { is_expected.to eq :foo }
        end

        context "(compiled-function-p #'foo)" do
          let(:str) do
            <<~LISP
              (defun foo (x) (1+ x))
              (compiled-function-p #'foo)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "(compile 'foo)" do
          let(:str) do
            <<~LISP
              (defun foo (x) (1+ x))
              (compile 'foo)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "(compile 'foo)(compiled-function-p #'foo)" do
          let(:str) do
            <<~LISP
              (defun foo (x) (1+ x))
              (compile 'foo)
              (compiled-function-p #'foo)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "(compile nil '(lambda (x) (+ x 2)))" do
          let(:str) do
            <<~LISP
              (compile nil '(lambda (x) (+ x 2)))
            LISP
          end
          it { is_expected.to eq 99999999 }
        end

        # TODO: まだある
      end
    end

    context '3. 関数的プログラミング: https://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/functionalProgramming.html' do
      context '関数的デザイン' do
        context '(defun bad-reverse (lst)' do
          let(:str) do
            <<~LISP
              (defun bad-reverse (lst)
                (let* ((len (length lst))
                       (ilimit (truncate (/ len 2))))
                  (do ((i 0 (1+ i))
                       (j (1- len) (1- j)))
                      ((>= i ilimit))
                    (rotatef (nth i lst) (nth j lst)))))
                (bad-reverse '(1 2 3))
            LISP
          end
          it { is_expected.to eq [3, 2, 1] }
        end

        context '(defun bad-reverse (lst)' do
          let(:str) do
            <<~LISP
              (defun bad-reverse (lst)
                (let* ((len (length lst))
                       (ilimit (truncate (/ len 2))))
                  (do ((i 0 (1+ i))
                       (j (1- len) (1- j)))
                      ((>= i ilimit))
                    (rotatef (nth i lst) (nth j lst)))))

                (setq lst '(a b c))
                (bad-reverse lst)
            LISP
          end
          it { is_expected.to eq nil }
        end

        context '(defun bad-reverse (lst)' do
          let(:str) do
            <<~LISP
              (defun bad-reverse (lst)
                (let* ((len (length lst))
                       (ilimit (truncate (/ len 2))))
                  (do ((i 0 (1+ i))
                       (j (1- len) (1- j)))
                      ((>= i ilimit))
                    (rotatef (nth i lst) (nth j lst)))))

                (setq lst '(a b c))
                (bad-reverse lst)
                lst
            LISP
          end
          it { is_expected.to eq [:c, :b, :a] }
        end
        # TODO:
      end

      context '命令的プログラミングの裏返し' do
        xit 'TODO' do
        end
      end

      context '関数的インタフェイス' do
        xit 'TODO' do
        end
      end

      context 'インタラクティブ・プログラミング' do
        xit 'TODO' do
        end
      end
    end

    context '4. ユーティリティ関数: https://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/utilityFunctions.html' do
      xit 'TODO' do
      end
    end

    context '5. 返り値としての関数: https://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/returningFunctions.html' do
      xit 'TODO' do
      end
    end

    context '6. 表現としての関数: https://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/functionsAsRepresentation.html' do
      xit 'TODO' do
      end
    end

    context '7. マクロ: https://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/macros.html' do
      context 'マクロはどのように動作するか' do
        context '(defmacro nil! (var)' do
          context '定義' do
            let(:str) do
              <<~LISP
                (defmacro nil! (var)
                  (list 'setq var nil))
              LISP
            end
            it { is_expected.to eq :nil! }
          end

          context '定義＆実行' do
            let(:str) do
              <<~LISP
                (setq x 2)
                (defmacro nil! (var)
                  (list 'setq var nil))
                (nil! x)
                x
              LISP
            end
            it { is_expected.to eq nil }
          end
        end
      end

      context '逆クォート' do
        context "(setq a 1 b 2 c 3)" do
          let(:str) do
            <<~LISP
              (setq a 1 b 2 c 3)
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context "`(a ,b c)" do
          let(:str) do
            <<~LISP
              `(a ,b c)
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context "`(a (,b c))" do
          let(:str) do
            <<~LISP
              `(a (,b c))
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context "(defmacro nil! (var) `(setq ,var nil))" do
          let(:str) do
            <<~LISP
              (setq x 2)
              (defmacro nil! (var)
                `(setq ,var nil))
              (nil! x)
              x
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "(defmacro nif (expr pos zero neg)  バッククォート版" do
          let(:str) do
            <<~LISP
              (defmacro nif (expr pos zero neg)
                `(case (truncate (signum ,expr))
                   (1 ,pos)
                   (0 ,zero)
                   (-1 ,neg)))
  
              (mapcar #'(lambda (x)
                (nif x 'p 'z 'n))
                '(0 2.5 -8))
            LISP
          end
          it { is_expected.to eq nil }
        end

        context "(defmacro nif (expr pos zero neg) バッククォートなし版" do
          let(:str) do
            <<~LISP
              (defmacro nif (expr pos zero neg)
                (list 'case
                      (list 'truncate (list 'signum expr))
                      (list 1 pos)
                      (list 0 zero)
                      (list -1 neg)))
  
              (mapcar #'(lambda (x)
                (nif x 'p 'z 'n))
                '(0 2.5 -8))
            LISP
          end
          it { is_expected.to eq nil }
        end

        xit 'TODO: まだある' do
        end
      end

      xit 'TODO: まだある' do
      end
    end
  end
end
