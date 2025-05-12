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

    describe '#レキシカルスコープ系の確認' do
      context '#let*との比較' do
        context '#let -> let内で宣言した変数を参照する(aは未定義)' do
          let(:str) do
            <<~LISP
                (let ((a 2) ; aはlet内ではまだ未定義
                      (b (+ a 3))) ; ← a まだ未定義
                  (list a b))
              LISP
          end
          it { expect { subject }.to raise_error }
        end

        context '#let -> let内で宣言した変数を参照する(aを外部で定義)' do
          let(:str) do
            <<~LISP
                (setq a 10)
                (let ((a 2) ; ← このタイミングではaは未定義
                      (b (+ a 3)))  ; ← このタイミングではaは未定義。外の a（10）を参照して b = 13
                  (list a b))
              LISP
          end
          it { is_expected.to eq [2, 13] }
        end
      end

      context '#let -> letをネストして、1階層目の変数を評価' do
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

      context '#dotimes -> (length list)回ループする' do
        let(:str) do
          <<~LISP
            (setq x 0)
            (dotimes (n (length (list 1 2 3)))
              (setq x (+ 1 x)))
            x
          LISP
        end
        it { is_expected.to eq 3 }
      end
    end

    describe '#let' do
      context '#let*との比較' do
        context 'let内で宣言した変数を参照する(aは未定義)' do
          let(:str) do
            <<~LISP
              (let ((a 2) ; aはlet内ではまだ未定義
                    (b (+ a 3))) ; ← a まだ未定義
                (list a b))
            LISP
          end
          it { expect { subject }.to raise_error }
        end

        context 'let内で宣言した変数を参照する(aを外部で定義)' do
          let(:str) do
            <<~LISP
              (setq a 10)
              (let ((a 2)
                    (b (+ a 3)))  ; ← 外の a（10）を参照して b = 13
                (list a b))
            LISP
          end
          it { is_expected.to eq [2, 13] }
        end
      end

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

    describe '#let*' do
      context '#letとの比較' do
        context 'let*内で宣言した変数を参照する' do
          let(:str) do
            <<~LISP
              (let* ((a 2) ; a = 2で定義
                    (b (+ a 3))) ; ← a 定義済み
                (list a b))
            LISP
          end
          it { is_expected.to eq [2, 5] }
        end
      end

      context '変数１つ宣言' do
        let(:str) do
          <<~LISP
            (let*
              ((x 2))
              x)
          LISP
        end
        it { is_expected.to eq 2 }
      end

      context '式が２つ' do
        let(:str) do
          <<~LISP
            (let*
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
            (let*
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
            (let*
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
            (let*
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
            (let* ((x 1))
              (let* ((x 2))
                x))
          LISP
        end
        it { is_expected.to eq 2 }
      end

      context 'letをネストして、1階層目の変数を評価' do
        let(:str) do
          <<~LISP
            (let* ((x 1))
              (let* ((x 2))
                x)
              x)
          LISP
        end
        it { is_expected.to eq 1 }
      end
    end

    describe '#labels' do
      context 'スコープ内で、定義した関数を実行する' do
        let(:str) do
          <<~LISP
            (labels ((double (x) (* 2 x)))
                    (double 3))
          LISP
        end
        it { is_expected.to eq 6 }
      end

      context 'スコープ外で、定義した関数を実行する' do
        let(:str) do
          <<~LISP
            (labels ((double (x) (* 2 x)))
                    (double 3))
            (double 3) ; labelsの中でしかdoubleは有効でない
          LISP
        end
        it { expect { subject }.to raise_error }
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

      context '変数を複数定義する' do
        context '1つ目の変数を参照する' do
          let(:str) do
            <<~LISP
              (setq a 1 b 2 c 3)
              a
            LISP
          end
          it { is_expected.to eq 1 }
        end

        context '3つ目の変数を参照する' do
          let(:str) do
            <<~LISP
              (setq a 1 b 2 c 3)
              c
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context '戻り値を確認する' do
          let(:str) do
            <<~LISP
              (setq a 1 b 2 c 3)
            LISP
          end
          it { is_expected.to eq 3 }
        end
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

      context "(setq add2 (make-adder 2) add10 (make-adder 10))" do
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

      context "place: symbol-functionで関数定義する" do
        context '定義のみ' do
          let(:str) do
            <<~LISP
              (setf (symbol-function 'double)
                #'(lambda (x) (* x 2)))
            LISP
          end
          it { is_expected.to be_instance_of Proc }
        end

        context '定義＆実行' do
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

      context "place: getで変数定義する" do
        context '定義のみ' do
          let(:str) do
            <<~LISP
              (setf (get 'color 'shade) 'dark)
            LISP
          end
          it { is_expected.to eq :dark }
        end

        context '定義＆参照' do
          let(:str) do
            <<~LISP
              (setf (get 'color 'shade) 'dark)
              (get 'color 'shade) ; => 'dark
            LISP
          end
          it { is_expected.to eq :dark }
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
          it { raise }
        end
      end

      context 'gethashの場合' do
        let(:str) do
          <<~LISP
            (setf (gethash 'foo table) 123)  ; ハッシュテーブルのキーに代入
          LISP
        end
        it { raise }
      end

      xcontext 'nth' do
        let(:str) do
          <<~LISP
            (setf (nth 1 my-list) 'apple)    ; リストの2番目の要素に代入
          LISP
        end
        it { raise }
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
            expect(proc.call(proc_params: [], lexical_hash: {}, stack_count: 0, nest: "")).to eq 3
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
            expect(proc.call(proc_params: [3], lexical_hash: {}, stack_count: 0, nest: "")).to eq 5
          end
        end
      end

      context 'オプション引数(&optional)' do
        context '引数を渡してない場合は、nilになる' do
          context 'すべて引数を設定した場合' do
            let(:str) do
              <<~LISP
                (defun foo (a b &optional c d)
                  (list a b c d))
                
                (foo 2 3 4 5) ;=> a=2 b=3 c=4 d=5
              LISP
            end
            it { is_expected.to eq [2, 3, 4, 5] }
          end

          context '１つ省略した場合' do
            let(:str) do
              <<~LISP
                (defun foo (a b &optional c d)
                  (list a b c d))
                
                (foo 2 3 4) ;=> a=2 b=3 c=4 d=NIL
              LISP
            end
            it { is_expected.to eq [2, 3, 4, nil] }
          end

          context '２つ省略した場合' do
            let(:str) do
              <<~LISP
                (defun foo (a b &optional c d)
                  (list a b c d))
                
                (foo 2 3) ;=> a=2 b=3 c=nil d=nil
              LISP
            end
            it { is_expected.to eq [2, 3, nil, nil] }
          end
        end

        context 'デフォルト値を指定した場合' do
          context 'すべて引数を設定した場合' do
            let(:str) do
              <<~LISP
                (defun foo (a b &optional (c -1) (d -2))
                  (list a b c d))

                (foo 2 3 4 5) ;=> a=2 b=3 c=4 d=5
              LISP
            end
            it { is_expected.to eq [2, 3, 4, 5] }
          end

          context '１つ省略した場合' do
            let(:str) do
              <<~LISP
                (defun foo (a b &optional (c -1) (d -2))
                  (list a b c d))
                
                (foo 2 3 4) ;=> a=2 b=3 c=4 d=-2
              LISP
            end
            it { is_expected.to eq [2, 3, 4, -2] }
          end

          context '２つ省略した場合' do
            let(:str) do
              <<~LISP
                (defun foo (a b &optional (c -1) (d -2))
                  (list a b c d))
                
                (foo 2 3) ;=> a=2 b=3 c=-1 d=-2
              LISP
            end
            it { is_expected.to eq [2, 3, -1, -2] }
          end
        end
      end

      context '残り引数(&rest)' do
        let(:str) do
          <<~LISP
            (defun foo (a b &rest values)
            (format t "a=~a b=~a values=~a~%" a b values))
            
            (foo 2 3 4 5 6)    ;=> a=2 b=3 values=(4 5 6)
          LISP
        end
        it { is_expected.to eq 9 } # 最後の結果が返ってくる
      end

      context 'キーワード引数(&key)' do
        context 'デフォルト値を省略した場合は、nilになる' do
          let(:str) do
            <<~LISP
              (defun foo (&key a b c d)
                (format t "a=~d b=~d c=~d d=~d~%" a b c d))
              
              (foo :d 10 :c 20)   ;=> a=NIL b=NIL c=20 d=10
            LISP
          end
          it { is_expected.to eq 9 } # 最後の結果が返ってくる
        end

        context 'デフォルト値を指定した場合' do
          let(:str) do
            <<~LISP
              (defun foo (&key (a 0) (b 0) (c 0) (d 0))
                (format t "a=~d b=~d c=~d d=~d~%" a b c d))
              
              (foo :d 10 :c 20)   ;=> a=0 b=0 c=20 d=10
            LISP
          end
          it { is_expected.to eq 9 } # 最後の結果が返ってくる
        end
      end

      context '実行する式が複数ある場合' do
        context '最後の結果が返ってくる' do
          let(:str) do
            <<~LISP
              ((lambda (x) (+ 1 2) (* x 3)) 3)
            LISP
          end
          it { is_expected.to eq 9 } # 最後の結果が返ってくる
        end

        context '最初の式が実行されている' do
          let(:str) do
            <<~LISP
              ((lambda (x) (setq x 2) (* x 3)) 3)
            LISP
          end
          it { is_expected.to eq 6 } # 最初の式が実行されている
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

      context 'オプション引数(&optional)' do
        context '引数を渡してない場合は、nilになる' do
          context 'すべて引数を設定した場合' do
            let(:str) do
              <<~LISP
                (defun foo (a b &optional c d)
                  (list a b c d))
                
                (foo 2 3 4 5) ;=> a=2 b=3 c=4 d=5
              LISP
            end
            it { is_expected.to eq [2, 3, 4, 5] }
          end

          context '１つ省略した場合' do
            let(:str) do
              <<~LISP
                (defun foo (a b &optional c d)
                  (list a b c d))
                
                (foo 2 3 4) ;=> a=2 b=3 c=4 d=NIL
              LISP
            end
            it { is_expected.to eq [2, 3, 4, nil] }
          end

          context '２つ省略した場合' do
            let(:str) do
              <<~LISP
                (defun foo (a b &optional c d)
                  (list a b c d))
                
                (foo 2 3) ;=> a=2 b=3 c=nil d=nil
              LISP
            end
            it { is_expected.to eq [2, 3, nil, nil] }
          end
        end

        context 'デフォルト値を指定した場合' do
          context 'すべて引数を設定した場合' do
            let(:str) do
              <<~LISP
                (defun foo (a b &optional (c -1) (d -2))
                  (list a b c d))

                (foo 2 3 4 5) ;=> a=2 b=3 c=4 d=5
              LISP
            end
            it { is_expected.to eq [2, 3, 4, 5] }
          end

          context '１つ省略した場合' do
            let(:str) do
              <<~LISP
                (defun foo (a b &optional (c -1) (d -2))
                  (list a b c d))
                
                (foo 2 3 4) ;=> a=2 b=3 c=4 d=-2
              LISP
            end
            it { is_expected.to eq [2, 3, 4, -2] }
          end

          context '２つ省略した場合' do
            let(:str) do
              <<~LISP
                (defun foo (a b &optional (c -1) (d -2))
                  (list a b c d))
                
                (foo 2 3) ;=> a=2 b=3 c=-1 d=-2
              LISP
            end
            it { is_expected.to eq [2, 3, -1, -2] }
          end
        end
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
          expect(proc.call(proc_params: [3], lexical_hash: {}, stack_count: 0, nest: "")).to eq 6
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
          expect(proc.call(proc_params: [3], lexical_hash: {}, stack_count: 0, nest: "")).to eq 6
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
          expect(proc.call(proc_params: [3], lexical_hash: {}, stack_count: 0, nest: "")).to eq 6
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
          expect(proc.call(proc_params: [3], lexical_hash: {}, stack_count: 0, nest: "")).to eq 6
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

      context 'labelsの中で定義した関数を取り出す' do
        let(:str) do
          <<~LISP
              (labels ((instances-in () 2))
                #'instances-in)
            LISP
        end
        it { is_expected.to be_instance_of Proc }
      end

      context '組み込み関数の場合' do
        %w(+ - * / < > <= >=).each do |func|
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
        %w(list cons null atom funcall = /= not).each do |func|
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

    describe '#nth' do
      context 'index:0の場合' do
        let(:str) { "(nth 0 '(a b c))" }
        it { is_expected.to eq :a }
      end

      context 'index:1の場合' do
        let(:str) { "(nth 1 '(a b c))" }
        it { is_expected.to eq :b }
      end

      context 'index:2の場合' do
        let(:str) { "(nth 2 '(a b c))" }
        it { is_expected.to eq :c }
      end

      context 'indexがはみ出した場合' do
        let(:str) { "(nth 3 '(a b c))" }
        it { is_expected.to eq nil }
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
      context 'リストが空の場合①' do
        let(:str) do
          <<~LISP
            (null '())
          LISP
        end
        it { is_expected.to eq true }
      end

      context 'リストが空の場合②' do
        let(:str) do
          <<~LISP
            (null ())
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

    describe '#incf' do
      let(:str) do
        <<~LISP
          (setq x 10)
          (incf x)
          x
        LISP
      end
      it { is_expected.to eq 11 }
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

    describe '#copy-tree' do
      context 'コピーする' do
        let(:str) do
          <<~LISP
            (copy-tree '((1 2) (3 4)))
          LISP
        end
        it { is_expected.to eq [[1, 2], [3, 4]] }
      end

      context 'メモリを比較する(copyできているか確認する)' do
        let(:str) do
          <<~LISP
            (setq a '((1 2) (3 4)))
            (setq b (copy-tree a))
            (eq a b)
          LISP
        end
        it { is_expected.to eq nil }
      end

      context 'メモリを比較する(deep copyできているか確認する)' do
        let(:str) do
          <<~LISP
            (setq a '((1 2) (3 4)))
            (setq b (copy-tree a))
            (eq (car a) (car b))
          LISP
        end
        it { is_expected.to eq nil }
      end

      context "TODO: 普通にバグってる。OnLispのサンプルコード" do
        let(:str) do
          <<~LISP
            (mapcar #'copy-tree '((a b) (c d e)))
          LISP
        end
        it { is_expected.to eq [[:a, :b], [:c, :d, :e]] }
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

    describe '#case' do
      context '数値の比較の場合' do
        context '値が一致する場合' do
          let(:str) do
            <<~LISP
              (setq x 2)
              (case x
                (1 'dog)
                (2 'cat)
                (otherwise 'human))
            LISP
          end
          it { is_expected.to eq :cat }
        end

        context 'otherwiseに到達する場合' do
          let(:str) do
            <<~LISP
              (setq x 2)
              (case x
                (1 'dog)
                (2 'cat)
                (otherwise 'human))
            LISP
          end
          it { is_expected.to eq :cat }
        end

        context '値が一致しない場合' do
          let(:str) do
            <<~LISP
              (setq x 3)
              (case x
                (1 'dog)
                (2 'cat))
            LISP
          end
          it { is_expected.to eq nil }
        end
      end

      context 'シンボルの比較の場合' do
        context '値が一致する場合' do
          let(:str) do
            <<~LISP
              (setq x 'cat)
              (case x
                (dog 1)
                (cat 2)
                (otherwise 3))
            LISP
          end
          it { is_expected.to eq 2 }
        end

        context 'otherwiseに到達する場合' do
          let(:str) do
            <<~LISP
              (setq x 'human)
              (case x
                ('dog 1)
                ('cat 2)
                (otherwise 3))
            LISP
          end
          it { is_expected.to eq 3 }
        end

        context '値が一致しない場合' do
          let(:str) do
            <<~LISP
              (setq x 'human)
              (case x
                ('dog 1)
                ('cat 2))
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

    describe '#dotimes' do
      context '5回ループする' do
        let(:str) do
          <<~LISP
            (setq x 0)
            (dotimes (n 5)
              (setq x (+ 1 x)))
            x
          LISP
        end
        it { is_expected.to eq 5 }
      end

      context '(length list)回ループする' do
        let(:str) do
          <<~LISP
            (setq x 0)
            (dotimes (n (length (list 1 2 3)))
              (setq x (+ 1 x)))
            x
          LISP
        end
        it { is_expected.to eq 3 }
      end

      context '変数nを使う' do
        let(:str) do
          <<~LISP
            (dotimes (n 5)
              (setq x n))
            x
          LISP
        end
        it { is_expected.to eq 4 }
      end

      context '戻り値を指定する' do
        let(:str) do
          <<~LISP
            (dotimes (n 5 2) n)
          LISP
        end
        it { is_expected.to eq 2 }
      end

      context '戻り値に変数を指定する' do
        let(:str) do
          <<~LISP
            (dotimes (n 5 n) 0)
          LISP
        end
        it { is_expected.to eq 5 }
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
    end

    describe '#sort' do
      context "OnLispのサンプルコード" do
        let(:str) do
          <<~LISP
            (sort '(1 4 2 5 6 7 3) #'<)
          LISP
        end
        it { is_expected.to eq [1, 2, 3, 4, 5, 6, 7] }
      end
    end

    describe '#get' do
      context 'グローバル変数を取得する' do
        let(:str) do
          <<~LISP
            (setf (get 'color 'shade) 'dark)
            (get 'color 'shade)
          LISP
        end
        it { is_expected.to eq :dark }
      end

      context "On Lispのサンプルコード" do
        let(:str) do
          <<~LISP
            (setf (get 'dog 'behavior) (lambda () 'hoge))
            (defun behave (animal)
              (funcall (get animal 'behavior)))
            (behave 'dog)
          LISP
        end
        it { is_expected.to eq :hoge }
      end
    end

    describe '#length' do
      context "配列の場合①" do
        let(:str) do
          <<~LISP
            (length '(1 2 3 4 5))
          LISP
        end
        it { is_expected.to eq 5 }
      end

      context "配列の場合②" do
        let(:str) do
          <<~LISP
            (length '(1))
          LISP
        end
        it { is_expected.to eq 1 }
      end

      context "空配列の場合" do
        let(:str) do
          <<~LISP
            (length ())
          LISP
        end
        it { is_expected.to eq 0 }
      end

      context "nilの場合" do
        let(:str) do
          <<~LISP
            (length nil)
          LISP
        end
        it { is_expected.to eq 0 }
      end
    end

    describe '再帰処理の動作確認' do
      context '合計値' do
        context 'n == 0' do
          let(:str) do
            <<~LISP
              (defun nsum (n)
                (if (= n 0)
                  0
                  (+ n (nsum (- n 1)))))
              (nsum 0)
            LISP
          end
          it { is_expected.to eq 0 }
        end

        context 'n == 1' do
          let(:str) do
            <<~LISP
              (defun nsum (n)
                (if (= n 0)
                  0
                  (+ n (nsum (- n 1)))))
              (nsum 1)
            LISP
          end
          it { is_expected.to eq 1 }
        end

        context 'n == 2' do
          let(:str) do
            <<~LISP
              (defun nsum (n)
                (if (= n 0)
                  0
                  (+ n (nsum (- n 1)))))
              (nsum 2)
            LISP
          end
          it { is_expected.to eq 3 }
        end
      end

      context '階乗' do
        let(:str) do
          <<~LISP
            (defun fact (n)
              (if (= n 0)
                1
                (* n (fact (- n 1)))))
            (fact 5)
          LISP
        end
        it { is_expected.to eq 120 }
      end
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
      # Common Lispで動くサンプルコードはなかった
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
          context "#'<" do
            let(:str) do
              <<~LISP
                ;(sort '(1 4 2 5 6 7 3) #'<)
                #'<
              LISP
            end
            it { is_expected.to be_instance_of Proc }
          end

          context "実行する" do
            let(:str) do
              <<~LISP
                (sort '(1 4 2 5 6 7 3) #'<)
              LISP
            end
            it { is_expected.to eq [1, 2, 3, 4, 5, 6, 7] }
          end
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
        context "CommonLispでは動かないコード？ (defun behave (animal)" do
          context 'caseの動作確認' do
            let(:str) do
              <<~LISP
                (defun wag-tail () "WagTail")
                (defun bark () "Bark")
  
                (case 'dog
                  (dog (wag-tail)
                    (bark))
                  (rat (scurry)
                    (squeak))
                  (cat (rub-legs)
                    (scratch-carpet))))
              LISP
            end
            it { is_expected.to eq "Bark" }
          end

          context '実際のサンプルコード' do
            let(:str) do
              <<~LISP
                (defun wag-tail () "WagTail")
                (defun bark () "Bark")
  
                (defun behave (animal)
                  (case animal
                    (dog (wag-tail)
                      (bark))
                    (rat (scurry)
                      (squeak))
                    (cat (rub-legs)
                      (scratch-carpet))))
  
                (behave 'dog)
              LISP
            end
            it { is_expected.to eq "Bark" }
          end
        end

        context "(setf (get 'dog 'behavior)" do
          let(:str) do
            <<~LISP
              (defun wag-tail () "WagTail")
              (defun bark () "Bark")

              (defun behave (animal)
                (funcall (get animal 'behavior)))

              (setf (get 'dog 'behavior)
                #'(lambda ()
                    (wag-tail)
                      (bark)))

              (behave 'dog)
            LISP
          end
          it { is_expected.to eq "Bark" }
        end
      end

      # schemeの仕様のため、対応しない
      # xcontext 'スコープ' do
      #   context "(let ((y 7))" do
      #     let(:str) do
      #       <<~LISP
      #         (let ((y 7))
      #           (defun scope-test (x)
      #             (list x y)))
      #       LISP
      #     end
      #     it { is_expected.to eq :"scope-test" }
      #   end
      #
      #   context "(let ((y 7))" do
      #     let(:str) do
      #       <<~LISP
      #         (let ((y 7))
      #           (defun scope-test (x)
      #             (list x y)))
      #         (let ((y 5))
      #           (scope-test 3))
      #             (3 5)
      #       LISP
      #     end
      #     it { is_expected.to eq :"scope-test" }
      #   end
      # end

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
          context '元のコード' do
            let(:str) do
              <<~LISP
                (let ((counter 0))
                  (defun new-id () (incf counter))
                  (defun reset-id () (setq counter 0))
                  (new-id)
                  (reset-id)
                  (new-id)
                  (new-id)
                  counter)
              LISP
            end
            it { is_expected.to eq 2 }
          end

          context 'incfをsetqに置き換えた' do
            let(:str) do
              <<~LISP
                (let ((counter 0))
                  (defun new-id () (setq counter (+ counter 1)))
                  (defun reset-id () (setq counter 0))
                  (new-id)
                  (reset-id)
                  (new-id)
                  (new-id)
                  counter)
              LISP
            end
            it { is_expected.to eq 2 }
          end

          context 'incfをsetqに置き換えたxxx' do
            let(:str) do
              <<~LISP
                (let ((counter 0))
                  (defun new-id () (setq counter (+ counter 1)))
                  (defun reset-id () (setq counter 0))
                  (new-id)
                  ; (reset-id)
                  ; (new-id)
                  ; (new-id)
                  counter)
              LISP
            end
            it { is_expected.to eq 1 }
          end

          context '関数経由でsetqを実行する' do
            let(:str) do
              <<~LISP
                (let ((counter 0))
                  (setq counter (+ counter 1))
                  (defun reset-id () (setq counter 0))
                  (reset-id)
                  counter)
              LISP
            end
            it { is_expected.to eq 0 }
          end

          context '関数を使わずに直接setqを実行する' do
            let(:str) do
              <<~LISP
                (let ((counter 0))
                  (setq counter (+ counter 1))
                  ; (defun new-id () (setq counter (+ counter 1)))
                  ; (defun reset-id () (setq counter 0))
                  ; (new-id)
                  ; (reset-id)
                  ; (new-id)
                  ; (new-id)
                  counter)
              LISP
            end
            it { is_expected.to eq 1 }
          end
        end

        context "(defun make-adder (n)" do
          let(:str) do
            <<~LISP
              (defun make-adder (n)
                #'(lambda (x) (+ x n)))
              (funcall (make-adder 1) 2)
            LISP
          end
          it { is_expected.to eq 3 }
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

        context "TODO: 普通にバグってるぽい。(funcall add2 5)" do
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
              (funcall addx 3) ; => 4
            LISP
          end
          it { is_expected.to eq 4 }
        end

        context "(funcall addx 100 t) (funcall addx 100 t)" do
          let(:str) do
            <<~LISP
              (defun make-adderb (n)
                #'(lambda (x &optional change)
                    (if change
                        (setq n x)
                        (+ x n))))
              (setq addx (make-adderb 1))
              (funcall addx 3) ; => 4
              (funcall addx 100 t) ; => 100
            LISP
          end
          it { is_expected.to eq 100 }
        end

        context "TODO: 単純にバグってそう。(funcall addx 3) (funcall addx 100 t) (funcall addx 3)" do
          let(:str) do
            <<~LISP
              (defun make-adderb (n)
                #'(lambda (x &optional change)
                    (if change
                        (setq n x)
                        (+ x n))))
              (setq addx (make-adderb 1))
              (funcall addx 3) ; => 4
              (funcall addx 100 t) ; => 100
              (funcall addx 3) ; => 103
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
          it do
            result = subject
            expect(result.size).to eq 3
            expect(result[0]).to be_instance_of Proc
            expect(result[1]).to be_instance_of Proc
            expect(result[2]).to be_instance_of Proc
          end
        end

        context "TODO: 単純にバグってそう。(funcall (car cities) 'boston)" do
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
              (funcall (car cities) 'boston) ; => us
            LISP
          end
          it { is_expected.to eq :us }
        end

        context "TODO: 単純にバグってそう。(funcall (second cities) 'london 'england)" do
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
              (funcall (car cities) 'boston) ; => us
              (funcall (second cities) 'london 'england) ; => london
            LISP
          end
          it { is_expected.to eq :london }
        end

        context "TODO: 単純にバグってそう。(funcall (car cities) 'london)" do
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
              (funcall (car cities) 'boston) ; => us
              (funcall (second cities) 'london 'england) ; => london
              (funcall (car cities) 'london) ; => england
            LISP
          end
          it { is_expected.to eq :england }
        end

        context "TODO: ↑を対応したら仕様が分かりそう。(funcall (car cities) 'london)" do
          let(:str) do
            <<~LISP
              (defun lookup (key db)
                (funcall (car db) key))
            LISP
            # TODO: callする
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

        context "TODO: 普通にバグってる。(mapcar #'copy-tree '((a b) (c d e)))" do
          let(:str) do
            <<~LISP
              (mapcar #'copy-tree '((a b) (c d e)))
            LISP
          end
          it { is_expected.to eq [[:a, :b], [:c, :d, :e]] }
        end

        context "(defun list+ (lst n) (mapcar #'(lambda (x) (+ x n)) lst))" do
          let(:str) do
            <<~LISP
              (defun list+ (lst n)
                (mapcar #'(lambda (x) (+ x n))
                        lst))
              (list+ '(2 5 7 3) 2)
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
          it { is_expected.to eq 4 }
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
          it { is_expected.to eq :'count-instances' }
        end

        context "TODO: mapcar #'instances-in lsts ここが原因っぽい (count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))" do
          context 'On Lispのサンプルコード' do
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
            # TODO: mapcar #'instances-in lsts ここが原因っぽい
            it { is_expected.to eq [1, 2, 1, 2] }
          end

          context '問題の部分を切り出す' do
            let(:str) do
              <<~LISP
                (labels ((instances-in () 2))
                  #'instances-in)
              LISP
            end
            it { is_expected.to be_instance_of Proc }
          end
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
              (our-find-if #'evenp '(1 3 5 6 8 10))
            LISP
          end
          it { is_expected.to eq 6 }
        end

        context "TODO: 無限ループする (defun our-length (lst)" do
          let(:str) do
            <<~LISP
              (defun our-length (lst)
                (labels ((rec (lst acc)
                         (if (null lst)
                             acc
                             (rec (cdr lst) (1+ acc)))))
                  (rec lst 0)))
              (our-length '(1 2 3 4 5))
            LISP
          end
          it { is_expected.to eq 5 }
        end

        context "コンパイラの最適化の話のためスキップ (proclaim '(optimize speed))" do
          let(:str) do
            <<~LISP
              (proclaim '(optimize speed))
            LISP
          end
          xit {}
        end

        context "TODO: 無限ループ (defun triangle (n)" do
          let(:str) do
            <<~LISP
              (defun triangle (n)
                (labels ((tri (c n)
                              ; (declare (type fixnum n c)) コンパイラ関係の処理のためスキップ
                              (if (zerop n)
                                  c
                                  (tri (+ n c)
                                       (- n 1)))))
                                  ; (tri (the fixnum (+ n c)) コンパイラ関係の処理のためスキップ
                                  ;      (the fixnum (- n 1))))))
                  (tri 0 n)))
              (triangle 6)
            LISP
          end
          it { is_expected.to eq 21 }
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

        # コンパイラ関係の処理なのでスキップ
        xcontext "(compiled-function-p #'foo)" do
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

        # コンパイラ関係の処理なのでスキップ
        xcontext "(compile 'foo)(compiled-function-p #'foo)" do
          let(:str) do
            <<~LISP
              (defun foo (x) (1+ x))
              (compile 'foo)
              (compiled-function-p #'foo)
            LISP
          end
          it { is_expected.to eq true }
        end

        context "(compile nil '(lambda (x) (+ x 2)))" do
          let(:str) do
            <<~LISP
              (compile nil '(lambda (x) (+ x 2)))
            LISP
          end
          it { is_expected.to eq nil }
        end

        # コンパイラ関係の処理なのでスキップ
        xcontext "(compiled-function-p #'bar))" do
          let(:str) do
            <<~LISP
              (progn (compile 'bar '(lambda (x) (* x 3)))
                (compiled-function-p #'bar))
            LISP
          end
          it { is_expected.to eq true }
        end

        context '(let ((y 2)) (defun foo (x) (+ x y)))' do
          let(:str) do
            <<~LISP
              (let ((y 2))
                (defun foo (x) (+ x y)))
            LISP
          end
          it { is_expected.to eq :foo }
        end

        context "(compile 'make-adder)" do
          let(:str) do
            <<~LISP
              (compile 'make-adder)
            LISP
          end
          it { is_expected.to eq nil }
        end

        # コンパイラ関係の処理なのでスキップ
        xcontext '(compiled-function-p (make-adder 2))' do
          let(:str) do
            <<~LISP
              (defun make-adder (n)
                #'(lambda (x) (+ x n)))
              (compile 'make-adder)
              (compiled-function-p (make-adder 2))
            LISP
          end
          it { is_expected.to eq true }
        end

        context '(defun 50th (lst) (nth 49 lst))' do
          let(:str) do
            <<~LISP
              (defun 50th (lst) (nth 49 lst))
              (50th `(1 2 3 4 5 6 7 8 9 10
                      11 12 13 14 15 16 17 18 19 20
                      21 22 23 24 25 26 27 28 29 30
                      31 32 33 34 35 36 37 38 39 40
                      41 42 43 44 45 46 47 48 49 50))
            LISP
          end
          it { is_expected.to eq 50 }
        end

        context "TODO: proclaimは空実装。(proclaim '(inline 50th))" do
          let(:str) do
            <<~LISP
              (proclaim '(inline 50th))
            LISP
          end
          it { is_expected.to eq nil }
        end

        context '(defun foo (lst) (+ (50th lst) 1))' do
          let(:str) do
            <<~LISP
              (defun 50th (lst) (nth 49 lst))
              (defun foo (lst)
                (+ (50th lst) 1))
              (foo `(1 2 3 4 5 6 7 8 9 10
                    11 12 13 14 15 16 17 18 19 20
                    21 22 23 24 25 26 27 28 29 30
                    31 32 33 34 35 36 37 38 39 40
                    41 42 43 44 45 46 47 48 49 50))
            LISP
          end
          it { is_expected.to eq 51 }
        end

        context '(defun foo (lst) (+ (nth 49 lst) 1))' do
          let(:str) do
            <<~LISP
              (defun foo (lst)
                (+ (nth 49 lst) 1))
              (foo `(1 2 3 4 5 6 7 8 9 10
                    11 12 13 14 15 16 17 18 19 20
                    21 22 23 24 25 26 27 28 29 30
                    31 32 33 34 35 36 37 38 39 40
                    41 42 43 44 45 46 47 48 49 50))
            LISP
          end
          it { is_expected.to eq 51 }
        end
      end
    end

    context '3. 関数的プログラミング: https://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/functionalProgramming.html' do
      context '関数的デザイン' do
        context 'TODO: truncateが未実装。(defun bad-reverse (lst)' do
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

        context 'TODO: truncateが未実装。(defun bad-reverse (lst)' do
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

        context 'TODO: truncateが未実装。(defun bad-reverse (lst)' do
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

        context "(setq lst '(a b c)) (bad-reverse lst)" do
          let(:str) do
            <<~LISP
              (setq lst '(a b c)) ; => (A B C)
              (bad-reverse lst) ; => NIL
              lst ; => (C B A)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(setq lst '(a b c))" do
          let(:str) do
            <<~LISP
              > (setq lst '(a b c))
              (A B C)
              > (good-reverse lst)
              (C B A)
              > lst
              (A B C)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context '(defun good-reverse (lst)' do
          let(:str) do
            <<~LISP
              (defun good-reverse (lst)
                (labels ((rev (lst acc)
                         (if (null lst)
                             acc
                             (rev (cdr lst) (cons (car lst) acc)))))
                  (rev lst nil)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context '#TODO: reverseが未実装。(reverse lst)' do
          let(:str) do
            <<~LISP
              (reverse lst)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context '#TODO: reverseが未実装。(setq lst (reverse lst))' do
          let(:str) do
            <<~LISP
              (setq lst (reverse lst))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context '#TODO: nreverseが未実装。(nreverse lst)' do
          let(:str) do
            <<~LISP
              (nreverse lst)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "#TODO: nreverseが未実装。(setq lst '(a b c)) (nreverse lst) lst" do
          let(:str) do
            <<~LISP
              (setq lst '(a b c)) ; => (A B C)
              (nreverse lst) ; => (C B A)
              lst ; => (A)
            LISP
          end
          it { is_expected.to eq [:a] }
        end

        context '(nconc x y)' do
          let(:str) do
            <<~LISP
              (nconc x y)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context '(setq x (nconc x y))' do
          let(:str) do
            <<~LISP
              (setq x (nconc x y))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "set setq setf psetf psetq incf decf push pop pushnew" do
          let(:str) do
            <<~LISP
              set setq setf psetf psetq incf decf push pop pushnew
              rplaca rplacd rotatef shiftf remf remprop remhash
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(truncate 26.21875)" do
          let(:str) do
            <<~LISP
              > (truncate 26.21875)
              26
              0.21875
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context 'TODO: truncateが未実装。(= (truncate 26.21875) 26)' do
          let(:str) do
            <<~LISP
              (= (truncate 26.21875) 26) ; => T
            LISP
          end
          it { is_expected.to eq true }
        end

        context '(multiple-value-bind (int frac) (truncate 26.21875) (list int frac))' do
          let(:str) do
            <<~LISP
              (multiple-value-bind (int frac) (truncate 26.21875) ; => (26 0.21875)
                (list int frac))
            LISP
          end
          it { is_expected.to eq [26, 0.21875] }
        end

        context "(defun powers (x)" do
          let(:str) do
            <<~LISP
              (defun powers (x)
                (values x (sqrt x) (expt x 2))) ; => POWERS

              (multiple-value-bind (base root square) (powers 4)
                (list base root square)) ; => (4 2.0 16)
            LISP
          end
          it { is_expected.to eq [4, 2.0, 16] }
        end
      end

      context '命令的プログラミングの裏返し' do
        context "(defun fun (x) (list 'a (expt (car x) 2)))" do
          let(:str) do
            <<~LISP
              (defun fun (x)
                (list 'a (expt (car x) 2)))
              (fun '(1 2 3))
            LISP
          end
          it { is_expected.to eq [:a, 1] }
        end

        context "(defun imp (x)" do
          let(:str) do
            <<~LISP
              (defun imp (x)
                (let (y sqr)
                  (setq y (car x))
                  (setq sqr (expt y 2))
                  (list 'a sqr)))
              (imp '(1 2 3))
            LISP
          end
          it { is_expected.to eq [:a, 1] }
        end
      end

      context '関数的インタフェイス' do
        context "TODO: nconcが未実装。(defun qualify (expr)" do
          let(:str) do
            <<~LISP
              (defun qualify (expr)
                (nconc (copy-list expr) (list 'maybe)))
              (qualify '(1 2))
            LISP
          end
          it { is_expected.to eq [:a, 1] }
        end

        context '(let ((x 0)) (defun total (y) (incf x y)))' do
          let(:str) do
            <<~LISP
              (let ((x 0))
                (defun total (y)
                  (incf x y)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun ok (x) (nconc (list 'a x) (list 'c)))" do
          let(:str) do
            <<~LISP
              (defun ok (x)
                (nconc (list 'a x) (list 'c)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun not-ok (x) (nconc (list 'a) x (list 'c)))" do
          let(:str) do
            <<~LISP
              (defun not-ok (x)
                (nconc (list 'a) x (list 'c)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context '(defun anything (x) (+ x *anything*))' do
          let(:str) do
            <<~LISP
              (defun anything (x)
                (+ x *anything*))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun f (x) (let ((val (g x))) ; ここでvalを書き換えていいものか？ ))" do
          let(:str) do
            <<~LISP
              (defun f (x)
                (let ((val (g x)))
                ; ここでvalを書き換えていいものか？
                ))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun exclaim (expression) (append expression '(oh my)))" do
          let(:str) do
            <<~LISP
              (defun exclaim (expression)
                (append expression '(oh my)))
            LISP
          end
          it { is_expected.to eq :exclaim }
        end

        context "(exclaim '(lions and tigers and bears))" do
          let(:str) do
            <<~LISP
              (defun exclaim (expression)
                (append expression '(oh my)))

              (exclaim '(lions and tigers and bears)) ; => (LIONS AND TIGERS AND BEARS OH MY)
            LISP
          end
          it { is_expected.to eq [:lions, :and, :tigers, :and, :bears, :oh, :my] }
        end

        context "TODO: nconcが未実装。(nconc * '(goodness))" do
          let(:str) do
            <<~LISP
              (defun exclaim (expression)
                (append expression '(oh my)))

              (exclaim '(lions and tigers and bears)) ; => (LIONS AND TIGERS AND BEARS OH MY)

              (nconc * '(goodness)) ; => (LIONS AND TIGERS AND BEARS OH MY GOODNESS)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(exclaim '(fixnums and bignums and floats))" do
          let(:str) do
            <<~LISP
              (defun exclaim (expression)
                (append expression '(oh my)))

              (exclaim '(lions and tigers and bears)) ; => (LIONS AND TIGERS AND BEARS OH MY)

              (nconc * '(goodness)) ; => (LIONS AND TIGERS AND BEARS OH MY GOODNESS)

              (exclaim '(fixnums and bignums and floats)) ; => (FIXNUMS AND BIGNUMS AND FLOATS OH MY GOODNESS)
            LISP
          end
          it { is_expected.to eq [:fixnums, :and, :bignums, :and, :floats, :oh, :my, :goodness] }
        end

        context "(defun exclaim (expression) (append expression (list 'oh 'my)))" do
          let(:str) do
            <<~LISP
              (defun exclaim (expression)
                (append expression (list 'oh 'my)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end
    end

    context '4. ユーティリティ関数: https://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/utilityFunctions.html' do
      context 'ユーティリティの誕生' do
        context "(defun all-nicknames (names)" do
          let(:str) do
            <<~LISP
              (defun all-nicknames (names)
                (if (null names)
                    nil
                    (nconc (nicknames (car names))
                      (all-nicknames (cdr names)))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(mapcan #'nicknames people)" do
          let(:str) do
            <<~LISP
              (mapcan #'nicknames people)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(let ((town (find-if #'bookshops towns))) (values town (bookshops town)))" do
          let(:str) do
            <<~LISP
              (let ((town (find-if #'bookshops towns)))
                (values town (bookshops town)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun find-books (towns)" do
          let(:str) do
            <<~LISP
              (defun find-books (towns)
                (if (null towns)
                    nil
                    (let ((shops (bookshops (car towns))))
                      (if shops
                          (values (car towns) shops)
                          (find-books (cdr towns))))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun find2 (fn lst)" do
          let(:str) do
            <<~LISP
              (defun find2 (fn lst)
                (if (null lst)
                    nil
                    (let ((val (funcall fn (car lst))))
                      (if val
                          (values (car lst) val)
                          (find2 fn (cdr lst))))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(find2 #'bookshops towns)" do
          let(:str) do
            <<~LISP
              (find2 #'bookshops towns)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context '抽象化への投資' do
        context "(> (length x) (length y))" do
          let(:str) do
            <<~LISP
              (> (length x) (length y))
            LISP
          end
          it { is_expected.to eq [:a, 1] }
        end

        context "(mapcar fn (append x y z))" do
          let(:str) do
            <<~LISP
              (mapcar fn (append x y z))
            LISP
          end
          it { is_expected.to eq [:a, 1] }
        end
      end

      context 'リストに対する操作' do
        context "(proclaim '(inline last1 single append1 conc1 mklist))" do
          let(:str) do
            <<~LISP
              (proclaim '(inline last1 single append1 conc1 mklist))

              (defun last1 (lst)
                (car (last lst)))

              (defun single (lst)
                (and (consp lst) (not (cdr lst))))

              (defun append1 (lst obj)
                (append lst (list obj)))

              (defun conc1 (lst obj)
                (nconc lst (list obj)))

              (defun mklist (obj)
                (if (listp obj) obj (list obj)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(last1 blub)" do
          let(:str) do
            <<~LISP
              > (last1 "blub")
              >>Error: "blub" is not a list.
              Broken at LAST...
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(= (length lst) 1)" do
          let(:str) do
            <<~LISP
              (= (length lst) 1)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(mapcan #'(lambda (d) (mklist (lookup d))) data)" do
          let(:str) do
            <<~LISP
              (mapcan #'(lambda (d) (mklist (lookup d)))
                      data)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(> (length x) (length y))" do
          let(:str) do
            <<~LISP
              (> (length x) (length y))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "TODO: filterが未実装。(filter #'(lambda (x) (if (numberp x) (1+ x))) '(a 1 2 b 3 c d 4))" do
          let(:str) do
            <<~LISP
              (filter #'(lambda (x) (if (numberp x) (1+ x)))
                '(a 1 2 b 3 c d 4)) ; => (2 3 4 5)
            LISP
          end
          it { is_expected.to eq [2, 3, 4, 5] }
        end

        context "(defun longer (x y)" do
          let(:str) do
            <<~LISP
              (defun longer (x y)
                (labels ((compare (x y)
                                  (and (consp x)
                                       (or (null y)
                                           (compare (cdr x) (cdr y))))))
                  (if (and (listp x) (listp y))
                      (compare x y)
                      (> (length x) (length y)))))

              (defun filter (fn lst)
                (let ((acc nil))
                  (dolist (x lst)
                    (let ((val (funcall fn x)))
                      (if val (push val acc))))
                  (nreverse acc)))

              (defun group (source n)
                (if (zerop n) (error "zero length"))
                (labels ((rec (source acc)
                              (let ((rest (nthcdr n source)))
                                (if (consp rest)
                                    (rec rest (cons (subseq source 0 n) acc))
                                    (nreverse (cons source acc))))))
                  (if source (rec source nil) nil)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "TODO: groupが未実装。(group '(a b c d e f g) 2)" do
          let(:str) do
            <<~LISP
              (group '(a b c d e f g) 2) ; => ((A B) (C D) (E F) (G))
            LISP
          end
          it { is_expected.to eq [[:a, :b], [:c, :d], [:e, :f], [:g]] }
        end

        context "(defun flatten (x)" do
          let(:str) do
            <<~LISP
              (defun flatten (x)
                (labels ((rec (x acc)
                              (cond ((null x) acc)
                                    ((atom x) (cons x acc))
                                    (t (rec (car x) (rec (cdr x) acc))))))
                  (rec x nil)))

              (defun prune (test tree)
                (labels ((rec (tree acc)
                              (cond ((null tree) (nreverse acc))
                                    ((consp (car tree))
                                     (rec (cdr tree)
                                          (cons (rec (car tree) nil) acc)))
                                    (t (rec (cdr tree)
                                            (if (funcall test (car tree))
                                              acc
                                              (cons (car tree) acc)))))))
                  (rec tree nil)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "TODO: flattenが未実装。(flatten '(a (b c) ((d e) f)))" do
          let(:str) do
            <<~LISP
              (flatten '(a (b c) ((d e) f))) ; => (A B C D E F)
            LISP
          end
          it { is_expected.to eq [:a, :b, :c, :d, :e, :f] }
        end

        context "TODO: pruneが未実装。(prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9)))" do
          let(:str) do
            <<~LISP
              (prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9))) ; => (1 (3 (5)) 7 (9))
            LISP
          end
          it { is_expected.to eq [1, [3, [5]], 7, [9]] }
        end
      end

      context '検索' do
        context "(defun find2 (fn lst)" do
          let(:str) do
            <<~LISP
              (defun find2 (fn lst)
                (if (null lst)
                  nil
                  (let ((val (funcall fn (car lst))))
                    (if val
                        (values (car lst) val)
                        (find2 fn (cdr lst))))))

              (defun before (x y lst &key (test #'eql))
                (and lst
                     (let ((first (car lst)))
                       (cond ((funcall test y first) nil)
                             ((funcall test x first) lst)
                             (t (before x y (cdr lst) :test test))))))

              (defun after (x y lst &key (test #'eql))
                (let ((rest (before y x lst :test test)))
                  (and rest (member x rest :test test))))

              (defun duplicate (obj lst &key (test #'eql))
                (member obj (cdr (member obj lst :test test))
                        :test test))

              (defun split-if (fn lst)
                (let ((acc nil))
                  (do ((src lst (cdr src)))
                    ((or (null src) (funcall fn (car src)))
                     (values (nreverse acc) src))
                    (push (car src) acc))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(before 'a 'b '(a b c d))" do
          let(:str) do
            <<~LISP
              > (before 'a 'b '(a b c d))
              (B C D)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(< (position 'a '(a b c d)) (position 'b '(a b c d)))" do
          let(:str) do
            <<~LISP
              (< (position 'a '(a b c d)) (position 'b '(a b c d)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(before 'a 'b '(a))" do
          let(:str) do
            <<~LISP
              > (before 'a 'b '(a))
              (A)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(after 'a 'b '(b a d))" do
          let(:str) do
            <<~LISP
              > (after 'a 'b '(b a d))
              (A D)
              > (after 'a 'b '(a))
              NIL
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(duplicate 'a '(a b c a d))" do
          let(:str) do
            <<~LISP
              > (duplicate 'a '(a b c a d))
              (A D)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(split-if #'(lambda (x) (> x 4))" do
          let(:str) do
            <<~LISP
              > (split-if #'(lambda (x) (> x 4))
                          '(1 2 3 4 5 6 7 8 9 10))
              (1 2 3 4)
              (5 6 7 8 9 10)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun most (fn lst)" do
          let(:str) do
            <<~LISP
              (defun most (fn lst)
                (if (null lst)
                    (values nil nil)
                    (let* ((wins (car lst))
                           (max (funcall fn wins)))
                      (dolist (obj (cdr lst))
                        (let ((score (funcall fn obj)))
                          (when (> score max)
                            (setq wins obj
                                  max score))))
                      (values wins max))))

              (defun best (fn lst)
                (if (null lst)
                    nil
                    (let ((wins (car lst)))
                      (dolist (obj (cdr lst))
                        (if (funcall fn obj wins)
                            (setq wins obj)))
                      wins)))

              (defun mostn (fn lst)
                (if (null lst)
                    (values nil nil)
                    (let ((result (list (car lst)))
                          (max (funcall fn (car lst))))
                      (dolist (obj (cdr lst))
                        (let ((score (funcall fn obj)))
                          (cond ((> score max)
                                 (setq max score
                                       result (list obj)))
                                ((= score max)
                                 (push obj result)))))
                      (values (nreverse result) max))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(most #'length '((a b) (a b c) (a) (e f g)))" do
          let(:str) do
            <<~LISP
              > (most #'length '((a b) (a b c) (a) (e f g)))
              (A B C)
              3
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(best #'> '(1 2 3 4 5))" do
          let(:str) do
            <<~LISP
              > (best #'> '(1 2 3 4 5))
              5
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(mostn #'length '((a b) (a b c) (a) (e f g)))" do
          let(:str) do
            <<~LISP
              > (mostn #'length '((a b) (a b c) (a) (e f g)))
              ((A B C) (E F G))
              3
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context '対応付け' do
        context "(map0-n #'1+ 5)" do
          let(:str) do
            <<~LISP
              (map0-n #'1+ 5) ; => (1 2 3 4 5 6)
            LISP
          end
          it { is_expected.to eq [1, 2, 3, 4, 5, 6] }
        end

        context "(mapa-b #'1+ -2 0 .5)" do
          let(:str) do
            <<~LISP
              (mapa-b #'1+ -2 0 .5) ; => (-1 -0.5 0.0 0.5 1.0)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun mapa-b (fn a b &optional (step 1))" do
          let(:str) do
            <<~LISP
              (defun mapa-b (fn a b &optional (step 1))
                (map-> fn
                       a
                       #'(lambda (x) (> x b))
                       #'(lambda (x) (+ x step))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun our-mapcan (fn &rest lsts)" do
          let(:str) do
            <<~LISP
              (defun our-mapcan (fn &rest lsts)
                (apply #'nconc (apply #'mapcar fn lsts)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(mapcar #'sqrt (append list1 list2))" do
          let(:str) do
            <<~LISP
              (mapcar #'sqrt (append list1 list2))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(mapcars #'sqrt list1 list2)" do
          let(:str) do
            <<~LISP
              (mapcars #'sqrt list1 list2)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun map0-n (fn n)" do
          let(:str) do
            <<~LISP
              (defun map0-n (fn n)
                (mapa-b fn 0 n))

              (defun map1-n (fn n)
              (mapa-b fn 1 n))

              (defun mapa-b (fn a b &optional (step 1))
                (do ((i a (+ i step))
                     (result nil))
                  ((> i b) (nreverse result))
                  (push (funcall fn i) result)))

              (defun map-> (fn start test-fn succ-fn)
                (do ((i start (funcall succ-fn i))
                     (result nil))
                  ((funcall test-fn i) (nreverse result))
                  (push (funcall fn i) result)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun mappend (fn &rest lsts)" do
          let(:str) do
            <<~LISP
              (defun mappend (fn &rest lsts)
                (apply #'append (apply #'mapcar fn lsts)))

              (defun mapcars (fn &rest lsts)
                (let ((result nil))
                  (dolist (lst lsts)
                    (dolist (obj lst)
                      (push (funcall fn obj) result)))
                  (nreverse result)))

              (defun rmapcar (fn &rest args)
                (if (some #'atom args)
                    (apply fn args)
                    (apply #'mapcar
                           #'(lambda (&rest args)
                               (apply #'rmapcar fn args))
                           args)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(rmapcar #'princ '(1 2 (3 4 (5) 6) 7 (8 9)))" do
          let(:str) do
            <<~LISP
              > (rmapcar #'princ '(1 2 (3 4 (5) 6) 7 (8 9)))
              123456789
              (1 2 (3 4 (5) 6) 7 (8 9))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40)))" do
          let(:str) do
            <<~LISP
              > (rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40)))
              (11 (22 (33) 44))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(mapa-b #'fn a b c)" do
          let(:str) do
            <<~LISP
              (mapa-b #'fn a b c)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(collect (#Mfn (scan-range :from a :upto b :by c)))" do
          let(:str) do
            <<~LISP
              (collect (#Mfn (scan-range :from a :upto b :by c)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context '入出力' do
        context "(defun readlist (&rest args)" do
          let(:str) do
            <<~LISP
              (defun readlist (&rest args)
                (values (read-from-string
                          (concatenate 'string "("
                                       (apply #'read-line args)
                                       ")"))))

              (defun prompt (&rest args)
                (apply #'format *query-io* args)
                (read *query-io*))

              (defun break-loop (fn quit &rest args)
                (format *query-io* "Entering break-loop.~%")
                (loop
                  (let ((in (apply #'prompt args)))
                    (if (funcall quit in)
                        (return)
                        (format *query-io* "~A~%" (funcall fn in))))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(readlist)" do
          let(:str) do
            <<~LISP
              > (readlist)
              Call me "Ed"
              (CALL ME "Ed")
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(prompt Enter a number between ~A and ~A.~%>> 1 10)" do
          let(:str) do
            <<~LISP
              > (prompt "Enter a number between ~A and ~A.~%>> " 1 10)
              Enter a number between 1 and 10.
              >> 3
              3
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(break-loop #'eval #'(lambda (x) (eq x :q)) >> )" do
          let(:str) do
            <<~LISP
              > (break-loop #'eval #'(lambda (x) (eq x :q)) ">> ")
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(+ 2 3)" do
          let(:str) do
            <<~LISP
              >> (+ 2 3)
              5
              >> :q
              :Q
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context 'シンボルとストリング' do
        context '(mkstr pi " pieces of " `pi)' do
          let(:str) do
            <<~LISP
              > (mkstr pi " pieces of " 'pi)
              "3.141592653589793 pieces of PI"
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(symb 'ar Madi #\L #\L 0)" do
          let(:str) do
            <<~LISP
              > (symb 'ar "Madi" #\L #\L 0)
              |ARMadiLL0|
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(let ((s (symb '(a b))))" do
          let(:str) do
            <<~LISP
              > (let ((s (symb '(a b))))
              (and (eq s '|(A B)|) (eq s '\(A\ B\))))
              T
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun mkstr (&rest args)" do
          let(:str) do
            <<~LISP
              (defun mkstr (&rest args)
                (with-output-to-string (s)
                  (dolist (a args) (princ a s))))

              (defun symb (&rest args)
                (values (intern (apply #'mkstr args))))

              (defun reread (&rest args)
                (values (read-from-string (apply #'mkstr args))))

              (defun explode (sym)
                (map 'list #'(lambda (c)
                               (intern (make-string 1
                                                    :initial-element c)))
                     (symbol-name sym)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(explode 'bomb)" do
          let(:str) do
            <<~LISP
              > (explode 'bomb)
              (B O M B)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end
    end

    context '5. 返り値としての関数: https://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/returningFunctions.html' do
      context 'Common Lispは進化する' do
        context "(remove-if-not #'pred lst)" do
          let(:str) do
            <<~LISP
              ; (remove-if-not #'pred lst) predがCommonLispにないものなので変更する
              (remove-if-not #'evenp '(1 2 3 4))
            LISP
          end
          it { is_expected.to eq [2, 4] }
        end

        context "(remove-if #'(lambda (x) (not (pred x))) lst)" do
          let(:str) do
            <<~LISP
              (remove-if #'(lambda (x) (not (pred x))) lst)
            LISP
          end
          it { is_expected.to eq [:a, 1] }
        end

        context "(remove-if-not #'pred lst)" do
          let(:str) do
            <<~LISP
              (remove-if-not #'pred lst)
            LISP
          end
          it { is_expected.to eq [:a, 1] }
        end

        context "(remove-if (complement #'pred) lst)" do
          let(:str) do
            <<~LISP
              (remove-if (complement #'pred) lst)
            LISP
          end
          it { is_expected.to eq [:a, 1] }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun joiner (obj)
                (typecase obj
                  (cons #'append)
                  (number #'+)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun join (&rest args)
                (apply (joiner (car args)) args))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun make-adder (n)
                #'(lambda (x) (+ x n)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              > (setq add3 (make-adder 3))
              #<Interpreted-Function BF1356>
              > (funcall add3 2)
              5
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun complement (fn)
                #'(lambda (&rest args) (not (apply fn args))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              > (remove-if (complement #'oddp) '(1 2 3 4 5 6))
              (1 3 5)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context '直交性' do
        context "(setf (get 'ball 'color) 'red)" do
          let(:str) do
            <<~LISP
              (setf (get 'ball 'color) 'red)
            LISP
          end
          it { is_expected.to eq :red }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defvar *!equivs* (make-hash-table))

              (defun ! (fn)
                (or (gethash fn *!equivs*) fn))

              (defun def! (fn fn!)
                (setf (gethash fn *!equivs*) fn!))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (def! #'remove-if #'delete-if)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (delete-if #'oddp lst)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (funcall (! #'remove-if) #'oddp lst)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              ((! remove-if) oddp lst)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context '関数の値のメモワイズ' do
        context "(defun memoize (fn)" do
          let(:str) do
            <<~LISP
              (defun memoize (fn)
                (let ((cache (make-hash-table :test #'equal)))
                  #'(lambda (&rest args)
                      (multiple-value-bind (val win) (gethash args cache)
                        (if win
                            val
                            (setf (gethash args cache)
                                  (apply fn args)))))))
            LISP
          end
          it { is_expected.to eq :red }
        end

        context "" do
          let(:str) do
            <<~LISP
              > (setq slowid (memoize #'(lambda (x) (sleep 5) x)))
              #<Interpreted-Function C38346>
              > (time (funcall slowid 1))
              Elapsed Time = 5.15 seconds
              1
              > (time (funcall slowid 1))
              Elapsed Time = 0.00 seconds
              1
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context '関数を合成する' do
        context "" do
          let(:str) do
            <<~LISP
              (defun compose (&rest fns)
                (if fns
                    (let ((fn1 (car (last fns)))
                          (fns (butlast fns)))
                      #'(lambda (&rest args)
                                 (reduce #'funcall fns
                                         :from-end t
                                         :initial-value (apply fn1 args))))
                    #'identity))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (compose #'list #'1+)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              #'(lambda (x) (list (1+ x)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              > (funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))
              4
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun complement (pred)
                (compose #'not pred))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun fif (if then &optional else)
                #'(lambda (x)
                    (if (funcall if x)
                        (funcall then x)
                        (if else (funcall else x)))))

              (defun fint (fn &rest fns)
                (if (null fns)
                    fn
                    (let ((chain (apply #'fint fns)))
                      #'(lambda (x)
                          (and (funcall fn x) (funcall chain x))))))

              (defun fun (fn &rest fns)
                (if (null fns)
                    fn
                    (let ((chain (apply #'fun fns)))
                      #'(lambda (x)
                          (or (funcall fn x) (funcall chain x))))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (mapcar #'(lambda (x)
                          (if (slave x)
                              (owner x)
                              (employer x)))
                      people)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (mapcar (fif #'slave #'owner #'employer)
                      people)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (find-if #'(lambda (x)
                           (and (signed x) (sealed x) (delivered x)))
                       docs)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (find-if (fint #'signed #'sealed #'delivered) docs)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context 'Cdr部での再帰' do
        context '(defun our-length (lst)' do
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

        context "" do
          let(:str) do
            <<~LISP
              (defun our-every (fn lst)
                (if (null lst)
                    t
                    (and (funcall fn (car lst))
                         (our-every fn (cdr lst)))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun lrec (rec &optional base)
                (labels ((self (lst)
                               (if (null lst)
                                   (if (functionp base)
                                       (funcall base)
                                       base)
                                   (funcall rec (car lst)
                                            #'(lambda ()
                                                (self (cdr lst)))))))
                  #'self))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (lrec #'(lambda (x f) (1+ (funcall f))) 0)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              ; copy-list
              (lrec #'(lambda (x f) (cons x (funcall f))))
              ; remove-duplicates
              (lrec #'(lambda (x f) (adjoin x (funcall f))))
              ; find-if, for some function fn
              (lrec #'(lambda (x f) (if (fn x) x (funcall f))))
              ; some, for some function fn
              (lrec #'(lambda (x f) (or (fn x) (funcall f))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context '部分ツリーでの再帰' do
        context "" do
          let(:str) do
            <<~LISP
              (a b c) = (a . (b . (c . nil)))
              (a b (c d)) = (a . (b . ((c . (d . nil)) . nil)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              > (setq x '(a b)
                      listx (list x 1))
              ((A B) 1)
              > (eq x (car (copy-list listx)))
              T
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              > (eq x (car (copy-tree listx)))
              NIL
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun our-copy-tree (tree)
                (if (atom tree)
                    tree
                    (cons (our-copy-tree (car tree))
                          (if (cdr tree) (our-copy-tree (cdr tree))))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun count-leaves (tree)
                (if (atom tree)
                    1
                    (+ (count-leaves (car tree))
                       (or (if (cdr tree) (count-leaves (cdr tree)))
                           1))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              > (count-leaves '((a b (c d)) (e) f))
              10
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              > (flatten '((a b (c d)) (e) f ()))
              (A B C D E F)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun flatten (tree)
                (if (atom tree)
                    (mklist tree)
                    (nconc (flatten (car tree))
                           (if (cdr tree) (flatten (cdr tree))))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun rfind-if (fn tree)
                (if (atom tree)
                    (and (funcall fn tree) tree)
                    (or (rfind-if fn (car tree))
                        (if (cdr tree) (rfind-if fn (cdr tree))))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              > (rfind-if (fint #'numberp #'addp) '(2 (3 4) 5))
              3
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun ttrav (rec &optional (base #'identity))
                (labels ((self (tree)
                               (if (atom tree)
                                   (if (functionp base)
                                       (funcall base tree)
                                       base)
                                   (funcall rec (self (car tree))
                                            (if (cdr tree)
                                                (self (cdr tree)))))))
                  #'self))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (ttrav #'cons #'identity)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              ; our-copy-tree
              (ttrav #'cons)
              ; count-leaves
              (ttrav #'(lambda (l r) (+ l (or r 1))) 1)
              ; flatten
              (ttrav #'nconc #'mklist)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (trec #'(lambda (o l r) (nconc (funcall l) (funcall r)))
                    #'mklist)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (trec #'(lambda (o l r) (or (funcall l) (funcall r)))
                    #'(lambda (tree) (and (oddp tree) tree)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "" do
          let(:str) do
            <<~LISP
              (defun trec (rec &optional (base #'identity))
                (labels
                  ((self (tree)
                         (if (atom tree)
                             (if (functionp base)
                                 (funcall base tree)
                                 base)
                             (funcall rec tree
                                      #'(lambda ()
                                          (self (car tree)))
                                      #'(lambda ()
                                          (if (cdr tree)
                                              (self (cdr tree))))))))
                  #'self))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context 'いつ関数を作るべきか' do
        context "" do
          let(:str) do
            <<~LISP
              (find-if #.(compose #'oddp #'truncate) lst)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end
    end

    context '6. 表現としての関数: https://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/functionsAsRepresentation.html' do
      context 'ネットワーク' do
        context "(run-node 'people)" do
          let(:str) do
            <<~LISP
              > (run-node 'people)
              Is the person a man?
              >> yes
              Is he living?
              >> no
              Was he American?
              >> yes
              Is he on a coin?
              >> yes
              Is the coin a penny?
              >> yes
              LINCOLN
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defnode 'people Is the person a man?" do
          let(:str) do
            <<~LISP
              (defnode 'people "Is the person a man?"
                       'male 'female)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defstruct node contents yes no)" do
          let(:str) do
            <<~LISP
              (defstruct node contents yes no)

              (defvar *nodes* (make-hash-table))

              (defun defnode (name conts &optional yes no)
                (setf (gethash name *nodes*)
                      (make-node :contents conts
                                 :yes yes
                                 :no no)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defnode 'people Is the person a man? 'male 'female)" do
          let(:str) do
            <<~LISP
              (defnode 'people "Is the person a man?" 'male 'female)
              (defnode 'male "Is he living?" 'liveman 'deadman)
              (defnode 'deadman "Was he American?" 'us 'them)
              (defnode 'us "Is he on a coin?" 'coin 'cidence)
              (defnode 'coin "Is the coin a penny?" 'penny 'coins)
              (defnode 'penny 'lincoln)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun run-node (name)" do
          let(:str) do
            <<~LISP
              (defun run-node (name)
                (let ((n (gethash name *nodes*)))
                  (cond ((node-yes n)
                         (format t "~A~%>> " (node-contents n))
                         (case (read)
                           (yes (run-node (node-yes n)))
                           (t (run-node (node-no n)))))
                        (t (node-contents n)))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context 'ネットワークのコンパイル' do
        context "(defvar *nodes* (make-hash-table))" do
          let(:str) do
            <<~LISP
              (defvar *nodes* (make-hash-table))

              (defun defnode (name conts &optional yes no)
                (setf (gethash name *nodes*)
                      (if yes
                          #'(lambda ()
                              (format t "~A~%>> " conts)
                              (case (read)
                                (yes (funcall (gethash yes *nodes*)))
                                (t (funcall (gethash no *nodes*)))))
                          #'(lambda () conts))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(funcall (gethash 'people *nodes*))" do
          let(:str) do
            <<~LISP
              (funcall (gethash 'people *nodes*))
              Is the person a man?
              >>
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "TODO: defvarが未実装。(defvar *nodes* nil)" do
          let(:str) do
            <<~LISP
              (defvar *nodes* nil)

              (defun defnode (&rest args)
                (push args *nodes*)
                args)

              (defun compile-net (root)
                (let ((node (assoc root *nodes*)))
                  (if (null node)
                      nil
                      (let ((conts (second node))
                            (yes (third node))
                            (no (fourth node)))
                        (if yes
                            (let ((yes-fn (compile-net yes))
                                  (no-fn (compile-net no)))
                              #'(lambda ()
                                  (format t "~A~%>> " conts)
                                  (funcall (if (eq (read) 'yes)
                                               yes-fn
                                               no-fn))))
                            #'(lambda () conts))))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(setq n (compile-net 'people))" do
          let(:str) do
            <<~LISP
              > (setq n (compile-net 'people))
              #<Compiled-Function BF3C06>
              > (funcall n)
              Is the person a man?
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
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
              (setq a 1 b 2 c 3)
              `(a ,b c)
            LISP
          end
          it { is_expected.to eq [:a, 2, :c] }
        end

        context "`(a (,b c))" do
          let(:str) do
            <<~LISP
              (setq a 1 b 2 c 3)
              `(a (,b c))
            LISP
          end
          it { is_expected.to eq [:a, [2, :c]] }
        end

        context "`(1 ,(+ 1 2))" do
          let(:str) do
            <<~LISP
              `(1 ,(+ 1 2))
            LISP
          end
          it { is_expected.to eq [1, 3] }
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

        context "TODO: truncateが未実装。(defmacro nif (expr pos zero neg)  バッククォート版" do
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
          it { is_expected.to eq [:z, :p, :n] }
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
          it { is_expected.to eq [:z, :p, :n] }
        end

        context "(defmacro nif (expr pos zero neg)" do
          let(:str) do
            <<~LISP
              (defmacro nif (expr pos zero neg)
                `(case (truncate (signum ,expr))
                   (1 ,pos)
                   (0 ,zero)
                   (-1 ,neg)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro nif (expr pos zero neg)" do
          let(:str) do
            <<~LISP
              (defmacro nif (expr pos zero neg)
                (list 'case
                      (list 'truncate (list 'signum expr))
                      (list 1 pos)
                      (list 0 zero)
                      (list -1 neg)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "TODO: truncateが未実装。(case (truncate (signum x))" do
          let(:str) do
            <<~LISP
              (case (truncate (signum x))
                (1 'p)
                (0 'z)
                (-1 'n))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(setq b '(1 2 3))" do
          let(:str) do
            <<~LISP
              (setq b '(1 2 3)) ; => (1 2 3)
            LISP
          end
          it { is_expected.to eq [1, 2, 3] }
        end

        context '`(a ,b c)' do
          let(:str) do
            <<~LISP
              (setq b '(1 2 3)) ; => (1 2 3)
              `(a ,b c) ; => (A (1 2 3) C)
            LISP
          end
          it { is_expected.to eq [:a, [1, 2, 3], :c] }
        end

        context '`(a ,@b c)' do
          let(:str) do
            <<~LISP
              (setq b '(1 2 3)) ; => (1 2 3)
              `(a ,@b c) ; => (A 1 2 3 C)
            LISP
          end
          it { is_expected.to eq [:a, 1, 2, 3, :c] }
        end

        context "TODO: whenが未実装。(when (eligible obj)" do
          let(:str) do
            <<~LISP
              (when (eligible obj)
                (do-this)
                (do-that)
                obj)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "TODO: eligibleが未実装。(defmacro our-when (test &body body)" do
          let(:str) do
            <<~LISP
              (defmacro our-when (test &body body)
                `(if ,test
                     (progn
                       ,@body)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(if (eligible obj)" do
          let(:str) do
            <<~LISP
              (if (eligible obj)
                  (progn (do-this)
                         (do-that)
                         obj))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun greet (name)" do
          let(:str) do
            <<~LISP
              (defun greet (name)
                `(hello ,name))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context '単純なマクロの定義' do
        context "(member x choices :test #'eq)" do
          let(:str) do
            <<~LISP
              (member x choices :test #'eq)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(memq x choices)" do
          let(:str) do
            <<~LISP
              (memq x choices)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro memq (obj lst)" do
          let(:str) do
            <<~LISP
              (defmacro memq (obj lst)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro memq (obj lst) `(member" do
          let(:str) do
            <<~LISP
              (defmacro memq (obj lst)
                `(member
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro memq (obj lst) `(member ,obj" do
          let(:str) do
            <<~LISP
              (defmacro memq (obj lst)
                `(member ,obj
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro memq (obj lst) `(member ,obj ,lst :test #'eq))" do
          let(:str) do
            <<~LISP
              (defmacro memq (obj lst)
                `(member ,obj ,lst :test #'eq))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro while (test &body body)" do
          let(:str) do
            <<~LISP
              (defmacro while (test &body body)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro while (test &body body)" do
          let(:str) do
            <<~LISP
              (defmacro while (test &body body)
                `(do ()
                   ((not ,test))
                   ,@body))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context 'マクロ展開の確認' do
        context "(defmacro while (test &body body)" do
          let(:str) do
            <<~LISP
              (defmacro while (test &body body)
                `(do ()
                   ((not ,test))
                   ,@body)) ; => WHILE
              > (pprint (macroexpand '(while (able) (laugh))))
              (BLOCK NIL
                     (LET NIL
                          (TAGBODY
                            #:G61
                            (IF (NOT (ABLE)) (RETURN NIL))
                            (LAUGH)
                            (GO #:G61))))
              T
              > (pprint (macroexpand-1 '(while (able) (laugh))))
              (DO NIL
                  ((NOT (ABLE)))
                  (LAUGH))
              T
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro mac (expr)" do
          let(:str) do
            <<~LISP
              (defmacro mac (expr)
                `(pprint (macroexpand-1 ',expr)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(pprint (macroexpand-1 '(or x y)))" do
          let(:str) do
            <<~LISP
              (pprint (macroexpand-1 '(or x y)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(mac (or x y))" do
          let(:str) do
            <<~LISP
              (mac (or x y))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(setq exp (macroexpand-1 '(memq 'a '(a b c))))" do
          let(:str) do
            <<~LISP
              > (setq exp (macroexpand-1 '(memq 'a '(a b c))))
              (MEMBER (QUOTE A) (QUOTE (A B C)) :TEST (FUNCTION EQ))
              > (eval exp)
              (A B C)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context '引数リストの構造化代入' do
        context "(defun foo (x y z)" do
          let(:str) do
            <<~LISP
              (defun foo (x y z)
                (+ x y z))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(foo 1 2 3)" do
          let(:str) do
            <<~LISP
              (foo 1 2 3)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(destructuring-bind (x (y) . z) '(a (b) c d)" do
          let(:str) do
            <<~LISP
              > (destructuring-bind (x (y) . z) '(a (b) c d)
                  (list x y z))
              (A B (C D))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(dolist (x '(a b c))" do
          let(:str) do
            <<~LISP
              (dolist (x '(a b c))
                (print x))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro our-dolist ((var list &optional result) &body body)" do
          let(:str) do
            <<~LISP
              (defmacro our-dolist ((var list &optional result) &body body)
                `(progn
                   (mapc #'(lambda (,var) ,@body)
                         ,list)
                   (let ((,var nil))
                     ,result)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro when-bind ((var expr) &body body)" do
          let(:str) do
            <<~LISP
              (defmacro when-bind ((var expr) &body body)
                `(let ((,var ,expr))
                   (when ,var
                     ,@body)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(when-bind (input (get-user-input))" do
          let(:str) do
            <<~LISP
              (when-bind (input (get-user-input))
                         (process input))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(let ((input (get-user-input)))" do
          let(:str) do
            <<~LISP
              (let ((input (get-user-input)))
                (when input
                  (process input)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context 'マクロのモデル' do
        context "(defmacro our-expander (name) `(get ,name 'expander))" do
          let(:str) do
            <<~LISP
              (defmacro our-expander (name) `(get ,name 'expander))

              (defmacro our-defmacro (name parms &body body)
                (let ((g (gensym)))
                  `(progn
                     (setf (our-expander ',name)
                           #'(lambda (,g)
                               (block ,name
                                      (destructuring-bind ,parms (cdr ,g)
                                        ,@body))))
                     ',name)))

              (defun our-macroexpand-1 (expr)
                (if (and (consp expr) (our-expander (car expr)))
                    (funcall (our-expander (car expr)) expr)
                    expr))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(let ((op 'setq))" do
          let(:str) do
            <<~LISP
              (let ((op 'setq))
                (defmacro our-setq (var val)
                  (list op var val)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context 'プログラムとしてのマクロ' do
        context "(do ((w 3)" do
          let(:str) do
            <<~LISP
              (do ((w 3)
                   (x 1 (1+ x))
                   (y 2 (1+ y))
                   (z))
                ((> x 10) (princ z) y)
                (princ x)
                (princ y))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(prog ((w 3) (x 1) (y 2) (z nil))" do
          let(:str) do
            <<~LISP
              (prog ((w 3) (x 1) (y 2) (z nil))
                    foo
                    (if (> x 10)
                        (return (progn (princ z) y)))
                    (princ x)
                    (princ y)
                    (psetq x (1+ x) y (1+ y))
                    (go foo))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(let ((a 1))" do
          let(:str) do
            <<~LISP
              > (let ((a 1))
                  (setq a 2 b a)
                  (list a b))
              (2 2)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(let ((a 1))" do
          let(:str) do
            <<~LISP
              > (let ((a 1))
              (psetq a 2 b a)
              (list a b))
              (2 1)
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro our-do (bindforms (test &rest result) &body body)" do
          let(:str) do
            <<~LISP
              (defmacro our-do (bindforms (test &rest result) &body body)
                (let ((label (gensym)))
                  `(prog ,(make-initforms bindforms)
                         ,label
                         (if ,test
                             (return (progn ,@result)))
                         ,@body
                         (psetq ,@(make-stepforms bindforms))
                         (go ,label))))

              (defun make-initforms (bindforms)
                (mapcar #'(lambda (b)
                            (if (consp b)
                                (list (car b) (cadr b))
                                (list b nil)))
                        bindforms))

              (defun make-stepforms (bindforms)
                (mapcan #'(lambda (b)
                            (if (and (consp b) (third b))
                                (list (car b) (third b))
                                nil))
                        bindforms))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context 'マクロのスタイル' do
        context "(defmacro our-and (&rest args)" do
          let(:str) do
            <<~LISP
              (defmacro our-and (&rest args)
                (case (length args)
                  (0 t)
                  (1 (car args))
                  (t `(if ,(car args)
                          (our-and ,@(cdr args))))))

              (defmacro our-andb (&rest args)
                (if (null args)
                    t
                    (labels ((expander (rest)
                                       (if (cdr rest)
                                           `(if ,(car rest)
                                                ,(expander (cdr rest)))
                                           (car rest))))
                      (expander args))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context 'マクロへの依存' do
        context "(defmacro mac (x) `(1+ ,x))" do
          let(:str) do
            <<~LISP
              > (defmacro mac (x) `(1+ ,x))
              MAC
              > (setq fn (compile nil `(lambda (y) (mac y))))
              #<Compiled-Function BF7E7E>
              > (defmacro mac (x) `(+ ,x 100))
              MAC
              > (funcall fn 1)
              2
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context '関数からマクロへ' do
        context "(defun second (x) (cadr x))" do
          let(:str) do
            <<~LISP
              (defun second (x) (cadr x))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro second (x) `(cadr ,x))" do
          let(:str) do
            <<~LISP
              (defmacro second (x) `(cadr ,x))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun noisy-second (x)" do
          let(:str) do
            <<~LISP
              (defun noisy-second (x)
                (princ "Someone is taking a cadr!")
                (cadr x))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro noisy-second (x)" do
          let(:str) do
            <<~LISP
              (defmacro noisy-second (x)
                `(progn
                   (princ "Someone is taking a cadr!")
                   (cadr ,x)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun sum (&rest args)" do
          let(:str) do
            <<~LISP
              (defun sum (&rest args)
                (apply #'+ args))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro sum (&rest args)" do
          let(:str) do
            <<~LISP
              (defmacro sum (&rest args)
                `(apply #'+ (list ,@args)))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro sum (&rest args)" do
          let(:str) do
            <<~LISP
              (defmacro sum (&rest args)
                `(+ ,@args))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defun foo (x y z)" do
          let(:str) do
            <<~LISP
              (defun foo (x y z)
                (list x (let ((x y))
                          (list x z))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end

        context "(defmacro foo (x y z)" do
          let(:str) do
            <<~LISP
              (defmacro foo (x y z)
                `(list ,x (let ((x ,y))
                            (list x ,z))))
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end

      context 'シンボル・マクロ' do
        context "(symbol-macrolet ((hi (progn (print Howdy)" do
          let(:str) do
            <<~LISP
              > (symbol-macrolet ((hi (progn (print "Howdy")
                                             1)))
                                 (+ hi 2))
              "Howdy"
              3
            LISP
          end
          it { is_expected.to eq 99999999999 }
        end
      end
    end
  end
end
