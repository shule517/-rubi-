require_relative '../rubi.rb'

describe Rubi::Evaluator do
  describe '#eval' do
    subject { ast.map { |code| pp code: code; evaluator.eval(code) }.last }

    let(:evaluator) { Rubi::Evaluator.new }
    let(:ast) { Rubi::Parser.new.parse(tokens) }
    let(:tokens) { Rubi::Tokenizer.new.split_tokens(str) }

    context 'let' do
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

      # context 'グローバル変数 と ローカル変数で 同じ名前の変数を定義する' do
      #   let(:str) do
      #     <<~LISP
      #       (setq x 3)
      #       (let
      #         ((x 2) (y 3))
      #         (+ x y))
      #       x
      #     LISP
      #   end
      #   it { is_expected.to eq 3 }
      # end
    end
    context 'setq' do
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

    context 'defun' do
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

      # context '関数を定義して実行する' do
      #   let(:str) do
      #     <<~LISP
      #       (defun double (x) (* x 2))
      #       (double 1)
      #     LISP
      #   end
      #   it { is_expected.to eq 2 }
      # end

    #   context '関数を定義して実行する' do
    #     let(:str) do
    #       <<~LISP
    #         (defun square (x) (* x x))
    #         (square 3)
    #       LISP
    #     end
    #     it { is_expected.to eq 9 }
    #   end
    end

    context '値' do
      context '整数' do
        let(:str) { "1" }
        it { is_expected.to eq 1 }
      end

      context '小数' do
        let(:str) { "1.1" }
        it { is_expected.to eq 1.1 }
      end

      context '文字列' do
        let(:str) { "あいうえお" }
        it { is_expected.to eq :"あいうえお" }
      end
    end

    context '+' do
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

    context '-' do
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

    context '*' do
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

    context '/' do
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
  end
end
