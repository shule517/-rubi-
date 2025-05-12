require_relative '../rubi.rb'

describe Rubi::Tokenizer do
  describe '#split_tokens' do
    subject { Rubi::Tokenizer.new.split_tokens(str) }

    context '足し算' do
      let(:str) { "(+ 1 2)" }
      it { is_expected.to eq [:"(", :"+", 1, 2, :")"] }
    end

    context '二重かっこ' do
      let(:str) { "(defun double (x) (* x 2))" }
      it { is_expected.to eq [:"(", :defun, :double, :"(", :x, :")", :"(", :*, :x, 2, :")", :")"] }
    end

    context '複数行' do
      let(:str) do
        <<~LISP
          (define x 3)
          (+ x 4)
        LISP
      end
      it { is_expected.to eq [:"(", :define, :x, 3, :")", :"(", :+, :x, 4, :")"] }
    end

    context 'quote 1文字' do
      let(:str) do
        <<~LISP
          'a
        LISP
      end
      it { is_expected.to eq [:"'", :a] }
    end

    context 'quote list' do
      let(:str) do
        <<~LISP
          '(1 2 a)
        LISP
      end
      it { is_expected.to eq [:"'", :"(", 1, 2, :a, :")"] }
    end

    context 'コメントアウト' do
      let(:str) do
        <<~LISP
          '(1 2 a) ; コメント
        LISP
      end
      it { is_expected.to eq [:"'", :"(", 1, 2, :a, :")"] }
    end

    context "`,の変数展開①" do
      let(:str) { "`(1 ,(+ 1 2))" }
      it { is_expected.to eq [:`, :"(", 1, :",", :"(", :+, 1, 2, :")", :")"] }
    end

    context "`,の変数展開②" do
      let(:str) { "`(a (,b c))" }
      it { is_expected.to eq [:`, :"(", :a, :"(", :",", :b, :c, :")", :")"] }
    end
  end
end
