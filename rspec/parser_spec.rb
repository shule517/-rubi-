require_relative '../rubi.rb'

describe Rubi::Parser do
  describe '#parse' do
    subject { Rubi::Parser.new.parse(tokens) }
    let(:tokens) { Rubi::Tokenizer.new.split_tokens(str) }

    context '()*1' do
      let(:str) { "(+ 1 2)" }
      it { is_expected.to eq [[:+, 1, 2]] }
    end

    context '()*2' do
      let(:str) { "(+ (+ 1 2) 3)" }
      it { is_expected.to eq [[:+, [:+, 1, 2], 3]] }
    end

    context '二重かっこ' do
      let(:str) { "(defun double (x) (* x 2))" }
      it { is_expected.to eq [[:defun, :double, [:x], [:*, :x, 2]]] }
    end

    context '複数行' do
      let(:str) do
        <<~LISP
            (define x 3)
            (+ x 4)
        LISP
      end
      it { is_expected.to eq [[:define, :x, 3], [:+, :x, 4]] }
    end
  end

  describe '#expand_syntactic_sugar' do
    subject { pp ast: ast; parser.expand_syntactic_sugar(ast) }

    let(:parser) { Rubi::Parser.new }
    let(:ast) { parser.parse(tokens) }
    let(:tokens) { Rubi::Tokenizer.new.split_tokens(str) }

    context 'quoteの糖衣構文' do
      context "'の場合" do
        context 'シンボルの場合' do
          let(:str) { "'a" }
          before { expect(ast).to eq [:"'", :a] }
          it { is_expected.to eq [[:quote, :a]] }
        end

        context '記号の場合' do
          let(:str) { "(funcall '+ 1 2)" }
          before { expect(ast).to eq [[:funcall, :"'", :+, 1, 2]] }
          it { is_expected.to eq [[:funcall, [:quote, :+], 1, 2]] }
        end

        context 'リストの場合' do
          let(:str) { "'(1 2 a)" }
          before { expect(ast).to eq [:"'", [1, 2, :a]] }
          it { is_expected.to eq [[:quote, [1, 2, :a]]] }
        end
      end

      context "`の場合" do
        context 'シンボルの場合' do
          let(:str) { "`a" }
          before { expect(ast).to eq [:`, :a] }
          it { is_expected.to eq [[:quote, :a]] }
        end

        context '記号の場合' do
          let(:str) { "(funcall `+ 1 2)" }
          before { expect(ast).to eq [[:funcall, :`, :+, 1, 2]] }
          it { is_expected.to eq [[:funcall, [:quote, :+], 1, 2]] }
        end

        context 'リストの場合' do
          let(:str) { "`(1 2 a)" }
          before { expect(ast).to eq [:"`", [1, 2, :a]] }
          it { is_expected.to eq [[:quote, [1, 2, :a]]] }
        end
      end

      context "#'の場合" do
        let(:str) { "#'double" }
        before { expect(ast).to eq [:"#'", :double] }
        it { is_expected.to eq [[:function, :double]] }
      end
    end
  end
end
