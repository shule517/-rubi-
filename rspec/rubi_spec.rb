require_relative '../rubi.rb'

describe 'rubi.rb' do
  describe '#split_tokens' do
    subject { split_tokens(str) }

    context '足し算' do
      let(:str) { "(+ 1 2)" }
      it { is_expected.to eq [:"(", :"+", 1, 2, :")"] }
    end

    context '二重かっこ' do
      let(:str) { "(defun double (x) (* x 2))" }
      it { is_expected.to eq [:"(", :defun, :double, :"(", :x, :")", :"(", :*, :x, 2, :")", :")"] }
    end
  end

  describe '#parse_lisp' do
    subject { parse_lisp(split_tokens(str)) }

    context '()*1' do
      let(:str) { "(+ 1 2)" }
      it { is_expected.to eq [:+, 1, 2] }
    end

    context '()*2' do
      let(:str) { "(+ (+ 1 2) 3)" }
      it { is_expected.to eq [:+, [:+, 1, 2], 3] }
    end

    context '二重かっこ' do
      let(:str) { "(defun double (x) (* x 2))" }
      it { is_expected.to eq [:defun, :double, [:x], [:*, :x, 2]] }
    end
  end

  describe '#eval_lisp' do
    subject { eval_lisp(parse_lisp(split_tokens(str))) }

    context '足し算' do
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
    end

    context '引き算' do
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
    end
  end
end
