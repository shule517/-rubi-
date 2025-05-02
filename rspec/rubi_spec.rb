require_relative '../rubi.rb'

describe '#eval_lisp' do
  subject { eval_lisp(parse_lisp(split_tokens(str))) }

  context 'define' do
    let(:str) do
      <<~LISP
        (define x 3)
        x
      LISP
    end
    it { is_expected.to eq 3 }
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
  end
end
