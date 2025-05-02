require_relative '../rubi.rb'

describe '#parse_lisp' do
  subject { rubi.parse_lisp(rubi.split_tokens(str)) }
  let(:rubi) { Rubi.new }

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
