require_relative '../rubi.rb'

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
