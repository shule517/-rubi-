require_relative '../rubi.rb'

describe Rubi::Cons do
  describe '#inspect' do
    subject { Rubi::Cons.new(car: a, cdr: b).inspect }

    context 'car: atom, cdr: atom' do
      let(:a) { 1 }
      let(:b) { 2 }
      it { is_expected.to eq "(1 . 2)" }
    end

    context 'car: atom, cdr: list' do
      let(:a) { 1 }
      let(:b) { [2, 3] }
      it { is_expected.to eq [1, 2, 3] }
    end

    context 'car: atom, cdr: nil' do
      let(:a) { 1 }
      let(:b) { nil }
      it { is_expected.to eq [1] }
    end

    context 'car: list, cdr: atom' do
      let(:a) { [1, 2] }
      let(:b) { 3 }
      it { is_expected.to eq "((1 2) . 3)" }
    end

    context 'car: list, cdr: list' do
      let(:a) { [1, 2] }
      let(:b) { [3, 4] }
      it { is_expected.to eq [[1, 2], 3, 4] }
    end
  end
end
