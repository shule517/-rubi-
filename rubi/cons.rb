module Rubi
  class Cons
    attr_reader :car, :cdr

    def initialize(car:, cdr:)
      @car = car
      @cdr = cdr
    end

    def list?(a)
      a.is_a?(Array)
    end

    def atom?(a)
      !list?(a)
    end

    def inspect
      if cdr.nil?
        [car]
      elsif list?(cdr)
        [car] + cdr
      elsif atom?(car) && atom?(cdr)
        "(#{car} . #{cdr})"
      elsif list?(car) && atom?(cdr)
        "((#{car.join(' ')}) . #{cdr})"
      end
    end
  end
end
