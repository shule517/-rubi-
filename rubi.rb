class Rubi
  attr_reader :hash

  def initialize
    @hash = {}
  end

  def split_tokens(str)
    str
      .gsub("(", " ( ")
      .gsub(")", " ) ")
      .split
      .map { |c| Integer(c) rescue c.to_sym }
  end

  def parse_lisp(tokens)
    list = []

    until tokens.empty? do
      token = tokens.shift

      if ![:"(", :")"].include?(token)
        list << token # ()の中を追加していく
      elsif token == :")"
        return list # )で終了
      elsif token == :"("
        result = parse_lisp(tokens) # 次の()へ
        list << result # ()の中身を追加
      end
    end

    list
  end

  def eval_lisp(ast)
    if ast.is_a?(Symbol)
      pp "#{ast} : #{hash[ast]}"
      return hash[ast]
    end

    pp ast: ast
    function = ast.shift
    pp function: function

    if function == :define
      puts "--- define ---"
      a = ast.shift
      b = ast.shift
      hash[a] = b
    elsif function == :+
      puts "--- + ---"
      pp params: ast
      ast.map do |a|
        Integer(a) rescue eval_lisp(a)
        # if a.instance_of?(Array)
        #   eval_lisp(a)
        # else
        #   a
        # end
      end.sum
    elsif function == :-
      puts "--- - ---"
      a = ast.shift
      if a.instance_of?(Array)
        a = eval_lisp(a)
      end
      ast.each do |b|
        b = if b.instance_of?(Array)
          eval_lisp(b)
        else
          b
            end
        a -= b
      end
      a
    else
      a = ast.shift
      hash[a]
    end
  end
end
