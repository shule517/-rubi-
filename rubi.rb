class Rubi
  attr_reader :hash, :func_hash

  def initialize
    @hash = {}
    @func_hash = {}
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
      if a.instance_of?(Array)
        # 関数定義
        function_name = a[0]
        x = a[1]
        formula = ast.shift
        func_hash[function_name] = { x => formula }
        pp func_hash: func_hash
      else
        # 変数定義
        b = ast.shift
        hash[a] = b
      end
    elsif function == :+
      puts "--- + ---"
      pp params: ast
      ast.map do |a|
        Integer(a) rescue eval_lisp(a)
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
    elsif function == :*
      puts "--- * ---"
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
        a *= b
      end
      a
    elsif function == :/
      puts "--- / ---"
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
        a /= b
      end
      a
    end
  end
end
