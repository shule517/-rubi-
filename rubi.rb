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
      .map { |c| atom(c) }
  end

  def atom(c)
    Integer(c) rescue Float(c) rescue c.to_sym
  end

  def atom?(ast)
    !ast.is_a?(Array)
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
      # 変数を参照
      pp "#{ast} : #{hash[ast]}"

      if hash.key?(ast)
        return hash[ast]
      else
        return ast
      end
    elsif atom?(ast)
      # 値を返す
      return ast
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
      ast.map { |a| eval_lisp(a) }.sum
    elsif function == :-
      puts "--- - ---"
      ast.map { |a| eval_lisp(a) }.reduce(:-)
    elsif function == :*
      puts "--- * ---"
      ast.map { |a| eval_lisp(a) }.reduce(:*)
    elsif function == :/
      puts "--- / ---"
      ast.map { |a| eval_lisp(a) }.reduce(:/)
    end
  end
end
