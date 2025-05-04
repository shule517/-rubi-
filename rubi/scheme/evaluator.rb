module Rubi::Scheme
  class Evaluator
    attr_reader :var_hash, :func_hash

    def initialize
      @var_hash = {}
      @func_hash = {}
    end

    def atom?(ast)
      !ast.is_a?(Array)
    end

    def eval(ast)
      if ast.is_a?(Symbol)
        pp var_hash: var_hash
        if var_hash.key?(ast)
          puts "var_hash[ast]: #{var_hash[ast]}"
          return var_hash[ast] # 変数を参照する
        else
          puts "ast: #{ast}"
          return ast # シンボルを返す
        end
      elsif atom?(ast)
        return ast # 値を返す
      end

      pp ast: ast
      function = ast.shift
      pp function: function

      if function == :define
        puts "--- define ---"
        a = ast.shift
        if a.instance_of?(Array)
          # 関数定義
          puts "関数定義"
          function_name = a[0]
          x = a[1]
          formula = ast.shift
          func_hash[function_name] = { x => formula }
          pp func_hash: func_hash
        else
          # 変数定義
          puts "変数定義"
          b = ast.shift
          puts "var_hash[#{a}] = #{b}"
          var_hash[a] = b
        end
      elsif function == :+
        ast.map { |a| eval(a) }.sum
      elsif function == :-
        ast.map { |a| eval(a) }.reduce(:-)
      elsif function == :*
        ast.map { |a| eval(a) }.reduce(:*)
      elsif function == :/
        ast.map { |a| eval(a) }.reduce(:/)
      end
    end
  end
end
