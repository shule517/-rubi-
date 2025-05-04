module Rubi
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
      puts "----"
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
      # else
      #   func_name = ast.shift
      #   puts func_name: func_name
      #   pp func_hash: func_hash
      #   if func_hash.key?(func_name)
      #     puts "func_hash[func_name]: #{func_hash[func_name]}"
      #     pp params: ast
      #     return func_hash[func_name].call(ast) # 変数を参照する
      #   end
      end

      pp ast: ast
      function = ast.shift
      pp function: function

      if function == :setq # 変数定義
        var_name, value = ast
        var_hash[var_name] = eval(value)
      elsif function == :defun # 関数定義
        puts "-- defun --"
        pp defun_ast: ast
        func_name = ast.shift
        params, expression = ast
        pp params: params, expression: expression
        @func_hash[func_name] = lambda { eval(expression) }
        pp func_hash: @func_hash
        func_name # 定義した関数名のシンボルを返す
      elsif function == :+
        ast.map { |a| eval(a) }.sum
      elsif function == :-
        ast.map { |a| eval(a) }.reduce(:-)
      elsif function == :*
        ast.map { |a| eval(a) }.reduce(:*)
      elsif function == :/
        ast.map { |a| eval(a) }.reduce(:/)
      else
        puts func_name: function
        pp func_hash: func_hash
        if func_hash.key?(function)
          func = func_hash[function]
          puts "func_hash[function]: #{func}"
          pp params: ast
          return func.call(*ast) # 変数を参照する
        end
      end
    end
  end
end
