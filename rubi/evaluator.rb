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

    def eval(ast, lexical_hash, stack_count)
      nest = "  " * (stack_count + 1)
      # puts "#{nest}--- eval ---"
      puts "#{nest}eval(ast: #{ast}, lexical_hash: #{lexical_hash})"
      if ast.is_a?(Symbol)
        # puts "#{nest}var_hash: #{var_hash}"
        if lexical_hash.key?(ast)
          puts "#{nest}-> ローカル変数を返す #{lexical_hash[ast]}"
          return lexical_hash[ast] # レキシカルスコープの変数を参照する
        elsif var_hash.key?(ast)
          puts "#{nest}-> グローバル変数を返す #{var_hash[ast]}"
          return var_hash[ast] # グローバル変数を参照する
        else
          puts "#{nest}-> シンボルを返す #{ast}"
          return ast # シンボルを返す
        end
      elsif atom?(ast)
        puts "#{nest}-> 値を返す #{ast}"
        return ast # 値を返す
      end

      function = ast.shift
      # puts "#{nest}--- #{function} ---"

      if function == :let
        var_params = ast.shift
        expression = ast
        puts "#{nest}#{function}(var_params: #{var_params}, expression: #{expression})"
        # レキシカル変数(next_lexical_hash)を定義する
        puts "#{nest}#レキシカル変数を定義する"
        next_lexical_hash = lexical_hash.dup
        var_params.each do |var_name, value|
          next_lexical_hash[var_name] = eval(value, next_lexical_hash, stack_count + 1)
        end
        puts "#{nest}-> next_lexical_hash: #{next_lexical_hash}"
        puts "#{nest}#式を評価する - expression: #{expression}"
        expression.map { |e| eval(e, next_lexical_hash, stack_count + 1) }.last
      elsif function == :setq # 変数定義
        var_name, value = ast
        var_hash[var_name] = eval(value, {}, stack_count + 1)
      elsif function == :defun # 関数定義
        puts "#{nest}defun_ast: #{ast}"
        func_name = ast.shift
        params, expression = ast
        puts "#{nest}params: #{params}, expression: #{expression}"
        @func_hash[func_name] = Proc.new { eval(expression, lexical_hash, stack_count + 1) }
        # @func_hash[func_name] = lambda { eval(expression) }
        puts "#{nest}func_hash: #{@func_hash}"
        func_name # 定義した関数名のシンボルを返す
      elsif function == :+
        # pp ast_map: ast.map { |a| eval(a, lexical_hash, stack_count + 1) }
        ast.map { |a| eval(a, lexical_hash, stack_count + 1) }.sum
      elsif function == :-
        ast.map { |a| eval(a, lexical_hash, stack_count + 1) }.reduce(:-)
      elsif function == :*
        ast.map { |a| eval(a, lexical_hash, stack_count + 1) }.reduce(:*)
      elsif function == :/
        ast.map { |a| eval(a, lexical_hash, stack_count + 1) }.reduce(:/)
      else
        puts "#{nest}func_hash: #{func_hash}"
        if func_hash.key?(function)
          func = func_hash[function]
          puts "#{nest}func_hash[function]: #{func}"
          puts "#{nest}params: #{ast}"
          return func.call(*ast) # 変数を参照する
        end
      end
    end
  end
end
