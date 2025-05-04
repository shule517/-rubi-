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
      raise "スタック多すぎ問題" if stack_count > 100
      nest = "  " * (stack_count + 1)
      puts "#{nest}eval(ast: #{ast}, lexical_hash: #{lexical_hash})"
      if atom?(ast)
        return eval_atom(ast, lexical_hash, nest)
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
        puts "#{nest}#{function}(var_name: #{var_name}, value: #{value})"
        var_hash[var_name] = eval(value, {}, stack_count + 1)
      elsif function == :lambda
        params, expression = ast
        puts "#{nest}#{function}(params: #{params}, expression: #{expression})"
        Proc.new { eval(expression, lexical_hash, stack_count + 1) } # TODO: lambdaが引数に対応していない
      elsif function == :defun # 関数定義
        func_name = ast.shift
        params, expression = ast
        puts "#{nest}#{function}(params: #{params}, expression: #{expression})"
        @func_hash[func_name] = Proc.new { eval(expression, lexical_hash, stack_count + 1) }
        # @func_hash[func_name] = lambda { eval(expression) }
        puts "#{nest}func_hash: #{@func_hash}"
        func_name # 定義した関数名のシンボルを返す
      elsif function == :list
        puts "#{nest}#{function}(params: #{ast}, lexical_hash: #{lexical_hash})"
        ast.map { |a| eval(a, lexical_hash, stack_count + 1) }
      elsif function == :quote
        puts "#{nest}#{function}(params: #{ast}, lexical_hash: #{lexical_hash})"
        ast[0] # quoteは評価しない
      elsif function == :funcall
        puts "#{nest}#{function}(params: #{ast}, lexical_hash: #{lexical_hash})"
        array = ast.map { |a| eval(a, lexical_hash, stack_count + 1) }
        eval(array, lexical_hash, stack_count + 1)
      # elsif function == :defmacro
      #   puts "#{nest}#{function}(params: #{ast}, lexical_hash: #{lexical_hash})"
      elsif function == :+
        puts "#{nest}#{function}(params: #{ast}, lexical_hash: #{lexical_hash})"
        ast.map { |a| eval(a, lexical_hash, stack_count + 1) }.sum
      elsif function == :-
        puts "#{nest}#{function}(params: #{ast}, lexical_hash: #{lexical_hash})"
        ast.map { |a| eval(a, lexical_hash, stack_count + 1) }.reduce(:-)
      elsif function == :*
        puts "#{nest}#{function}(params: #{ast}, lexical_hash: #{lexical_hash})"
        ast.map { |a| eval(a, lexical_hash, stack_count + 1) }.reduce(:*)
      elsif function == :/
        puts "#{nest}#{function}(params: #{ast}, lexical_hash: #{lexical_hash})"
        ast.map { |a| eval(a, lexical_hash, stack_count + 1) }.reduce(:/)
      elsif function.instance_of?(Array) # lambdaの実行
        puts "#{nest}式を評価後に戻り値を関数として実行します(function: #{function}, (params: #{ast}, lexical_hash: #{lexical_hash})"
        expression = eval(function, lexical_hash, stack_count + 1)
        expression.call(4) # TODO: 引数ありに対応していない
      else
        if func_hash.key?(function)
          puts "#{nest}#{function}関数が見つかった(params: #{ast}, lexical_hash: #{lexical_hash})"
          func = func_hash[function]
          puts "#{nest}func_hash[function]: #{func}"
          puts "#{nest}params: #{ast}"
          return func.call(*ast) # 変数を参照する
        else
          puts "#{nest}TODO: else -> #{function}(params: #{ast}, lexical_hash: #{lexical_hash})"
        end
      end
    end

    private

    def eval_atom(ast, lexical_hash, nest)
      if lexical_hash.key?(ast)
        puts "#{nest}-> ローカル変数を返す #{lexical_hash[ast]}"
        lexical_hash[ast] # レキシカルスコープの変数を参照する
      elsif var_hash.key?(ast)
        puts "#{nest}-> グローバル変数を返す #{var_hash[ast]}"
        var_hash[ast] # グローバル変数を参照する
      else
        puts "#{nest}-> シンボルor値を返す #{ast}"
        ast # シンボルor値を返す
      end
    end
  end
end
