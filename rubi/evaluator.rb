module Rubi
  class Evaluator
    attr_reader :var_hash, :func_hash, :macro_hash, :built_system_func, :debug

    def initialize(debug: false)
      @var_hash = {}
      @func_hash = {}
      @macro_hash = {}
      @built_system_func = false
      @debug = debug
    end

    def build_system_func(stack_count, nest)
      func_hash[:+] = Proc.new do |proc_params:, lexical_hash:|
        if proc_params.empty?
          0
        else
          proc_params.map { |param| eval(param, lexical_hash, stack_count + 1) }.reduce(:+)
        end
      end

      func_hash[:append] = Proc.new do |proc_params:, lexical_hash:|
        eval_params = proc_params.map { |param| eval(param, lexical_hash, stack_count + 1) }.reject(&:nil?)

        head_lists = eval_params[0...-1]
        last = eval_params[-1]

        if eval_params.all? { |eval_param| list?(eval_param) }
          eval_params.reduce(:+)
        elsif atom?(last)
          Rubi::Cons.new(car: head_lists.reduce(:+), cdr: last)
        else
          raise "atomが先頭の場合は実行できません。(eval_params: #{eval_params})"
        end
      end

      func_hash[:mapcar] = Proc.new do |proc_params:, lexical_hash:|
        # TODO: Lispのcarで実装した方がよいのでは？
        puts "#{nest}mapcar(proc_params: #{proc_params}, lexical_hash: #{lexical_hash})"
        a, b, c = proc_params
        if c.nil?
          # 引数が2つの場合
          # (mapcar
          #   #'(lambda (x) (+ x 10))
          #   '(1 2 3))
          proc = eval(a, lexical_hash, stack_count + 1)
          array = eval(b, lexical_hash, stack_count + 1)
          array.map do |element|
            puts "#{nest}mapcar(element: #{element}, lexical_hash: #{lexical_hash})"
            proc.call(proc_params: [element], lexical_hash: lexical_hash.dup)
          end
        else
          # 引数が3つの場合
          # (mapcar #'+
          #   '(1 2 3)
          #   '(10 100 1000))
          proc = eval(a, lexical_hash, stack_count + 1)
          array1 = eval(b, lexical_hash, stack_count + 1)
          array2 = eval(c, lexical_hash, stack_count + 1)
          array1.map.with_index do |element, index|
            puts "#{nest}mapcar(element: #{element}, lexical_hash: #{lexical_hash})"
            param1 = element
            param2 = array2[index]
            proc.call(proc_params: [param1, param2], lexical_hash: lexical_hash.dup)
          end
        end
      end

      # システム関数を登録済み
      @built_system_func = true
    end

    def atom?(ast)
      !list?(ast)
    end

    def list?(ast)
      ast.is_a?(Array)
    end

    def puts(*args)
      Kernel::puts(args) if debug
    end

    def eval(ast, lexical_hash, stack_count)
      raise "スタック多すぎ問題" if stack_count > 100
      nest = "  " * (stack_count + 1)
      build_system_func(stack_count + 1, nest) unless built_system_func
      puts "#{nest}eval(ast: #{ast}, lexical_hash: #{lexical_hash})"
      if atom?(ast)
        return eval_atom(ast, lexical_hash, nest)
      elsif ast == []
        return nil # ()はnilになる
      end

      # リストのため、関数呼び出しをする
      function = ast.shift
      params = ast

      if function == :let
        var_params = params.shift
        expression = params
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
        var_name, value = params
        puts "#{nest}#{function}(var_name: #{var_name}, value: #{value})"
        if lexical_hash.key?(var_name)
          # ローカル変数がある場合は、ローカル変数を変更する
          lexical_hash[var_name] = value
        else
          # ローカル変数がない場合は、グローバル変数を定義する
          var_hash[var_name] = eval(value, lexical_hash, stack_count + 1)
          puts "#{nest}-> var_hash: #{var_hash}"
          var_hash[var_name]
        end
      elsif function == :setf # TODO: 未実装。setqをコピーしただけ
        var_name, value = params
        puts "#{nest}#{function}(var_name: #{var_name}, value: #{value})"
        if lexical_hash.key?(var_name)
          # ローカル変数がある場合は、ローカル変数を変更する
          lexical_hash[var_name] = value
        else
          # ローカル変数がない場合は、グローバル変数を定義する
          var_hash[var_name] = eval(value, lexical_hash, stack_count + 1)
          puts "#{nest}-> var_hash: #{var_hash}"
          var_hash[var_name]
        end
      elsif function == :lambda
        params, expression = params
        puts "#{nest}#{function}(params: #{params}, expression: #{expression})"
        # 関数定義
        build_lambda(params, expression, stack_count, nest)
      elsif function == :defun # 関数定義
        func_name = params.shift
        params, expression = params
        puts "#{nest}#{function}(params: #{params}, expression: #{expression})"
        # 関数定義
        func_hash[func_name] = build_lambda(params, expression, stack_count, nest)
        puts "#{nest}func_hash: #{@func_hash}"
        func_name # 定義した関数名のシンボルを返す
      elsif function == :function
        puts "#{nest}#{function}(params: #{params})"
        a = params.shift
        if a.instance_of?(Array)
          expression = a
          puts "#{nest}1. 式を評価して、関数を取り出す(expression: #{expression})"
          eval(expression, lexical_hash, stack_count + 1)
        else
          func_name = a
          puts "#{nest}1. シンボルから関数を取り出す(func_name: #{func_name})"
          func_hash[func_name]
        end
      elsif function == :"symbol-function"
        puts "#{nest}#{function}(params: #{params})"
        # TODO: functionとの違いがよく分かってない。参照できるスコープの違いなどがあるのか？
        a = params.shift
        if a.instance_of?(Array)
          expression = a
          puts "#{nest}1. 式を評価して、シンボルを取り出す(expression: #{expression})"
          func_name = eval(expression, lexical_hash, stack_count + 1)
          func_hash[func_name]
        elsif a.instance_of?(Symbol)
          func_name = a
          puts "#{nest}1. シンボルから関数を取り出す(func_name: #{func_name})"
          func_hash[func_name]
        end
      elsif function == :"symbol-value"
        puts "#{nest}#{function}(params: #{params})"
        expression = params.shift
        var_name = eval(expression, lexical_hash, stack_count + 1)
        var_hash[var_name]
      elsif function == :list
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        params.map { |a| eval(a, lexical_hash, stack_count + 1) }
      elsif function == :car
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        result = eval(params.first, lexical_hash, stack_count + 1)
        result.first
      elsif function == :cdr
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        result = eval(params.first, lexical_hash, stack_count + 1)
        result[1..]
      elsif function == :cons
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a = eval(params[0], lexical_hash, stack_count + 1)
        b = eval(params[1], lexical_hash, stack_count + 1)
        if b.nil?
          [a] # 末がnilのものは、配列
        elsif atom?(b)
          Cons.new(car: a, cdr: b) # 末がnilじゃないものは、cons
        elsif atom?(a) && list?(b)
          [a] + b
        elsif list?(a) && list?(b)
          a + b
        end
      elsif function == :null
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        result = eval(params.first, lexical_hash, stack_count + 1)
        true if result.empty? # falseの場合は、nilを返す
      elsif function == :atom
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a = params.shift
        puts "#{nest}1. 評価する(a: #{a})"
        atom = eval(a, lexical_hash, stack_count + 1)
        true if atom?(atom) # listの場合は、nilを返す
      elsif function == :quote
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        params[0] # quoteは評価しない
      elsif function == :funcall
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        array = params.map { |a| puts "#{nest}(a: #{a})";result = eval(a, lexical_hash, stack_count + 1);puts "#{nest}-> #{result}";result }
        puts "#{nest}#{function}(array: #{array})"
        eval(array, lexical_hash, stack_count + 1)
      elsif function == :defmacro
        macro_name = params.shift
        params, expression = params
        puts "#{nest}#{function}(macro_name: #{macro_name}, params: #{params}, expression: #{expression}, lexical_hash: #{lexical_hash})"
        # マクロ定義
        macro_hash[macro_name] = build_macro(params, expression, lexical_hash, stack_count, nest)
        puts "#{nest}-> macro_hash: #{macro_hash}"
        macro_name
      elsif function == :-
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        params.map { |a| eval(a, lexical_hash, stack_count + 1) }.reduce(:-)
      elsif function == :*
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        params.map { |a| eval(a, lexical_hash, stack_count + 1) }.reduce(:*)
      elsif function == :/
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        params.map { |a| eval(a, lexical_hash, stack_count + 1) }.reduce(:/)
      elsif function == :eq
        # オブジェクトが一致しているか。ポインタが一致しているか。
        # Lispのeq は Rubyのequal? とほぼ一致する

        # 数値
        # (eq 1 1) # => true
        # (setq x 1)(eq x 1) # => true

        # 文字列
        # (eq "あ" "あ") # => nil
        # (setq x "あ")(eq x "あ") # => nil

        # 配列
        # (eq '(1 2) '(1 2)) #=> nil

        # 同じ変数
        # (setq x "あ")(eq x x) # => true
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a, b = params.map { |a| eval(a, lexical_hash, stack_count + 1) }
        true if a.equal?(b) # 一致しない場合は、nilを返す
      elsif function == :eql
        # オブジェクトが一致しているか。ポインタが一致しているか。
        # Lispのeq は Rubyのequal? とほぼ一致する

        # 数値
        # (eql 1 1) # => true
        # (setq x 1)(eql x 1) # => true

        # 文字列
        # (eql "あ" "あ") # => nil
        # (setq x "あ")(eql x "あ") # => nil

        # 配列
        # (eql '(1 2) '(1 2)) #=> nil

        # 同じ変数
        # (setq x "あ")(eql x x) # => true
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a, b = params.map { |a| eval(a, lexical_hash, stack_count + 1) }
        true if a.equal?(b) # 一致しない場合は、nilを返す
      elsif function == :equal
        # 値が一致しているか
        # Lispのequal は Rubyのeql? とほぼ一致する

        # 数値
        # (equal 1 1) # => true
        # (setq x 1)(equal x 1) # => true

        # 文字列
        # (equal "あ" "あ") # => true
        # (setq x "あ")(equal x "あ") # => true

        # 配列
        # (equal '(1 2) '(1 2)) #=> true

        # 同じ変数
        # (setq x "あ")(equal x x) # => true
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a, b = params.map { |a| eval(a, lexical_hash, stack_count + 1) }
        true if a.eql?(b) # 一致しない場合は、nilを返す
      elsif function == :"="
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a, b = params
        true if a == b # 不一致の場合は、nilを返す
      elsif function == :"/="
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a, b = params
        true if a != b # 不一致の場合は、nilを返す
      elsif function == :"<"
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a, b = params
        true if a < b # 不一致の場合は、nilを返す
      elsif function == :">"
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a, b = params
        true if a > b # 不一致の場合は、nilを返す
      elsif function == :"<="
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a, b = params
        true if a <= b # 不一致の場合は、nilを返す
      elsif function == :">="
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a, b = params
        true if a >= b # 不一致の場合は、nilを返す
      elsif function == :not
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a = params.shift
        condition = eval(a, lexical_hash, stack_count + 1)
        true if condition.nil? # nil以外の場合は、nilを返す
      elsif function == :and
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        true if params.all? { |param| eval(param, lexical_hash, stack_count + 1) }
      elsif function == :or
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        true if params.any? { |param| eval(param, lexical_hash, stack_count + 1) }
      elsif function == :if
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a, b, c = params
        condition = eval(a, lexical_hash, stack_count + 1)
        puts "#{nest}#(condition: #{condition}, b: #{b}, c: #{c})"
        if condition
          eval(b, lexical_hash, stack_count + 1)
        else
          eval(c, lexical_hash, stack_count + 1)
        end
      elsif function == :cond
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        # (cond (t 1) (t 2) (t 3))
        # (cond ((= 1 1) (+ 1 1)) ((= 2 2) (+ 2 2)) ((= 3 3) (+ 3 3)))
        _cond, expression = params.find { |cond, _expression| eval(cond, lexical_hash, stack_count + 1) }
        eval(expression, lexical_hash, stack_count + 1)
      elsif function == :progn
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        params.map { |param| eval(param, lexical_hash, stack_count + 1) }.last
      elsif function == :apply
        puts "#{nest}#{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        a = params.shift
        puts "#{nest}(a: #{a}, params: #{params})"
        puts "#{nest}1. funcを評価する(a: #{a})"
        func = eval(a, lexical_hash, stack_count + 1)
        puts "#{nest}-> (func: #{func})"
        puts "#{nest}2. proc_paramsを評価して、リストにまとめる(params: #{params})"
        # (apply #'+ 1 '(2))
        # ↓と等価。 applyの仕様として、第3引数以降のatomをリスト化して、最後の引数のリストと結合する。
        # (apply #'+ (append '(1) '(2)))
        eval_params = params.map { |param| Array(eval(param, lexical_hash, stack_count + 1)) }.reduce(&:+)
        puts "#{nest}-> (eval_params: #{eval_params})"
        func.call(proc_params: eval_params, lexical_hash: lexical_hash)
      elsif function.instance_of?(Array) # lambdaを実行する
        # 関数を返す式 を評価して、実行する
        # 例: ((lambda (x) (* 2 x)) 3) → (関数 3)
        # まずは第１引数を評価してから、実行する
        puts "#{nest}関数の実行(function: #{function}, (params: #{params}, lexical_hash: #{lexical_hash})"
        expression = eval(function, lexical_hash, stack_count + 1)
        puts "#{nest}関数の実行:#{function}(expression: #{expression})"
        puts "#{nest}関数の実行:#{function}(params: #{params})"
        expression.call(proc_params: params, lexical_hash: lexical_hash)
      elsif function.instance_of?(Proc) # funcallで関数を実行する
        puts "#{nest}関数の実行(function: #{function}, (params: #{params}, lexical_hash: #{lexical_hash})"
        function.call(proc_params: params, lexical_hash: lexical_hash)
      elsif func_hash.key?(function) # defunで登録した関数を実行する
        # 登録されている関数を呼び出す
        puts "#{nest}#{function}関数が見つかった(params: #{params}, lexical_hash: #{lexical_hash})"
        func = func_hash[function]
        puts "#{nest}func_hash[function]: #{func}"
        puts "#{nest}params: #{params}"
        func.call(proc_params: params, lexical_hash: lexical_hash)
      elsif macro_hash.key?(function)
        # 登録されているマクロを呼び出す
        puts "#{nest}#{function}マクロが見つかった(params: #{params}, lexical_hash: #{lexical_hash})"
        expanded = macro_hash[function].call(*params)
        puts "#{nest}マクロで展開された式を実行する(expanded: #{expanded}, lexical_hash: #{lexical_hash})"
        eval(expanded, lexical_hash, stack_count + 1)
      else
        puts "#{nest}TODO: else -> #{function}(params: #{params}, lexical_hash: #{lexical_hash})"
        raise "対応する関数(#{function})が見つかりません(params: #{params})"
      end
    end

    private

    # 関数定義
    def build_lambda(params, expression, stack_count, nest)
      Proc.new do |proc_params:, lexical_hash:|
        raise "proc_paramsは配列のみです！" unless proc_params.is_a?(Array)
        puts "#{nest}lambdaの中(params: #{params}, proc_params: #{proc_params}, expression: #{expression}, lexical_hash: #{lexical_hash})"
        puts "#{nest}1. lexical_hashに変数を展開していく(params: #{params}, proc_params: #{proc_params})"
        params.each.with_index do |param, index|
          puts "#{nest}1.1 変数を展開するために、評価する(index: #{index}, proc_params: #{proc_params}, proc_params[index]: #{proc_params[index]}, lexical_hash: #{lexical_hash})"
          lexical_hash[param] = eval(proc_params[index], lexical_hash, stack_count + 1)
        end
        puts "#{nest}-> lexical_hash: #{lexical_hash}"
        puts "#{nest}2. lambdaを実行する(expression: #{expression}, lexical_hash: #{lexical_hash})"
        # 関数を実行すると、expressionが書き換わってしまうので、expression.dup
        eval(expression.dup, lexical_hash, stack_count + 1)
      end
    end

    # マクロ定義
    def build_macro(params, expression, lexical_hash, stack_count, nest)
      Proc.new do |*proc_params|
        puts "#{nest}macroの中(params: #{params}, proc_params: #{proc_params}, expression: #{expression}, lexical_hash: #{lexical_hash})"
        puts "#{nest}1. lexical_hashに変数を展開していく(params: #{params}, proc_params: #{proc_params})"
        params.each.with_index do |param, index|
          # 変数を評価せずに展開する
          # 例:
          # (defmacro nil! (var) (list 'setq var nil))
          # (nil! x)
          # この時は、lexical_hash: {:var=>:x}) -> varをxに置き換えて実行する
          lexical_hash[param] = proc_params[index]
        end
        puts "#{nest}-> lexical_hash: #{lexical_hash}"
        puts "#{nest}2. macroを実行する(expression: #{expression}, lexical_hash: #{lexical_hash})"
        expanded = eval(expression, lexical_hash, stack_count + 1)
        puts "#{nest}-> 展開した式: #{expanded}"
        expanded
      end
    end

    def eval_atom(ast, lexical_hash, nest)
      if lexical_hash.key?(ast)
        puts "#{nest}-> ローカル変数を返す #{lexical_hash[ast]}"
        lexical_hash[ast] # レキシカルスコープの変数を参照する
      elsif var_hash.key?(ast)
        puts "#{nest}-> グローバル変数を返す #{var_hash[ast]}"
        var_hash[ast] # グローバル変数を参照する
      elsif ast == :t
        true
      elsif ast == :nil
        nil
      elsif ast.is_a?(Symbol)
        str = ast.to_s
        if str.start_with?('"') && str.end_with?('"')
          # 文字列の場合 例: "あ"
          return str[1...-1]
        end
        raise "#{ast}の値を評価できません"
      else
        puts "#{nest}-> 値を返す #{ast}"
        ast # シンボルor値を返す
      end
    end
  end
end
