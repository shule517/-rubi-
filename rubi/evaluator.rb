module Rubi
  class Evaluator
    attr_reader :var_hash, :func_hash, :macro_hash, :built_system_func, :debug, :tokenizer, :parser

    def initialize(debug: false)
      @var_hash = {}
      @func_hash = {}
      @macro_hash = {}
      @built_system_func = false
      @tokenizer = Rubi::Tokenizer.new
      @parser = Rubi::Parser.new
      @debug = debug
    end

    def build_system_func(stack_count, nest)
      # システム関数を登録済み(このメソッドを1回しか実行しない)
      @built_system_func = true

      puts "#### ↓ 初期化 ↓ ####"
      %i(+ - * / < > <= >=).each do |operator|
        func_hash[operator] = Proc.new do |proc_params:, lexical_hash:, stack_count:, nest:|
          puts "#{nest}#{operator}(proc_params: #{proc_params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
          if proc_params.empty?
            0
          else
            result = proc_params.map { |param| eval(param, lexical_hash, stack_count + 1) }.reduce(operator)
            result = nil if result.instance_of?(FalseClass)
            puts "#{nest}(#{operator} #{proc_params}) -> #{result}"
            result
          end
        end
      end

      func_hash[:mod] = Proc.new do |proc_params:, lexical_hash:, stack_count:, nest:|
        puts "#{nest}mod(proc_params: #{proc_params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        a, b = proc_params.map { |a| eval(a, lexical_hash, stack_count + 1) }
        a % b
      end

      func_hash[:expt] = Proc.new do |proc_params:, lexical_hash:, stack_count:, nest:|
        puts "#{nest}expt(proc_params: #{proc_params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        a, b = proc_params.map { |a| eval(a, lexical_hash, stack_count + 1) }
        a ** b
      end

      func_hash[:cons] = Proc.new do |proc_params:, lexical_hash:, stack_count:, nest:|
        puts "#{nest}cons(proc_params: #{proc_params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        a = eval(proc_params[0], lexical_hash, stack_count + 1)
        b = eval(proc_params[1], lexical_hash, stack_count + 1)
        if b.nil?
          [a] # 末がnilのものは、配列
        elsif atom?(b)
          Cons.new(car: a, cdr: b) # 末がnilじゃないものは、cons
        elsif atom?(a) && list?(b)
          [a] + b
        elsif list?(a) && list?(b)
          a + b
        end
      end

      func_hash[:consp] = Proc.new do |proc_params:, lexical_hash:, stack_count:, nest:|
        puts "#{nest}push(proc_params: #{proc_params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        value = proc_params.map { |a| eval(a, lexical_hash, stack_count + 1) }.first
        true if value.is_a?(Rubi::Cons) ||value.is_a?(Array) # ArrayはConsの集合です
      end

      func_hash[:append] = Proc.new do |proc_params:, lexical_hash:, stack_count:, nest:|
        puts "#{nest}append(proc_params: #{proc_params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        # TODO: Lispで実装した方がよいのでは？
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

      func_hash[:push] = Proc.new do |proc_params:, lexical_hash:, stack_count:, nest:|
        puts "#{nest}push(proc_params: #{proc_params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        newvalue, array = proc_params.map { |a| eval(a, lexical_hash, stack_count + 1) }
        array.unshift(newvalue)
      end

      func_hash[:mapcar] = Proc.new do |proc_params:, lexical_hash:, stack_count:, nest:|
        # TODO: Lispのcarで実装した方がよいのでは？
        puts "#{nest}mapcar(proc_params: #{proc_params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        a, b, c = proc_params
        if c.nil?
          puts "#{nest}引数が2つの場合(proc_params: #{proc_params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
          # 引数が2つの場合
          # (mapcar
          #   #'(lambda (x) (+ x 10))
          #     '(1 2 3))
          puts "#{nest}1. 関数を取り出す(a: #{a}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
          proc = eval(a, lexical_hash, stack_count + 1)
          if proc.is_a?(Symbol)
            proc = func_hash[proc]
          end
          raise "procがnil。(a: #{a})を評価して、procが戻ってこなかった" if proc.nil?
          puts "#{nest}2. 引数を評価する(b: #{b}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
          array = eval(b, lexical_hash, stack_count + 1)
          array.map.with_index do |element, index|
            puts "#{nest}3. #{index+1}回目のループ(element: #{element}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
            proc.call(proc_params: [element], lexical_hash: lexical_hash, stack_count: stack_count, nest: nest)
          end
        else
          puts "#{nest}引数が3つの場合(proc_params: #{proc_params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
          # 引数が3つの場合
          # (mapcar #'+
          #   '(1 2 3)
          #   '(10 100 1000))
          proc = eval(a, lexical_hash, stack_count + 1)
          array1 = eval(b, lexical_hash, stack_count + 1)
          array2 = eval(c, lexical_hash, stack_count + 1)
          array1.map.with_index do |element, index|
            puts "#{nest}mapcar(element: #{element}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
            param1 = element
            param2 = array2[index]
            proc.call(proc_params: [param1, param2], lexical_hash: lexical_hash, stack_count: stack_count, nest: nest)
          end
        end
      end

      func_hash[:"copy-tree"] = Proc.new do |proc_params:, lexical_hash:, stack_count:, nest:|
        puts "#{nest}copy-tree(proc_params: #{proc_params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        a = proc_params.shift
        # aがquoteを含む式なら評価する（例：[:quote, [[1, 2], [3, 4]]]）
        if a.is_a?(Array) && a.first == :quote
          value = eval(a, lexical_hash, stack_count + 1)
        elsif a.is_a?(Symbol)
          value = eval(a, lexical_hash, stack_count + 1)
        else
          value = a
        end
        Marshal.load(Marshal.dump(value))
      end

      # TODO: 遅くなってる。高速化したい。
      # 組み込み関数の定義
      lisp_eval("(defun evenp (x) (= (mod x 2) 0))")
      lisp_eval("(defun 1+ (x) (+ x 1))")
      lisp_eval("(defun zerop (n) (= n 0))")
      lisp_eval(<<~LISP)
        (defun length (lst)
          (if (null lst)
            0
            (1+ (length (cdr lst)))
          )
        )
      LISP
      lisp_eval(<<~LISP)
        (defun remove-if (func lst)
          (if (null lst)
            nil
            (if (evenp (car lst))
              (remove-if func (cdr lst))
              (cons (car lst) (remove-if func (cdr lst)))
            )
          )
        )
      LISP

      # incf
      lisp_eval(<<~LISP)
        (defmacro incf (x)
          (list 'setq x (list '+ x 1)))
      LISP
      # TODO: ほんとは`,を使いたい
      # (defmacro incf(x)
      # `(setq ,x (+ ,x 1)))

      # nth
      lisp_eval(<<~LISP)
        (defun nth (index lst)
          (let ((result lst))
            (dotimes (n index (car result))
              (setq result (cdr result)))))
      LISP
      lisp_eval("(defun first (lst) (nth 0 lst)")
      lisp_eval("(defun second (lst) (nth 1 lst)")
      lisp_eval("(defun third (lst) (nth 2 lst)")
      lisp_eval("(defun fourth (lst) (nth 3 lst)")

      # TODO: 要リファクタリング。sortをLispで実装してみた。
      # lisp_eval(<<~LISP)
      #   (defun private-sort (lst func)
      #     (let* (
      #         (a (car lst))
      #         (b (car (cdr lst)))
      #       )
      #       (if (null b) lst
      #         (if (funcall func a b)
      #           (append (list a) (private-sort (cdr lst) func))
      #           (append (private-sort (cdr lst) func) (list a))
      #         )
      #       )
      #     )
      #   )
      #
      #   (defun sort (lst func)
      #     (let ((result lst))
      #       (dotimes (x (length lst))
      #         (setq result (private-sort result func))
      #       )
      #       result
      #     )
      #   )
      # LISP

      # TODO: compileを仮実装
      lisp_eval("(defun compile (x) nil)")
      lisp_eval("(defun compiled-function-p (x) nil)")
      lisp_eval("(defun proclaim (x) nil)")

      puts "#### ↑ 初期化 ↑ ####"
    end

    def lisp_eval(line)
      tokens = tokenizer.split_tokens(line)
      ast = parser.parse(tokens)
      ast = parser.expand_syntactic_sugar(ast)
      ast.map { |code| eval(code, {}, 0) }.last
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

    def eval(arg_ast, lexical_hash, stack_count)
      ast = arg_ast.dup # evalに渡した引数を変更しない
      raise "スタック多すぎ問題" if stack_count > 100
      nest = "  " * (stack_count + 1)
      build_system_func(stack_count + 1, nest) unless built_system_func
      puts "#{nest}eval(ast: #{ast}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
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
        expressions = params
        puts "#{nest}#{function}(var_params: #{var_params}, expressions: #{expressions})"
        # レキシカル変数(next_lexical_hash)を定義する
        puts "#{nest}#レキシカル変数を定義する"

        # letをネストした時に、1階層目に2階層目の処理内容を反映させないように「.dup」する
        # (let ((x 1))
        #   (let ((x 2))
        #     x) ; => 2
        # x) ; => 1
        # 1. 元の環境で評価する
        values = var_params.map { |var_name, value| eval(value, lexical_hash, stack_count + 1) }
        # 2. 新しい環境を作る
        next_lexical_hash = lexical_hash.dup
        var_params.each.with_index do |(var_name, value), index|
          next_lexical_hash[var_name] = values[index]
        end
        puts "#{nest}-> next_lexical_hash(object_id: #{next_lexical_hash.object_id}): #{next_lexical_hash}"
        puts "#{nest}#式を評価する - expressions: #{expressions}"
        expressions.map.with_index do |expression, index|
          puts "#{nest}#{index + 1}ループ目(expression: #{expression}, next_lexical_hash(object_id: #{next_lexical_hash.object_id}): #{next_lexical_hash})"
          result = eval(expression, next_lexical_hash, stack_count + 1)
          puts "#{nest}-> #{result}(next_lexical_hash(object_id: #{next_lexical_hash.object_id}): #{next_lexical_hash})"
          result
        end.last
      elsif function == :'let*' # TODO: letをコピーしただけ
        var_params = params.shift
        expression = params
        puts "#{nest}#{function}(var_params: #{var_params}, expression: #{expression})"
        # レキシカル変数(next_lexical_hash)を定義する
        puts "#{nest}#レキシカル変数を定義する"
        next_lexical_hash = lexical_hash.dup
        var_params.each do |var_name, value|
          next_lexical_hash[var_name] = eval(value, next_lexical_hash, stack_count + 1)
        end
        puts "#{nest}-> next_lexical_hash(object_id: #{next_lexical_hash.object_id}): #{next_lexical_hash}"
        puts "#{nest}#式を評価する - expression: #{expression}"
        expression.map { |e| eval(e, next_lexical_hash, stack_count + 1) }.last
      elsif function == :labels
        # (labels ((double (x) (* 2 x)))
        #   (double 3))
        func_data, labels_expression = params
        func_name, func_params, func_expression = func_data[0]
        puts "#{nest}#{function}(params: #{params}, func_expression: #{func_expression})"

        # ローカル関数を定義する
        func = build_lambda(func_params, func_expression, lexical_hash, func_name)
        lexical_hash[func_name] = func # ローカル変数に関数を登録する

        # 式を実行する
        eval(labels_expression, lexical_hash, stack_count + 1)
      elsif function == :setq # 変数定義
        # (setq a 1 b 2 c 3)
        params.each_slice(2).map do |var_name, value|
          puts "#{nest}#{function}(var_name: #{var_name}, value: #{value})"
          puts "#{nest}式を展開する(#{var_name} = #{value}) => lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash}"
          newvalue = eval(value, lexical_hash, stack_count + 1)

          if lexical_hash.key?(var_name)
            # ローカル変数がある場合は、ローカル変数を変更する
            puts "#{nest}ローカル変数を見つけた(#{var_name}) => lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash}"
            lexical_hash[var_name] = newvalue
            puts "#{nest}ローカル変数がある場合は、ローカル変数を変更する(#{var_name} = #{newvalue}) => lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash}"
            lexical_hash[var_name]
          else
            # ローカル変数がない場合は、グローバル変数を定義する
            var_hash[var_name] = newvalue
            puts "#{nest}ローカル変数がない場合は、グローバル変数を定義する(#{var_name} = #{newvalue}(object_id: #{var_hash[var_name].object_id})) => var_hash: #{var_hash}"
            puts "#{nest}-> var_hash: #{var_hash}"
            var_hash[var_name]
          end
        end.last
      elsif function == :get
        puts "#{nest}#{function}(params: #{params})"
        hash_keys = params.map { |param| eval(param, lexical_hash, stack_count + 1) }
        puts "#{nest}(hash_keys: #{hash_keys}, var_hash: #{var_hash})"
        var_hash.dig(*hash_keys)
      elsif function == :setf
        place, newvalue = params
        puts "#{nest}#{function}(place: #{place}, newvalue: #{newvalue})"
        if list?(place)
          place_func, *place_params = place
          if place_func == :'symbol-function'
            # (setf (symbol-function 'double)
            #   #'(lambda (x) (* x 2)))

            place_param = place_params[0]
            puts "#{nest}place_func: #{place_func}, place_param: #{place_param}, newvalue: #{newvalue}"
            func_name = eval(place_param, lexical_hash, stack_count + 1)
            func = eval(newvalue, lexical_hash, stack_count + 1)
            func_hash[func_name] = func
            func # 定義した関数を返す
          elsif place_func == :get
            # (setf (get 'color 'shade) 'dark)
            # (get 'color 'shade) ; => 'dark
            puts "#{nest}place_func: #{place_func}, place_params: #{place_params}, newvalue: #{newvalue}"
            hash_keys = place_params.map { |place_param| eval(place_param, lexical_hash, stack_count + 1) }
            # TODO: いったん2段固定
            if hash_keys.size == 2
              a, b = hash_keys
              var_hash[a] ||= {}
              var_hash[a][b] = eval(newvalue, lexical_hash, stack_count + 1)
              puts "#{nest}-> var_hash: #{var_hash}"
              var_hash[a][b]
            else
              raise "setf getの設定はキー2段階にしか対応してないです！"
            end
          end
        elsif lexical_hash.key?(place)
          # ローカル変数がある場合は、ローカル変数を変更する
          lexical_hash[place] = newvalue
        else
          # ローカル変数がない場合は、グローバル変数を定義する
          var_hash[place] = eval(newvalue, lexical_hash, stack_count + 1)
          puts "#{nest}-> var_hash: #{var_hash}"
          var_hash[place]
        end
      elsif function == :lambda
        params, *expressions = params
        puts "#{nest}#{function}(params: #{params}, expressions: #{expressions}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        # 関数定義
        expression = expressions.last # TODO: 途中の式を実行していない
        build_lambda(params, expression, lexical_hash, "lambdaで定義した関数")
      elsif function == :defun # 関数定義
        func_name = params.shift
        params, expression = params
        puts "#{nest}#{function}(params: #{params}, expression: #{expression})"
        # 関数定義
        func_hash[func_name] = build_lambda(params, expression, lexical_hash, func_name)
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
          puts "#{nest}1. シンボルから関数を取り出す(func_name: #{func_name}, lexical_hash: #{lexical_hash})"
          if func_hash.key?(func_name)
            func_hash[func_name] # グローバルの定義を参照する
          elsif lexical_hash.key?(func_name)
            lexical_hash[func_name] # ローカル変数を参照する
          else
            raise "#{func_name}の関数が見つかりません(func_hash: #{func_hash}, lexical_hash: #{lexical_hash})"
          end
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
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        result = params.map { |a| eval(a, lexical_hash, stack_count + 1) }
        puts "#{nest}-> #{result}"
        result
      elsif function == :car
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        result = eval(params.first, lexical_hash, stack_count + 1)
        puts "#{nest}-> result: #{result}"
        car(result)
      elsif function == :cdr
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        result = eval(params.first, lexical_hash, stack_count + 1)
        puts "#{nest}result: #{result} params: #{params}"
        cdr(result)
      elsif function == :assoc
        # (setq alist '((a . 1) (b . 2) (c . 3)))
        # (assoc 'b alist) ; => (b . 2)
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        a, b = params
        eval_a = eval(a, lexical_hash, stack_count + 1)
        eval_b = eval(b, lexical_hash, stack_count + 1)
        puts "#{nest}(eval_a: #{eval_a}, eval_b: #{eval_b}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        eval_b.find { |c| puts "#{nest}ループ -> c: #{c}"; car(c) == eval_a }
      elsif function == :sort
        a, func = params
        list = eval(a, lexical_hash, stack_count + 1)
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        list.sort # TODO: <しか対応してない funcを参照していない
      elsif function == :null
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        result = eval(params.first, lexical_hash, stack_count + 1)
        true if result.nil? || result.empty? # falseの場合は、nilを返す
      elsif function == :atom
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        a = params.shift
        puts "#{nest}1. 評価する(a: #{a})"
        atom = eval(a, lexical_hash, stack_count + 1)
        true if atom?(atom) # listの場合は、nilを返す
      elsif function == :quote
        expressions = params[0] # quoteは評価しない
        puts "#{nest}#{function}(expressions: #{expressions}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        if expressions[1] == :"."
          raise "引数が多いです#{expressions}" if expressions.size != 3
          cons = Rubi::Cons.new(car: expressions[0], cdr: expressions[2])
          puts "#{nest}-> #{cons}"
          cons
        else
          puts "#{nest}-> expressions: #{expressions}"
          expressions # 評価しない
        end

        # TODO:
        # expressions.map do |expression|
        #   if list?(expression)
        #     if expression[0] == :unquote
        #       puts "#{nest}expression[0] == :unquote -> (expression: #{expression}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        #       eval(expression, lexical_hash, stack_count + 1) # 評価する
        #     else
        #       puts "#{nest}expression[0] != :unquote -> (expression: #{expression}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        #       expression
        #     end
        #   else
        #     puts "#{nest}atom?(expression) -> (expression: #{expression}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        #     expression # 評価しない
        #   end
        # end
      elsif function == :unquote
        expressions = params[0]
        puts "#{nest}#{function}(expressions: #{expressions}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        eval(expressions, lexical_hash, stack_count + 1)
      elsif function == :funcall
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        array = params
        array[0] = eval(params[0], lexical_hash, stack_count + 1) # 関数だけ評価する。引数は評価しない。
        puts "#{nest}#{function}(array: #{array})"
        eval(array, lexical_hash, stack_count + 1)
      elsif function == :defmacro
        macro_name = params.shift
        params, expression = params
        puts "#{nest}#{function}(macro_name: #{macro_name}, params: #{params}, expression: #{expression}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        # マクロ定義
        macro_hash[macro_name] = build_macro(params, expression, lexical_hash, stack_count, nest)
        puts "#{nest}-> macro_hash: #{macro_hash}"
        macro_name
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
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
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
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
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
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        a, b = params.map { |a| eval(a, lexical_hash, stack_count + 1) }
        true if a.eql?(b) # 一致しない場合は、nilを返す
      elsif function == :"="
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        a, b = params.map { |a| eval(a, lexical_hash, stack_count + 1) }
        puts "#{nest}#{a} == #{b} -> #{a == b}"
        true if a == b # 不一致の場合は、nilを返す
      elsif function == :"/="
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        a, b = params.map { |a| eval(a, lexical_hash, stack_count + 1) }
        true if a != b # 不一致の場合は、nilを返す
      elsif function == :not
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        a = params.shift
        condition = eval(a, lexical_hash, stack_count + 1)
        true if condition.nil? # nil以外の場合は、nilを返す
      elsif function == :and
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        true if params.all? { |param| eval(param, lexical_hash, stack_count + 1) }
      elsif function == :or
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        true if params.any? { |param| eval(param, lexical_hash, stack_count + 1) }
      elsif function == :if
        condition, b, c = params
        puts "#{nest}#{function}(condition: #{condition}, trueの式: #{b}, falseの式: #{c}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        eval_condition = eval(condition, lexical_hash, stack_count + 1)
        puts "#{nest}(eval_condition: #{eval_condition != nil}, trueの式: #{b}, falseの式: #{c})"
        if eval_condition
          puts "#{nest}(trueの式を採用: #{b})"
          result = eval(b, lexical_hash, stack_count + 1)
          puts "#{nest}-> #{result}"
          result
        else
          puts "#{nest}#{function}(falseの式を採用: #{c})"
          result = eval(c, lexical_hash, stack_count + 1)
          puts "#{nest}-> #{result}"
          result
        end
      elsif function == :cond
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        # (cond (t 1) (t 2) (t 3))
        # (cond ((= 1 1) (+ 1 1)) ((= 2 2) (+ 2 2)) ((= 3 3) (+ 3 3)))
        _cond, expression = params.find { |cond, _expression| eval(cond, lexical_hash, stack_count + 1) }
        eval(expression, lexical_hash, stack_count + 1)
      elsif function == :case
        # (setq x 2)
        # (case x
        #   (1 'dog)
        #   (2 'cat)
        #   (otherwise 'human)) ; -> 'cat
        var = eval(params.shift, lexical_hash, stack_count + 1)
        puts "#{nest}#{function}(var: #{var}, params: #{params.map.with_index { |param, index| "params[#{index}] => #{param}"}.join(", ")}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        params.each do |value, *expressions|
          puts "#{nest}(#{var} == #{value})"
          # elseに到着
          if value == :otherwise
            # TODO: 最後の式しか実行していない。本当は途中の式も実行する必要がある。
            return eval(expressions.last, lexical_hash, stack_count + 1)
          end
          # else以外
          # eval_value = eval(value, lexical_hash, stack_count + 1)
          if var == value
            puts "#{nest}(#{var} == #{value}){expressions: #{expressions}}"
            # TODO: 最後の式しか実行していない。本当は途中の式も実行する必要がある。
            return eval(expressions.last, lexical_hash, stack_count + 1)
          end
        end
        nil # 何もHITしなかった場合の戻り値
      elsif function == :dotimes
        a, b = params
        puts "#{nest}#{function}(a: #{a}, b: #{b} lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        var, number, result = a
        puts "#{nest}1. ループ回数を評価する(number: #{number}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        number = eval(number, lexical_hash.dup, stack_count + 1)
        number.times.with_index do |index|
          lexical_hash[var] = index
          puts "#{nest}2. #{index + 1}ループ目(b: #{b}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
          eval(b, lexical_hash, stack_count + 1)
        end
        lexical_hash[var] = number # dotimesはループ終わったがタイミングで、変数 = ループ回数 になっている仕様っぽい
        eval(result, lexical_hash, stack_count + 1)
      elsif function == :progn
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        params.map { |param| eval(param, lexical_hash, stack_count + 1) }.last
      elsif function == :apply
        puts "#{nest}#{function}(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
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
        func.call(proc_params: eval_params, lexical_hash: lexical_hash, stack_count: stack_count + 1, nest: nest)
      elsif function.instance_of?(Array) # lambdaを実行する
        # 関数を返す式 を評価して、実行する
        # 例: ((lambda (x) (* 2 x)) 3) → (関数 3)
        # まずは第１引数を評価してから、実行する
        expression = function
        puts "#{nest}関数の実行(expression: #{expression}, (params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        proc = eval(expression, lexical_hash, stack_count + 1)
        puts "#{nest}関数の実行:#{expression}(proc: #{proc}, params: #{params})"
        proc.call(proc_params: params, lexical_hash: lexical_hash, stack_count: stack_count, nest: nest)
      elsif function.instance_of?(Proc) # funcallで関数を実行する
        puts "#{nest}関数の実行(function: #{function}, (params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        function.call(proc_params: params, lexical_hash: lexical_hash, stack_count: stack_count, nest: nest)
      elsif func_hash.key?(function) # defunで登録した関数を実行する
        # 登録されている関数を呼び出す
        puts "#{nest}#{function}関数が見つかった(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        func = func_hash[function]
        puts "#{nest}func_hash[function]: #{func}"
        puts "#{nest}params: #{params}"
        func.call(proc_params: params, lexical_hash: lexical_hash, stack_count: stack_count, nest: nest)
      elsif macro_hash.key?(function)
        # 登録されているマクロを呼び出す
        puts "#{nest}#{function}マクロが見つかった(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        expanded = macro_hash[function].call(proc_params: params, lexical_hash: lexical_hash, stack_count: stack_count + 1, nest: nest)
        puts "#{nest}マクロで展開された式を実行する(expanded: #{expanded}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        eval(expanded, lexical_hash, stack_count + 1)
      elsif lexical_hash.key?(function)
        # 登録されている関数を呼び出す
        puts "#{nest}ローカル関数(#{function})が見つかった(params: #{params}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        lexical_hash[function].call(proc_params: params, lexical_hash: lexical_hash, stack_count: stack_count, nest: nest)
      else
        puts "#{nest}'#{function}'は、関数ではありません(params: #{params} lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        raise "'#{function}'は、関数ではありません(params: #{params})"
      end
    end

    private

    def car(x)
      if x.is_a?(Rubi::Cons)
        x.car
      elsif x.is_a?(Array) # [a, b] => a / [a . b] => a
        x.first
      end
    end

    def cdr(x)
      if x.nil?
        puts "-> nil"
        nil
      elsif x.is_a?(Rubi::Cons)
        puts "-> #{x.cdr} ※consの場合"
        x.cdr
      elsif x[1] == :"."
        puts "-> #{x[2]} ※a . b形式の場合" # TODO: consに変換して対応できないのか？
        x[2]
      else
        puts "-> #{x[1..]} ※配列の場合"
        x[1..]
      end
    end

    # 関数定義
    def build_lambda(params, expression, build_lexical_hash, func_name)
      # クロージャ：環境(レキシカルスコープ)をキャプチャした関数オブジェクトのこと

      # 例)
      # (defun make-adder (n)
      #   (lambda (x) (+ x n)))
      #
      # (setq add2 (make-adder 2))   ; n=2 をキャプチャ
      # (setq add10 (make-adder 10)) ; n=10 をキャプチャ
      #
      # (funcall add2 5) ; => 7
      # (funcall add10 5) ; => 15

      captured_lexical_env = build_lexical_hash.dup # 関数オブジェクトを生成したタイミングの環境を保持する（キャプチャする）
      # 他のクロージャと変数が共有されてしまうのを防ぐために、環境を複製する
      # 例)
      # add2のnは2
      # add10のnは10

      Proc.new do |proc_params:, lexical_hash:, stack_count:, nest:|
        # 1. build_lexical_hash は、setqで変数を変更して、引き継がないといけない → build_lexical_hashはdupしちゃいけない
        # local_lexical_hash = build_lexical_hash # build_lexical_hashはdupしちゃいけない
        # local_lexical_hash = local_lexical_hash.merge(lexical_hash) # 関数の引数を引き継ぐ(関数の引数: lexical_hash, 関数の関数の引数: build_lexical_hash)

        # local_lexical_hash = build_lexical_hash

        # 引数の値だけを新しくスコープに追加（上書き）
        local_lexical_hash = captured_lexical_env#.dup

        raise "proc_paramsは配列のみです！" unless proc_params.is_a?(Array)
        puts "#{nest}lambda(#{func_name})の中(expression: #{expression}, params: #{params}, proc_params: #{proc_params}, expression: #{expression}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"

        # オプショナル引数を分ける
        optional_index = params.index(:"&optional")
        if optional_index
          normal_params = params[0...optional_index]
          optional_params = params[optional_index+1..]
        else
          normal_params = params
        end

        # 通常引数の対応
        puts "#{nest}1. local_lexical_hashに通常引数を展開していく(normal_params: #{normal_params}, proc_params: #{proc_params}, local_lexical_hash(object_id: #{local_lexical_hash.object_id}): #{local_lexical_hash})"
        normal_params.each.with_index do |param, index|
          puts "#{nest}  1.1(#{index}ループ目) 変数を展開するために、評価する(param: #{param}, proc_params[index]: #{proc_params[index]}, local_lexical_hash(object_id: #{local_lexical_hash.object_id}): #{local_lexical_hash})"
          local_lexical_hash[param] = eval(proc_params[index], local_lexical_hash, stack_count + 2)
        end
        puts "#{nest}-> local_lexical_hash(object_id: #{local_lexical_hash.object_id}): #{local_lexical_hash}"

        # オプショナル引数の対応
        puts "#{nest}2. local_lexical_hashにオプショナル引数を展開していく(optional_params: #{optional_params}, proc_params: #{proc_params}, local_lexical_hash(object_id: #{local_lexical_hash.object_id}): #{local_lexical_hash})"
        Array(optional_params).each.with_index do |param, index|
          proc_params_index = index + normal_params.size
          value = proc_params[proc_params_index]
          var_name, default_value = param
          puts "#{nest}オプショナル引数(#{optional_params})を設定する(var_name: #{var_name}, value: #{value}, default_value: #{default_value})"
          if value
            puts "#{nest}  2.1 変数を展開するために、評価する local_lexical_hash[#{var_name}] = (value: #{value}, local_lexical_hash(object_id: #{local_lexical_hash.object_id}): #{local_lexical_hash})"
            local_lexical_hash[var_name] = eval(value, local_lexical_hash, stack_count + 2)
          else
            puts "#{nest}  2.2 引数の指定がないため、デフォルト値を採用する local_lexical_hash[#{var_name}] = (value: #{value}, local_lexical_hash(object_id: #{local_lexical_hash.object_id}): #{local_lexical_hash})"
            local_lexical_hash[var_name] = default_value
          end
        end

        puts "#{nest}-> local_lexical_hash(object_id: #{local_lexical_hash.object_id}): #{local_lexical_hash}"
        puts "#{nest}3. lambdaを実行する(expression: #{expression}, local_lexical_hash(object_id: #{local_lexical_hash.object_id}): #{local_lexical_hash})"
        result = eval(expression, local_lexical_hash, stack_count + 1)
        puts "#{nest}-> #{result}"
        result
      end
    end

    # マクロ定義
    def build_macro(params, expression, lexical_hash, stack_count, nest)
      Proc.new do |proc_params:, lexical_hash:, stack_count:, nest:|
        puts "#{nest}macroの中(params: #{params}, proc_params: #{proc_params}, expression: #{expression}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        puts "#{nest}1. lexical_hashに変数を展開していく(params: #{params}, proc_params: #{proc_params})"
        params.each.with_index do |param, index|
          # 変数を評価せずに展開する
          # 例:
          # (defmacro nil! (var) (list 'setq var nil))
          # (nil! x)
          # この時は、lexical_hash: {:var=>:x}) -> varをxに置き換えて実行する
          lexical_hash[param] = proc_params[index]
        end
        puts "#{nest}-> lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash}"
        puts "#{nest}2. macroを実行する(expression: #{expression}, lexical_hash(object_id: #{lexical_hash.object_id}): #{lexical_hash})"
        expanded = eval(expression, lexical_hash, stack_count + 1)
        puts "#{nest}-> 展開した式: #{expanded}"
        expanded
      end
    end

    def eval_atom(ast, lexical_hash, nest)
      if lexical_hash.key?(ast)
        puts "#{nest}-> ローカル変数を返す(#{ast}) #{lexical_hash[ast]}"
        lexical_hash[ast] # レキシカルスコープの変数を参照する
      elsif var_hash.key?(ast)
        puts "#{nest}-> グローバル変数を返す(#{ast}) #{var_hash[ast]}(object_id: #{var_hash[ast].object_id})"
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
        elsif str.start_with?(':')
          return ast # シンボルとして返す
        end
        raise "#{ast}の値を評価できません"
      else
        puts "#{nest}-> 値を返す #{ast}"
        ast # シンボルor値を返す
      end
    end

    # astを(+ 1 2)表記に変換する
    def expression_to_s(ast)
      ast.to_s.gsub("[", "(").gsub("]", ")").gsub(":", "").gsub(",", "")
    end
  end
end
