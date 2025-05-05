module Rubi
  class Parser
    def parse(tokens)
      list = []

      until tokens.empty? do
        token = tokens.shift

        if ![:"(", :")"].include?(token)
          list << token # ()の中を追加していく
        elsif token == :")"
          return list # )で終了
        elsif token == :"("
          result = parse(tokens) # 次の()へ
          list << result # ()の中身を追加
        end
      end

      list
    end

    def atom?(ast)
      !ast.is_a?(Array)
    end

    def expand_syntactic_sugar(ast)
      array = []
      until ast.empty? do
        token = ast.shift
        if atom?(token)
          if token == :"'" || token == :"`"
            # クォート、バッククォート を展開する
            # [:"'", [1, 2, :a]]
            # ↓
            # [:quote, [1, 2, :a]]
            token = ast.shift
            array << [:quote, token]
          else
            # 変更なし。そのまま追加する。
            array << token
          end
        else
          # リストの場合
          # [[:funcall, :"'", :+, 1, 2]]
          # ↓
          # [[:funcall, [:quote, :+], 1, 2]]
          array << expand_syntactic_sugar(token)
        end
      end
      array
    end
  end
end
