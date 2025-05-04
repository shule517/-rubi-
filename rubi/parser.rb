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

    def expand_syntactic_sugar(ast)
      array = []
      until ast.empty? do
        token = ast.shift
        if token == :"'"
          # [:"'", [1, :a]]
          # ↓
          # [:quote, 1, :a]
          token = ast.shift
          array << [:quote, *token]
        elsif token.to_s.start_with?("'")
          # [:"'a"]
          # ↓
          # [[:quote, :a]]
          array << [:quote, token[1..].to_sym]
        else
          array << token
        end
      end
      array
    end
  end
end
