
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

  list[0]
end
