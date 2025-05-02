
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

def eval_lisp(ast)
  pp ast: ast
  function = ast.shift

  if function == :define

  elsif function == :+
    ast.map do |a|
      if a.instance_of?(Array)
        eval_lisp(a)
      else
        a
      end
    end.sum
  elsif function == :-
    a = ast.shift
    if a.instance_of?(Array)
      a = eval_lisp(a)
    end
    ast.each do |b|
      b = if b.instance_of?(Array)
        eval_lisp(b)
      else
        b
          end
      a -= b
    end
    a
  end
end
