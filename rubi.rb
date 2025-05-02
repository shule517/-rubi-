
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

    if token == :"("
      puts "( -> parse_lisp(#{tokens}) を 実行はじめます"
      result = parse_lisp(tokens)
      puts "-> parse_lisp(#{tokens}) -> result: #{result}"
      list << result
      puts "list: #{list}"
    elsif token == :")"
      puts ") -> list: #{list}"
      return list
    else
      list << token
      puts "list: #{list}"
    end
  end

  list[0]
end
