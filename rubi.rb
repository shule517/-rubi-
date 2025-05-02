
def split_tokens(str)
  str
    .gsub("(", " ( ")
    .gsub(")", " ) ")
    .split
    .map { |c| Integer(c) rescue c.to_sym }
end

def parse_lisp(tokens)
  @nest_index ||= -1
  @nest_index += 1
  nest = "   " * @nest_index
  token = tokens.shift

  if token == :"("
    list = []
    puts "#{nest}parse_lisp(#{tokens})"
    while tokens.first != :")"
      list << parse_lisp(tokens)
    end
    puts "#{nest}list: #{list}"
    tokens.shift
    list  # 完成したリストを返す
  else
    token # トークンが整数またはシンボルの場合はそのまま返す
  end
end
