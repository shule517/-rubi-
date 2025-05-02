
def split_tokens(str)
  str.gsub("(", " ( ").gsub(")", " ) ").split.map { |c| Integer(c) rescue c.to_sym }
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
# def parse_lisp(tokens)
#   @nest_index ||= -1
#   @nest_index += 1
#   nest = "   " * @nest_index
#   puts "#{nest}------------"
#   puts "#{nest}tokens: #{tokens}"
#   # pp tokens: tokens
#   # pp nest_index: @nest_index
#   # pp tokens: tokens
#   # start_index = tokens.find_index { |c| c == :"(" }
#   # end_index = tokens.rindex { |c| c == :")" }
#
#   # token = tokens[0]
#   start_index = tokens.find_index { |c| c == :"(" }
#   end_index = tokens.find_index { |c| c == :")" }
#
#   if start_index == nil # ()がもうない
#     return tokens
#   else
#     # before = tokens[0...(start_index - 1)]
#     # kakko = tokens[(start_index + 1)...(end_index - 1)]
#     # after = tokens[(end_index + 1)...]
#
#     kakko = tokens[(start_index + 1)..(end_index - 1)]
#
#     # pp before: before
#     pp kakko: kakko
#     # pp after: after
#     return before + kakko + after
#   end
#
#   # if token == :"("
#   #   end_index = tokens.rindex { |c| c == :")" }
#   #   a = tokens[1...end_index]
#   #   b = tokens[(end_index+1)...]
#   #
#   #   puts "#{nest}return a: #{a} / b: #{b}"
#   #
#   #   [a, parse_lisp(b)]
#   # else
#   #   [token]
#   # end
#
#
#   # tokens.each_with_index do |c, index|
#   #   if c == :'('
#   #     # puts "#{nest}#{c} == ("
#   #     a, b = parse_lisp(tokens[(index+1)...])
#   #     if b.nil?
#   #       return a
#   #     else
#   #       return [a, parse_lisp(b)]
#   #     end
#   #   elsif c == :')'
#   #     puts "#{nest}#{c} == )"
#   #     a = tokens[0...index]
#   #     b = tokens[(index+1)...]
#   #
#   #     if b == [:")"]
#   #       puts "#{nest}return a: #{a}"
#   #       return [a]
#   #     else
#   #       puts "#{nest}return a: #{a} / b: #{b}"
#   #       return [a, b]
#   #     end
#   #     # pp a:a, b:b
#   #     # puts "#{nest}a: #{a} / b: #{b}"
#   #     # return [a, b]
#   #   else
#   #     puts "#{nest}else #{c}"
#   #   end
#   # end
#
#   # if start_index == nil || end_index == nil
#   #   return tokens
#   # end
#   #
#   # pp start_index: start_index
#   # pp end_index: end_index
#   # token = tokens[(start_index + 1)..(end_index - 1)]
#   # pp token: token
#   # parse_lisp(token)
#
#   # tokens.map do |c|
#   #   if c == '('
#   #   elsif c == ')'
#   #   else
#   #     Integer(c) rescue c.to_sym
#   #   end
#   # end.compact
# end
