require_relative './rubi'

tokenizer = Rubi::Tokenizer.new
parser = Rubi::Parser.new
evaluator = Rubi::Evaluator.new

loop do
  print "lisp: "
  line = gets
  begin
    tokens = tokenizer.split_tokens(line)
    ast = parser.parse(tokens)
    ast = parser.expand_syntactic_sugar(ast)
    result = ast.map { |code| pp code: code; evaluator.eval(code, {}, 0) }.last
    puts result
  end
end
