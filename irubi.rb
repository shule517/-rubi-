require_relative './rubi'

tokenizer = Rubi::Tokenizer.new
parser = Rubi::Parser.new
evaluator = Rubi::Evaluator.new

index = 0
loop.with_index do
  index += 1
  print "lisp:#{"%03d" % index}> "
  line = gets
  if line.strip == "exit"
    print "bye"
    return
  end
  begin
    tokens = tokenizer.split_tokens(line)
    ast = parser.parse(tokens)
    ast = parser.expand_syntactic_sugar(ast)
    result = ast.map { |code| evaluator.eval(code, {}, 0) }.last
    puts result
  end
end
