
def split_tokens(str)
  str.gsub("(", " ( ").gsub(")", " ) ").split
end

def parse_lisp(str)
  split_tokens(str).map do |c|
    if c == '('
    elsif c == ')'
    else
      Integer(c) rescue c.to_sym
    end
  end.compact
end
