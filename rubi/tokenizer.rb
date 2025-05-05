module Rubi
  class Tokenizer
    def split_tokens(str)
      str
        .gsub("(", " ( ")
        .gsub(")", " ) ")
        .gsub("'", " ' ") # クォート
        .gsub("`", " ` ") # バッククォート
        .split
        .map { |c| Integer(c) rescue Float(c) rescue c.to_sym }
    end
  end
end