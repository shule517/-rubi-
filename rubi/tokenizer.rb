module Rubi
  class Tokenizer
    def split_tokens(str)
      str
        .gsub(/;.*$/, "") # コメントを除去
        .gsub("(", " ( ")
        .gsub(")", " ) ")
        .gsub(%r{(#?')}, " \\1 ") # クォート
        .gsub("`", " ` ") # バッククォート
        .gsub(",", " , ") # バッククォートカンマ
        .split
        .map { |c| Integer(c) rescue Float(c) rescue c.to_sym }
    end
  end
end