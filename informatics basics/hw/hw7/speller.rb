#!/usr/bin/env -S ruby -W0

require 'continuation'
require 'set'

def whitespace?(ch)
  ch =~ /[[:space:]]/
end

def punctuation?(ch)
  (ch =~ /[[:punct:]]/) and ch != "`"
end

def symbol?(ch)
  (ch =~ /[[:alnum:]]/) or ch == "`"
end

def number?(ch)
  ch =~ /^[[:digit:]]$/
end

def make_stream(s)
  temp_stream = (s+"\0").downcase.chars
  last_newline = l = 0
  stream = []

  temp_stream.each_with_index do |x, i|
    stream << [i+1, l, i-last_newline+1, x]
    if x == "\n"
      last_newline = i+1
      l += 1
    end
  end

  stream
end

def scanner(s)
  stream = make_stream s

  callcc do |error|
    result = tokens(stream, error)
    (peek(stream)[-1] == "\0") and result
  end
end

def peek(stream)
  raise "Invalid stream" if stream.empty?
  stream[0]
end

def next_(stream)
  ch = peek stream
  stream.replace stream[1..]
  ch
end

def tokens(stream, error)
  pos, l, c, ch = peek stream
  result = []

  if whitespace? ch
    spaces stream, error
    result = tokens stream, error
  elsif (punctuation? ch) or (symbol? ch)
    result << token(stream, error)
    result += tokens(stream, error)
  end
  result
end

def token(stream, error)
  pos, l, c, ch = peek stream
  if punctuation? ch
    next_ stream
  elsif symbol? ch
    [pos, l, c, word(stream, error)]
  else
    error false
  end
end

def word(stream, error)
  pos, l, c, ch = peek stream
  if symbol? ch
    pos, l, c, ch = next_(stream); ch + word(stream, error)
  else
    ''
  end
end

def spaces(stream, error)
  next_ stream if whitespace? peek(stream)[-1]
end

def open_file(filename, mode)
  if mode == 'r'
    raise "Can't open file" if !File.file?(filename) or \
                               !File.readable?(filename)
  elsif mode == 'w'
    raise "Can't open file" if  File.file?(filename) and \
                               !File.writable?(filename)
  end
  File.open(filename, mode) 
      rescue raise "Don't have permissions to open file"
end

def create_dictionary(text)
  tokens = scanner(text)
  raise "Invalid dictionary content" if !tokens
  result = Set.new
  tokens.each {|pos, l, c, value| result << value \
      if symbol?(value) and not number?(value)}
  result
end

def load_dictionary(filename)
  s = open_file(filename, "r").read
  create_dictionary s
end

def save_dictionary(filename, dictionary)
  f = open_file(filename, "w")
  dictionary.each do |item|
    f.write("#{item}\n")
  end
end

def find_missspelled(filename, dictionary)
  s = open_file(filename, "r").read
  tokens = scanner(s)
  raise "Invalid content" if !tokens

  tokens.each do |pos, l, c, value|
    if symbol?(value) and not number?(value)
      if !dictionary.include?(value)
        puts "#{l},\t#{c}\t#{value}"
      end
    end
  end
end

if ARGV.size < 1 or ARGV.size > 2 
  puts "Usage: #{$0} <dictionary file> <test file>"
  exit
end

d = load_dictionary(ARGV[0])

if ARGV.size > 1
    find_missspelled(ARGV[1], d)
else
    save_dictionary("dict-#{ARGV[0]}", d) rescue Exception => e
end
