#!/usr/bin/env ruby

require 'optparse'

def wc_from_stream(stream)
  lines = words = symbols = bytes = 0

  begin
    stream.each do |line|
      symbols += line.length
      bytes += line.bytesize
      lines += 1 if line[-1] == "\n"
      words += line.split(' ').size
      puts "\n" if line[-1] != "\n" and stream == STDIN
    end
    rescue SignalException => e
  end
  [ bytes, symbols, lines, words ]
end

def wc_wrapper(stream, options)
  count_lines = (options.has_key?(:count_lines) or options.empty?)
  count_words = (options.has_key?(:count_words) or options.empty?)
  count_symbols = options.has_key?(:count_symbols)
  count_bytes = (options.has_key?(:count_bytes) or options.empty?)
  bytes, symbols, lines, words = wc_from_stream stream
  result = []
  result << lines if count_lines
  result << words if count_words
  result << symbols if count_symbols
  result << bytes if count_bytes
end

def wc(files, options)
  if files.empty?
    puts wc_wrapper(STDIN, options)
    return
  end

  total = []
  files.each do |file|
    (STDERR.puts "#{file}: Can't open file"; next) if !File.file?(file) or !File.readable?(file)
    result = wc_wrapper(File.open(file), options)
    total = (total.empty? ? result : [total, result].transpose.map(&:sum))
    puts "#{result.join(' ')} #{file}"
  end
  puts "#{total.join(' ')} total" if files.size > 1

end

options = {}
OptionParser.new do |opt|
  opt.banner = "Usage: #{$0} [option]... [file]..."
  opt.on('-c', 'print the byte counts') { |o| options[:count_bytes] = true}
  opt.on('-m', 'print the character counts') { |o| options[:count_symbols] = true}
  opt.on('-l', 'print the newline counts') { |o| options[:count_lines] = true}
  opt.on('-w', 'print the word counts') { |o| options[:count_words] = true}
end.parse!

wc ARGV, options
