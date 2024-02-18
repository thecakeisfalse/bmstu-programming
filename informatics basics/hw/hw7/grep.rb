#!/usr/bin/env ruby

require 'optparse'

def valid?(line, pattern)
  pattern.is_a?(String) ? (line.include? pattern) : (line =~ pattern)
end

def integer?(v)
  Integer(v) rescue false
end

def open_file(filename, mode)
  if mode == 'r'
    raise "Can't open file" if !File.file?(filename) or !File.readable?(filename)
  elsif mode == 'w'
    raise "Can't open file" if File.file?(filename) and !File.writable?(filename)
  end
  File.open(filename, mode) rescue raise "Don't have permissions to open file"
end

def grep_from_stream(stream, pattern, re, ignore_case, max_count, show_lines)
  return if max_count == 0
  pattern = pattern.downcase if ignore_case

  begin
    pattern = Regexp.new pattern if re
  rescue Exception => e
    raise "Invalid regular expression"
  end

  counter = 0
  begin
    stream.each_with_index do |line, index|
      temp = ignore_case ? line.downcase : line
      if valid?(temp, pattern)
        counter+=1
        if show_lines
          puts "#{(index+1)} #{line}"
        else
          puts "#{line}"
        end
        break if counter == max_count
      end
    end
    rescue SignalException => e
  end
end

def grep(files, options)
  re = options.has_key?(:re)
  ignore_case = options.has_key?(:ignore_case)
  max_count = options.fetch(:max_count, -1)
  raise "Invalid max count (expected integer, got #{max_count})" if !integer?(max_count)
  max_count = max_count.to_i
  pattern = options[:pattern]
  show_lines = options.has_key?(:show_lines)

  files.each do |file|
    if !File.file?(file)
      puts "Can't open file: #{file}"
      next
    end

    puts "#{file}\n#{'=' * file.length}"
    grep_from_stream File.open(file), pattern, re, ignore_case, max_count, show_lines
  end

  grep_from_stream STDIN, pattern, re, ignore_case, max_count, show_lines if files.empty?
end

ARGV << "-h" if ARGV.empty?

options = {}
OptionParser.new do |opt|
  opt.banner = "Usage: #{$0} [option]... [file]..."
  opt.on('-e PATTERN', 'use PATTERNS for matching') { |p| (options[:pattern] = p); options[:re] = true }
  opt.on('-i', 'ignore case distinctions in patterns and data') { |o| options[:ignore_case] = true }
  opt.on('-m NUM', 'stop after NUM selected lines') { |o| options[:max_count] = o }
  opt.on('-n', 'print line number with output lines') { |o| options[:show_lines] = true }
  opt.on_tail('-h', 'show help message') { |o| puts opt; exit }
end.parse!

args = ARGV
if !options.has_key?(:pattern)
  options[:pattern] = args[0]
  args = args[1..]
end

grep args, options
