#!/usr/bin/env ruby

require 'optparse'

def recursive_tree(wd, root, only_directory, outstream)
  dirs, files = [], []
  path = wd.gsub root, ''
  level = path.count '/'
  ident = (level > 0 ? "  |" * (level-1) : "") + "  +- "
  subident = (level > 0 ? "  |" : "") + ident

  begin
    Dir.open(wd).children().each() do |path|
      path = "#{wd}/#{path}"
      dirs << path if File.directory?(path)
      files << path if File.file?(path)
    end
  rescue
    return outstream.puts "#{ident}#{wd.split('/')[-1]} [error opening dir]"
  end

  outstream.puts "#{ident}#{path.split('/')[-1]}"

  dirs.each{|dir| recursive_tree dir, root, only_directory, outstream}
  only_directory || files.each{|file| outstream.puts "#{subident}#{file.split('/')[-1]}"}
end

def tree(wd, only_directory, outstream)
  recursive_tree("#{wd}", "#{wd}", only_directory, outstream)
end

def open_file(filename)
  raise "Can't open file" if File.file?(filename) and !File.writable?(filename)
  File.open(filename, "w") rescue raise "Don't have permissions to open file"
end

options = {}
OptionParser.new do |opt|
  opt.banner = "Usage: #{$0} [options] [path]"
  opt.on('-d', 'list directories only') { |o| options[:only_directory] = true}
  opt.on('-o FILENAME', 'output to file instead of stdout') { |o| options[:output_file] = o}
end.parse!

stream = options.has_key?(:output_file) ?
         open_file(options[:output_file]) : STDOUT

ARGV.empty? ? tree(Dir.getwd, options.has_key?(:only_directory), stream) :
              ARGV.each { |wd| tree wd, options.has_key?(:only_directory), stream }
