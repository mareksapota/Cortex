#!/usr/bin/env ruby

require 'rubygems'
require 'bundler/setup'

require 'optparse'

require 'eventmachine'


# Ruby is stupid and doesn't allow defining nested modules in one go, it will
# scream about uninitialized constants and name errors.
module Adria
end
module Adria::Backend
end

require 'Adria/Backend/GlobalStorage'
require 'Adria/Backend/Config'

options = {}
optparse = OptionParser.new do |opts|
  opts.on('-h', '--help', 'Print help and exit') do
    puts opts
    exit
  end

  opts.on('-c', '--config CONF', 'Location of a configuration file') do |c|
    options[:config] = c
  end

  opts.on(
    '--address ADDR',
    'Backend will bind to this address (default is 127.0.0.1)'
  ) do |a|
    options[:address] = a
  end

  opts.on(
    '--port PORT',
    Integer,
    'Backend will listen on this port (default is 8205)'
  ) do |p|
    options[:port] = p
  end

  opts.on(
    '--peer PEER',
    'Backend will mark this peer as online so it can sync with it'
  ) do |p|
    options[:peer] = p
  end
end

optparse.parse!

unless options[:config].nil?
  Adria::Backend::Config.load options[:config]
end

unless options[:address].nil?
  Adria::Backend::Config.address = options[:address]
end

unless options[:port].nil?
  Adria::Backend::Config.port = options[:port]
end

EventMachine.run do
  s = Adria::Backend::GlobalStorage.new
  unless options[:peer].nil?
    s["host::#{options[:peer]}"] = 'online'
  end
end
