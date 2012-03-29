module Adria::Backend::Config

require 'yaml'
require 'logger'

# Sync every n seconds.
SyncTime = 5
# Squash commits every n seconds.
SquashTime = 30
# Don't sync n seconds before and after squashing commits.
SquashCooldownTime = 5

Log = Logger.new(STDERR)

@@port = 8205
@@address = '127.0.0.1'

# If true then there shouldn't be any outgoing connections.
@@lockdown = false

def self.lockdown
  @@lockdown
end

def self.lock
  @@lockdown = true
end

def self.unlock
  @@lockdown = false
end

def self.load(conf_file)
  conf = YAML.load_file conf_file
  unless conf['port'].nil?
    @@port = conf['port']
  end
  unless conf['address'].nil?
    @@address = conf['address']
  end
end

def self.port=(port)
  @@port = port
end

def self.port
  @@port
end

def self.address=(address)
  @@address = address
end

def self.address
  @@address
end

end
