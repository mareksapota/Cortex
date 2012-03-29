module Adria::Backend

require 'digest'
require 'date'

require 'json'

class Commit
  include Comparable

  attr_accessor :id, :key, :value, :timestamp

  def initialize(options = {})
    @parent_id = options[:parent_id] || ''
    @key = options[:key]
    @value = options[:value]
    @timestamp = options[:timestamp] ||
      DateTime.now.strftime('%Y.%m.%d %H:%M:%S:%12N')

    update_id
  end

  def rebase(parent_id)
    @parent_id = parent_id
    update_id
  end

  def to_dict
    {
      :key => @key,
      :value => @value,
      :timestamp => @timestamp
    }
  end

  def self.from_dict(dict)
    Commit.new(
      :key => dict['key'],
      :value => dict['value'],
      :timestamp => dict['timestamp']
    )
  end

  def <=>(other)
    this = [@timestamp, @key, @value]
    other = [other.timestamp, other.key, other.value]
    this <=> other
  end

  private

  def update_id
    @id = Digest::SHA1.hexdigest "#{@parent_id} #{@key} #{@value} #{@timestamp}"
  end
end

end
