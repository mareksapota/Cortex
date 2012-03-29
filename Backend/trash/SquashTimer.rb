module Adria::Backend

require 'time'

require 'eventmachine'

require 'Adria/Backend/Config'

class SquashTimer
  def initialize(storage)
    @storage = storage
    set_lockdown_timer
  end

  def set_lockdown_timer
    t = Time.now.utc.to_i
    t = Config::SquashTime - t % Config::SquashTime - Config::SquashCooldownTime
    if t < 0
      t += Config::SquashTime
    end

    EventMachine.add_timer(t) do
      lockdown
    end
  end

  def lockdown
    Config::Log.info 'Going into lockdown'
    Config::lock
    EventMachine.add_timer(Config::SquashCooldownTime) do
      squash
    end
  end

  def squash
    Config::Log.info 'Squashing commits'
    @storage.squash
    EventMachine.add_timer(Config::SquashCooldownTime) do
      lift_lockdown
    end
  end

  def lift_lockdown
    Config::Log.info 'Lifting lockdown'
    Config::unlock
    set_lockdown_timer
  end
end

end
