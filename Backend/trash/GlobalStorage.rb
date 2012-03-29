module Adria::Backend

require 'set'

require 'eventmachine'

require 'Adria/Backend/Commit'
require 'Adria/Backend/BackendServer'
require 'Adria/Backend/BackendClient'
require 'Adria/Backend/SquashTimer'
require 'Adria/Backend/Config'

# Methods of this class are not synchronized because of EventMachine's
# single threaded nature.  This methods should not be accessed outside of EM
# thread.
class GlobalStorage
  def initialize
    @commits = []
    @commit_ids = Set.new
    # TODO: AVL trees? (does hash shrink after removal?).  Also current prefix
    # lookup is slow.
    @storage = {}

    start_server
    # TODO: remove - this code is only for debugging
    EventMachine.add_periodic_timer(1) do
      puts
      p @storage
      p "storage = #{@storage.length}"
      p "commits = #{@commits.length}"
      puts
    end
  end

  def []=(key, value)
    key = key.to_s
    last_id = @commits.empty? ? '' : @commits.last.id
    commit = Commit.new(
      :key => key,
      :value => value,
      :parent_id => last_id
    )
    @commits.push(commit)
    @commit_ids.add commit.id
    if value.nil?
      @storage.delete key
    else
      @storage[key] = value
    end
  end

  def delete(key)
    self[key] = nil
  end

  def [](key)
    @storage[key.to_s]
  end

  def find_where_key_starts_with(prefix)
    @storage.select { |k, v| k.match(/^#{prefix}/) }
  end

  def has_commit?(commit_id)
    @commit_ids.include? commit_id
  end

  def commits_length
    @commits.length
  end

  # Returns id of commit with given index or '' if the commit doesn't exist.
  def get_commit_id(index)
    commit = @commits[index]
    commit.nil? ? '' : commit.id
  end

  # Merge new commits with current ones.  Base commit is the last commit that
  # shouldn't be involved in the rebase.
  def rebase(base_commit_id, new_commits)
    @commits.reverse!
    old_commits = @commits.take_while { |c| c.id != base_commit_id }
    untouched_commits = @commits.drop old_commits.length
    @commits = untouched_commits.reverse!

    # Merge old and new commits.
    commits = []
    while (not new_commits.empty?) or (not old_commits.empty?)
      if new_commits.empty?
        commits.push old_commits.shift
      elsif old_commits.empty?
        commits.push new_commits.shift
      else
        if new_commits[0] < old_commits[0]
          commits.push new_commits.shift
        else
          commits.push old_commits.shift
        end
      end
    end
    commits.sort!

    last = nil
    unless untouched_commits.empty?
      last = untouched_commits.last
    end

    while not commits.empty?
      # Discard duplicates.
      if commits.first == last
        commits.shift
        next
      end

      c = commits.shift
      @commit_ids.delete c.id
      c.rebase(last.nil? ? '' : last.id)
      @commit_ids.add c.id
      apply c
      last = c
      @commits.push c
    end
  end

  # Remove useless commits.
  def squash
    commits = []
    keys = Set.new
    @commits.reverse!.each do |c|
      unless keys.include? c.key
        unless c.value == nil
          commits.push c
        end
        keys.add c.key
      end
    end
    @storage = {}
    @commit_ids = Set.new
    @commits = []
    rebase '', commits.reverse!
  end

  # Export all commits after the base commit.
  def export_since(base_commit_id)
    commits = @commits.reverse.take_while { |c| c.id != base_commit_id }
    commits.map { |c| c.to_dict }
  end

  private

  # Apply commit effect to storage.  Storage should be used directly to avoid
  # creation of additional commits as in self[]= method.
  def apply(commit)
    key = commit.key.to_s
    value = commit.value
    if value.nil?
      @storage.delete key
    else
      @storage[key] = value
    end
  end

  def hostname
    "host::#{Config::address}:#{Config::port}"
  end

  def start_server
    Config::Log.info "Starting server on #{Config.address}:#{Config.port}"
    EventMachine.start_server Config::address, Config::port, BackendServer, self
    EventMachine.add_periodic_timer(Config::SyncTime) do
      unless Config::lockdown
        if self[hostname] != 'online'
          self[hostname] = 'online'
        end
        hosts = find_where_key_starts_with 'host::'
        # Contact only online hosts and don't talk to yourself.
        hosts = hosts.select { |k, v| v == 'online' and k != hostname}
        hosts = hosts.map do |k, v|
          m = k.match(/^host::([^:]*):(.*)$/)
          { :address => m[1], :port => m[2] }
        end
        host = hosts.sample
        # Abandon sync if there are no viable hosts.
        unless host.nil?
          EventMachine.connect(
            host[:address],
            host[:port],
            BackendClient,
            self,
            host[:address],
            host[:port]
          )
        end
      end
    end
    @squash_timer = SquashTimer.new self
  end
end

end
