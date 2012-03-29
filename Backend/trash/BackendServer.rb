module Adria::Backend

require 'socket'

require 'eventmachine'

require 'Adria/Backend/Commit'
require 'Adria/Backend/Config'
require 'Adria/Backend/BackendConnection'

class BackendServer < BackendConnection
  def initialize(storage)
    super storage
    Config::Log.info "#{peer_name} - connection from #{peer_name}"
  end

  private

  def handle_data(data)
    case @state
    when :initial
      if data == 'sync'
        Config::Log.info "#{peer_name} - sync request"
        @state = :base_search
      elsif data == 'get'
        Config::Log.info "#{peer_name} - get request"
        @state = :get
      elsif data == 'set'
        Config::Log.info "#{peer_name} - set request"
        @state = :set
      elsif data == 'delete'
        Config::Log.info "#{peer_name} - delete request"
        @state = :delete
      else
        Config::Log.error "#{peer_name} - invalid request"
        close_connection
      end
    when :base_search
      # Search for common base commit.
      if data == 'done'
        Config::Log.info "#{peer_name} - base commit search finished"
        @state = :rebase
      else
        send_message @storage.has_commit?(data).to_s
      end
    when :rebase
      @state = :final
      # Apply all remote commits
      json = JSON.parse data
      base_commit_id = json[0]
      commits = json[1].map { |c| Commit.from_dict c }
      # Passed by reference to @storage.rebase and will be empty after that
      # call.
      commits_length = commits.length

      # Send back commits since the base commit.
      outgoing_commits = @storage.export_since(base_commit_id)
      send_message outgoing_commits.to_json
      Config::Log.info(
        "#{peer_name} - sending #{outgoing_commits.length} commits"
      )

      @storage.rebase base_commit_id, commits
      close_connection_after_writing

      Config::Log.info(
        "#{peer_name} - sync complete (#{commits_length} new commits)"
      )
    when :get
      send_message @storage[data]
      Config::Log.info "#{peer_name} - accessing '#{data}'"
      close_connection_after_writing
      @state = :final
    when :set
      @key = data
      @state = :set_value
    when :set_value
      @storage[@key] = data
      Config::Log.info "#{peer_name} - setting '#{@key}' to '#{data}'"
      close_connection
      @state = :final
    when :delete
      @storage.delete data
      Config::Log.info "#{peer_name} - deleting '#{data}'"
      close_connection
      @state = :final
    when :final
      # Ignore any extra data.
      Config::Log.error "#{peer_name} - too much data"
    end
  end
end

end
