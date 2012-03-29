module Adria::Backend

require 'eventmachine'

require 'Adria/Backend/BackendConnection'
require 'Adria/Backend/Commit'
require 'Adria/Backend/Config'

class BackendClient < BackendConnection
  def initialize(storage, address, port)
    Config::Log.info "#{address}:#{port} - connecting to #{address}:#{port}"
    super storage
  rescue Exception
    Config::Log.error "#{address}:#{port} - couldn't connect"
    storage["host::#{address}:#{port}"] = "offline"
    @state = :final
    close_connection
  end

  def post_init
    # It won't be initial if an error occured during the connection.
    if @state == :initial
      Config::Log.info "#{peer_name} - requesting sync"
      send_message 'sync'
      @left = 0
      @right = @storage.commits_length - 1
      @state = :base_search
      commit_search_query
    end
  end

  private

  def commit_search_query
    if @left < @right
      @middle = (@left + @right) / 2
      send_message @storage.get_commit_id(@middle)
    else
      @state = :base_search_final
      send_message @storage.get_commit_id(@left)
    end
  end

  # If inclusive is true then @left commit is present on the other side.
  def commit_search_final(inclusive)
    id = ''
    index = @left
    unless inclusive
      index -= 1
    end
    if index > 0
      id = @storage.get_commit_id(index)
    end
    send_message 'done'

    outgoing_commits = @storage.export_since(id)
    send_message [id, outgoing_commits].to_json
    Config::Log.info(
      "#{peer_name} - sending #{outgoing_commits.length} commits"
    )

    @base_commit_id = id
    @state = :rebase
  end

  def handle_data(data)
    case @state
    when :initial
      Common::Log.error "#{peer_name} - got data in initial state"
      close_connection
    when :base_search
      if data == 'true'
        @left = @middle + 1
      else
        @right = @middle
      end
      commit_search_query
    when :base_search_final
      commit_search_final(data == 'true')
    when :rebase
      @state = :final
      # Apply all remote commits
      json = JSON.parse data
      commits = json.map { |c| Commit.from_dict c }
      # Passed by reference to @storage.rebase and will be empty after that
      # call.
      commits_length = commits.length

      @storage.rebase @base_commit_id, commits
      close_connection_after_writing

      Config::Log.info(
        "#{peer_name} - client side sync complete " +
        "(#{commits_length} new commits)"
      )
    when :final
      # Ignore any extra data.
      Config::Log.error "#{peer_name} - too much data"
    end
  end
end

end
