module Adria::Backend

require 'socket'

require 'eventmachine'

class BackendConnection < EventMachine::Connection
  def initialize(storage)
    @storage = storage
    @buffer = ''
    @state = :initial
    @peer_port, @peer_address = Socket.unpack_sockaddr_in(get_peername)
  end

  def receive_data(data)
    @buffer += data
    i = @buffer.index("\r\n")
    while not i.nil?
      data = @buffer.slice 0, i
      @buffer = @buffer.slice i + 2, @buffer.length
      handle_data data
      i = @buffer.index("\r\n")
    end
  end

  private

  def send_message(text)
    send_data "#{text}\r\n"
  end

  def peer_name
    "#{@peer_address}:#{@peer_port}"
  end

  def handle_data(data)
    # Override this method.
  end
end

end
