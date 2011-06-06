
require 'rubygems'
require 'bundler/setup'

require 'eventmachine'
require 'bert'

class Handler < EM::Connection
  
  def initialize(ip, port)
    @ip = ip
    @port = port
  end
  
  def receive_data(data)
    data = BERT.decode(data)
    puts "received: #{data.inspect}"
  end
  
  def unbind
    sleep(0.1)
    reconnect(@ip, @port)
  end
end

# trap('INT') do
#   EM::stop_event_loop()
# end

EM::run do
  s = EM::connect('127.0.0.1', 12000, Handler, '127.0.0.1', 12000)
  puts "Client started."
end

puts "Client exited."
