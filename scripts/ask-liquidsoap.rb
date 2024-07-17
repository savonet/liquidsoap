#!/usr/bin/ruby

require 'net/telnet'

liq_host = "localhost"
liq_port = 1234

conn = Net::Telnet::new("Host" => liq_host, "Port" => liq_port)

conn.puts(ARGV[0])
conn.waitfor("Match" => /^END$/) do |data|
  puts data.sub(/\nEND\n/,"")
end
