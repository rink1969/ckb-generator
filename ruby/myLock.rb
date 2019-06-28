#!/usr/bin/env ruby

require_relative "lib"

client = Client.new
my_lock = client.lock

puts my_lock.to_h.to_json