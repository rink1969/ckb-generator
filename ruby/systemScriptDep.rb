#!/usr/bin/env ruby

require_relative "lib"

client = Client.new
system_dep = client.system_script_dep

puts system_dep.to_h.to_json