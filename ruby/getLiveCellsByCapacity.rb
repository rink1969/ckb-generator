#!/usr/bin/env ruby

require_relative "lib"

capacity = Integer(ARGV[0])
client = Client.new
i = client.getLiveCellsByCapacity(capacity)

json = {
  inputs: i.inputs.map(&:to_h),
  capacity: i.capacities.to_s,
}
puts json.to_json
