#!/usr/bin/env ruby

require_relative "lib"

lock_hash = ARGV[0]
capacity = Integer(ARGV[1])
i = getLiveCellsByCapacity(lock_hash, capacity)

json = {
  inputs: i.inputs.map(&:to_h),
  capacity: i.capacities.to_s,
}
puts json.to_json
