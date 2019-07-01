#!/usr/bin/env ruby

require_relative "lib"

tx_hash = ARGV[0]
index = Integer(ARGV[1])

cell_with_status = getLiveCellByTxHashIndex(tx_hash, index)

puts cell_with_status.to_h.to_json
