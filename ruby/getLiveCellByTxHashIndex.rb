#!/usr/bin/env ruby

require_relative "lib"

tx_hash = ARGV[0]
index = Integer(ARGV[1])
client = Client.new
cell_with_status = client.getLiveCellByTxHashIndex(tx_hash, index)

puts cell_with_status.to_h.to_json
