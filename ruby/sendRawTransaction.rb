#!/usr/bin/env ruby

require_relative "lib"

tx_path = ARGV[0]
tx_s = File.read(tx_path).strip
tx_json = JSON.parse(tx_s, symbolize_names: true)
tx = CKB::Types::Transaction.from_h(tx_json)
client = Client.new
tx_hash = client.send_raw_transaction(tx)

puts tx_hash.to_json