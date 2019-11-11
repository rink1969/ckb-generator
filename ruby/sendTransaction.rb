#!/usr/bin/env ruby

require_relative "lib"

privkey = ARGV[0]
tx_path = ARGV[1]
tx_s = File.read(tx_path).strip
tx_json = JSON.parse(tx_s, symbolize_names: true)
tx = CKB::Types::Transaction.from_h(tx_json)

# fix witnesses
witnesses = fake_witnesses(tx.inputs.length)
tx.witnesses = witnesses

client = Client.new(privkey)
tx_hash = client.send_transaction(tx)

puts tx_hash.to_json