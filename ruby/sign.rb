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
signature = client.simple_sign_transaction(tx)

witnesses = []
tx.inputs.each do |i|
witnesses << signature
end

puts witnesses.to_json