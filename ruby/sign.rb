#!/usr/bin/env ruby

require_relative "lib"

privkey = ARGV[0]
tx_path = ARGV[1]
tx_s = File.read(tx_path).strip
tx_json = JSON.parse(tx_s, symbolize_names: true)
tx = CKB::Types::Transaction.from_h(tx_json)
client = Client.new(privkey)
stx = client.sign_transaction(tx)

puts stx.witnesses.to_json