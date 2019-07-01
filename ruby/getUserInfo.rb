#!/usr/bin/env ruby

require_relative "lib"

privkey = ARGV[0]
client = Client.new(privkey)

json = {
  privkey: privkey,
  pubkey: client.pubkey,
  blake160: client.blake160,
  address: client.address
}
puts json.to_json