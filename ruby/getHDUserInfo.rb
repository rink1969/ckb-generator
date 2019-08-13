#!/usr/bin/env ruby

require_relative "lib"
require "money-tree"

index = Integer(ARGV[0])
raise "index too big" if index >= 0x80000000
seed_path = File.expand_path("~/.ckb-generator/seed_hex")
if File.exist?(seed_path)
  seed_hex = IO.read(seed_path).strip
  master = MoneyTree::Master.new({seed_hex: seed_hex})
else
  master = MoneyTree::Master.new
  Dir::mkdir(File.expand_path("~/.ckb-generator"))
  IO.write(File.expand_path("~/.ckb-generator/seed_hex"), master.seed_hex)
end
privkey = "0x" + MoneyTree::PrivateKey.new({key: master.derive_private_key(index)[0]}).to_hex
client = Client.new(privkey)

json = {
  privkey: privkey,
  pubkey: client.pubkey,
  blake160: client.blake160,
  address: client.address
}
puts json.to_json