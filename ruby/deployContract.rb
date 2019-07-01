#!/usr/bin/env ruby

require_relative "lib"

privkey = ARGV[0]
elf_path = ARGV[1]
client = Client.new(privkey)
contract_info = client.deployContract(elf_path)

puts contract_info.to_json