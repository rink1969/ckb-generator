#!/usr/bin/env ruby

require_relative "lib"

elf_path = ARGV[0]
client = Client.new
contract_info = client.deployContract(elf_path)

puts contract_info.to_json