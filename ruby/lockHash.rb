#!/usr/bin/env ruby

require_relative "lib"

code_hash = ARGV[0]
hash_type = ARGV[1]
args = ARGV[2..]
lock_hash = CKB::Types::Script.new(
                code_hash: code_hash,
                args: args,
                hash_type: hash_type
              ).compute_hash
puts lock_hash.to_json
