#!/usr/bin/env ruby

require_relative "lib"

code_hash = ARGV[0]
args = ARGV[1..]
lock_hash = CKB::Types::Script.new(
                code_hash: code_hash,
                args: args
              ).to_hash
puts lock_hash.to_json
