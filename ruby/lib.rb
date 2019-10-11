require 'rubygems'
require 'bundler/setup'
require "ckb"

def getLiveCellByTxHashIndex(tx_hash, index)
  out_point = CKB::Types::OutPoint.new(
                tx_hash: tx_hash,
                index: index
              )
  api = CKB::API.new
  cell_with_status = api.get_live_cell(out_point, true)
end

def system_script
  api = CKB::API.new
  system_group_outpoint = api.secp_group_out_point
  {
    name: "system",
    elf_path: "system",
    code_hash: api.secp_cell_type_hash,
    hash_type: "type",
    tx_hash: system_group_outpoint.tx_hash,
    index: CKB::Utils::to_hex(system_group_outpoint.index),
    dep_type: "dep_group"
  }
end

# send transaction which signed
# @param transaction [CKB::Transaction]
def send_raw_transaction(transaction)
  api = CKB::API.new
  tx_hash = api.send_transaction(transaction)
  # wait for tx committed
  count = 0
  while true do
    sleep(3)
    count += 1
    raise "deploy contract timeout" if count > 200

    ret = api.get_transaction(tx_hash)
    if ret.tx_status.status == "committed"
      return tx_hash
    end
  end
end

def min_output_capacity
  min_output = CKB::Types::Output.new(
    capacity: 0,
    lock:  CKB::Types::Script.generate_lock(
              "0x0000000000000000000000000000000000000000",
              "0x0000000000000000000000000000000000000000000000000000000000000000",
              "data"
            )
  )
  min_output.calculate_min_capacity("0x")
end

# @return [CKB::Types::Output[]]
def get_unspent_cells(lock_hash)
  api = CKB::API.new
  to = api.get_tip_block_number.to_i
  results = []
  current_from = 1
  while current_from <= to
    current_to = [current_from + 100, to].min
    cells = api.get_cells_by_lock_hash(lock_hash, current_from, current_to)
    results.concat(cells)
    current_from = current_to + 1
  end
  results
end

def get_balance(lock_hash)
  get_unspent_cells(lock_hash).map { |cell| cell.capacity.to_i }.reduce(0, &:+)
end

def gather_inputs(lock_hash, capacity, min_capacity)
  raise "capacity cannot be less than #{min_capacity}" if capacity < min_capacity

  input_capacities = 0
  inputs = []
  get_unspent_cells(lock_hash).each do |cell|
    input = CKB::Types::Input.new(
      previous_output: cell.out_point,
      since: 0
    )
    inputs << input
    input_capacities += cell.capacity.to_i

    diff = input_capacities - capacity
    break if diff >= min_capacity || diff.zero?
  end

  raise "Capacity not enough!" if input_capacities < capacity

  OpenStruct.new(inputs: inputs, capacities: input_capacities)
end

def getLiveCellsByCapacity(lock_hash, capacity)
  min_capacity = min_output_capacity()

  i = gather_inputs(
    lock_hash,
    capacity,
    min_capacity
  )
end

def fake_witnesses(n)
  witnesses = []
  n.times do
    witnesses << "0x"
  end
  witnesses
end

class Client
  attr_reader :api
  attr_reader :key

  def initialize(privkey)
    @api = CKB::API.new
    @key = CKB::Key.new(privkey)
  end

  def pubkey
    @key.pubkey
  end

  def blake160
    @key.address.blake160
  end

  def address
    @key.address.to_s
  end

  # @return [CKB::Types::Script]
  def lock
    CKB::Types::Script.generate_lock(
      blake160,
      api.secp_cell_type_hash,
      "type"
    )
  end

  def lock_hash
    @lock_hash ||= lock.compute_hash
  end

  # for Operators
  def deployContract(elf_path)
    contract_name = File.basename(elf_path)
    elf_bin = File.binread(elf_path)
    code_len = elf_bin.length
    code_hash = CKB::Utils.bin_to_hex(CKB::Blake2b.digest(elf_bin))

    capacity = code_len * 10 ** 8
    output= CKB::Types::Output.new(
      capacity: capacity,
      lock: lock
    )
    output_data = "0x#{elf_bin.unpack1('H*')}"
    capacity = output.calculate_min_capacity(output_data)
    output.capacity = capacity

    change_output = CKB::Types::Output.new(
      capacity: 0,
      lock: lock
    )
    change_output_data = "0x"

    i = gather_inputs(
      lock_hash,
      capacity,
      min_output_capacity()
    )
    input_capacities = i.capacities

    outputs = [output]
    outputs_data = [output_data]
    change_output.capacity = input_capacities - capacity
    if change_output.capacity.to_i > 0
      outputs << change_output
      outputs_data << change_output_data
    end

    tx = CKB::Types::Transaction.new(
      version: 0,
      cell_deps: [
        CKB::Types::CellDep.new(out_point: api.secp_group_out_point, dep_type: "dep_group")
      ],
      inputs: i.inputs,
      outputs: outputs,
      outputs_data: outputs_data,
      witnesses: fake_witnesses(i.inputs.length)
    )
    tx_hash = tx.compute_hash

    tx = tx.sign(key, tx_hash)
    send_raw_transaction(tx)

    # wait for tx committed
    count = 0
    while true do
      sleep(3)
      count += 1
      raise "deploy contract timeout" if count > 200

      ret = api.get_transaction(tx_hash)
      if ret.tx_status.status == "committed"
        return {name: contract_name,
                               elf_path: elf_path,
                               code_hash: code_hash,
                               hash_type: "data",
                               tx_hash: tx_hash,
                               index: "0x0",
                               dep_type: "code"
                              }
      end
    end
  end

  def sign_transaction(tx)
    tx_hash = tx.compute_hash
    tx.sign(key, tx_hash)
  end

  # send transaction which unsigned
  # @param transaction [CKB::Transaction]
  def send_transaction(tx)
    stx = sign_transaction(tx)
    send_raw_transaction(stx)
  end
end
