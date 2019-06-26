require 'rubygems'
require 'bundler/setup'
require "ckb"

class Client
  attr_reader :api
  attr_reader :key

  def initialize()
    @api = CKB::API.new
    privkey = File.read("privkey").strip
    @key = CKB::Key.new(privkey)
  end

  def lock
    CKB::Types::Script.generate_lock(
      key.address.blake160,
      api.system_script_code_hash
    )
  end

  def lock_hash
    @lock_hash ||= lock.to_hash
  end

  def min_output_capacity
    min_output = CKB::Types::Output.new(
      capacity: 0,
      lock: lock
    )
    min_output.calculate_min_capacity
  end

  # @return [CKB::Types::Output[]]
  def get_unspent_cells
    to = api.get_tip_block_number.to_i
    results = []
    current_from = 1
    while current_from <= to
      current_to = [current_from + 100, to].min
      cells = api.get_cells_by_lock_hash(lock_hash, current_from.to_s, current_to.to_s)
      results.concat(cells)
      current_from = current_to + 1
    end
    results
  end

  def get_balance
    get_unspent_cells.map { |cell| cell.capacity.to_i }.reduce(0, &:+)
  end

  def gather_inputs(capacity, min_capacity)
    raise "capacity cannot be less than #{min_capacity}" if capacity < min_capacity

    input_capacities = 0
    inputs = []
    get_unspent_cells.each do |cell|
      input = CKB::Types::Input.new(
        previous_output: cell.out_point,
        since: "0"
      )
      inputs << input
      input_capacities += cell.capacity.to_i

      diff = input_capacities - capacity
      break if diff >= min_capacity || diff.zero?
    end

    raise "Capacity not enough!" if input_capacities < capacity

    OpenStruct.new(inputs: inputs, capacities: input_capacities)
  end

  def getLiveCellsByCapacity(capacity)
    min_capacity = min_output_capacity()

    i = gather_inputs(
      capacity,
      min_capacity
    )
  end

  def getLiveCellByTxHashIndex(tx_hash, index)
    out_point = CKB::Types::OutPoint.new(
                        cell: CKB::Types::CellOutPoint.new(
                        tx_hash: tx_hash,
                        index: index
                      ))
    cell_with_status = api.get_live_cell(out_point)
  end
end