# 介绍

ckb提供了图灵完备的智能合约能力。但是它采用的是Cell Model（更类似比特币的UTXO Model，而不是以太坊的Account Model）。

链下计算，链上验证的方式，使得Dapp的开发分为两部分。

一部分是客户端，要实现完整的Dapp逻辑。从live cell集合中选择合适的cell出来，拿到对应的transaction hash和index，作为inputs。然后根据Dapp逻辑，从inputs计算出对应的outputs，组成transaction。接着对transaction进行签名，可能是一个人签名，也可能是多个人一起签名，还要对签名进行一些组合等操作。最后将交易发送到链上。

另外一个部分是智能合约（ckb里面遵循比特币的习惯叫script）。也要实现完整的Dapp逻辑，对交易的签名和内容进行验证。

我们可以看到，实现一个Dapp需要把同样的逻辑实现两编。目前合约开发用C语言，客户端用ruby或者java。导致门槛比较高，而且实现的时候容易出错。

本项目的目的是设计一个DSL，用户只要使用DSL来描述Dapp的逻辑，就可以自动生成合约端和客户端代码。提升开发效率，降低出错的可能。

# 设计

DSL使用Haskell实现，在这方面Haskell具有不可替代的优势。

设计DSL需要把业务逻辑拆分成一系列Operators，这些Operators足够灵活和稳定，能够通过组合满足各种逻辑业务的描述。

有点像是构造了一个面向业务逻辑的指令集。

每个Operator的后端实现是生成代码（客户端生成Ruby或者Java代码，合约端生成C代码）；或者调用已经写好的库函数（就比较接近框架了）。

暂时先实现成EDSL，Monadic的DSL，其实有点像写命令式语言的代码，按照代码顺序执行下去。

如果有必要后续可以设计成外部DSL，就比较类似一个单独的新语言，有独立的语法和编译器。编译器的工作是解析DSL源码之后转换成之前提到的Monadic的Operators序列（类似传统编译器里的IR）。

# Operators

通过总结之前写合约的经验，以及反复调整，目前Operators列表如下。

主要考虑是隔离SDK和DSL，尽量使得DSL写的程序不涉及太多链的实现细节。

```haskell
-- define of DSL
data Operator next =
  GetUserInfo Key (UserInfo -> next)
  | LockHash (Hash, [Arg]) (Hash -> next)
  | QueryLiveCells (Hash, Int) (RetQueryLiveCells -> next)
  | GetLiveCellByTxHashIndex (Hash, Index) (CellWithStatus -> next)
  | DeployContract (UserInfo, Path) (ContractInfo -> next)
  | Sign (UserInfo, Transaction) ([Witness] -> next)
  | SystemScript (ContractInfo -> next)
  | SendTransaction (UserInfo, Transaction) (Hash -> next)
  | SendRawTransaction Transaction (Hash -> next)
  | Ask String (String -> next)
  deriving (Functor)
```

# 合约

受 [Balzac](https://blockchain.unica.it/balzac/docs/nutshell.html) 的启发，把lock script表示成一个函数。

但是Cell Model相比Bitcoin的UTXO的模型有些扩展，多了code hash等概念。

因此设计了 ContractInfo 来存放合约相关的信息。

```Haskell
data ContractInfo = ContractInfo
  { contract_info_name :: Name
  , contract_info_elf_path :: Path
  , contract_info_code_hash :: Hash
  , contract_info_tx_hash :: Hash
  , contract_info_index :: Index
  }
```

然后搭配一个函数来表示合约对应的 lock 逻辑，输入是未签名/未完成的交易，输出是可以直接上链的完成交易。

比如always_success合约的lock函数表示如下：

```Haskell
always_success_lock :: Transaction -> Dapp Transaction
always_success_lock tx = return tx
```

lock的逻辑可以组合。

CKB相对Bitcoin的一个特点是，没有内置签名算法等密码学原语，因此验签名也作为部分lock逻辑组合进来。

比如 system script的lock逻辑如下：

```Haskell
system_script_lock :: UserInfo -> Transaction -> Dapp Transaction
system_script_lock user_info tx = do
  witnesses <- sign (user_info, tx)
  let stx = set transaction_witnesses witnesses tx
  return stx
```

将来还会出现更复杂的lock逻辑。

### 生成合约代码

查询并挑选inputs的逻辑跟合约没有关系，合约的目的就是检查从inputs生成outputs和witnesses的正确性。

如前所述，这部分都归结到lock逻辑中。

因此可以根据合约的lock逻辑来生成对应的合约代码。

前面提到合约的lock逻辑可以由多个小的函数组合而成。

目前的设想是提供一些描述lock逻辑的函数和对应的合约代码片段。

# 示例

如下示例，跟 [ckb contract example](https://github.com/rink1969/ckb-contract-examples)里面第一个例子是一样的。

```haskell
cabal new-repl
*Type> :m Dapp.AlwaysSuccess 
Prelude Dapp.AlwaysSuccess> runAlwaysSuccess 
Begin to run Dapp always_sucess!
Press Enter to continue...
<<<user input>>>
Please input privkey:
<<<user input>>> 0x...
Move some capacity from system_script to contract always_sucess!
Press Enter to continue...
<<<user input>>>
Please input sender user privkey:
<<<user input>>> 0x...
Please input receiver blake160:
<<<user input>>> 0x4a88cef22e4e71c48c40da51c1d6bd16daa97aa7
input capacity
<<<user input>>> 100000000000
data in output
<<<user input>>> 
Start loop call contract always_sucess!
Press Enter to continue...
<<<user input>>>
Loop call contract always_sucess!
Press Enter to continue...Input "e" to exit loop...
<<<user input>>>
Loop call contract always_sucess!
Press Enter to continue...Input "e" to exit loop...
<<<user input>>> e
Just "0x6c746347d5b029f6c64775e53f8e15d0cce1f2d5a46b844b5e9e6eb2151530f6"
```

第二个示例 `Vote`

```haskell
cabal new-repl
*Type> :m Dapp.Vote
Prelude Dapp.Vote> writeVoteConfig $ mkVoteConfig 3 2 ["4a88cef22e4e71c48c40da51c1d6bd16daa97aa7", "a47f8029997fcc67aff87384daac404f39e31ceb", "96f4093cf179aaa369379402d74f70090fae11ec"]
Prelude Dapp.Vote> runDeployConfigData 
privkey
<<<user input>>> 0x...
Just (ContractInfo {contract_info_name = "vote_config_data", contract_info_elf_path = "/tmp/vote_config_data", contract_info_code_hash = "0xa3f95fbb3f39cbc8c332540adc28da64da89ebb4c771e03d322c7627cf14c095", contract_info_tx_hash = "0xa508f9e41900beaaa1ca69d5a3a59310ca890b52be0e54543aca9cb5ca41f00f", contract_info_index = "0"})
Prelude Dapp.Vote> let Just config_info = it
Prelude Dapp.Vote> runDeployVoteContract 
privkey
<<<user input>>> 0x...
elf path
/path/to/ckb-contract-examples/vote
Just (ContractInfo {contract_info_name = "vote", contract_info_elf_path = "/path/to/ckb-contract-examples/vote", contract_info_code_hash = "0xa62834729f93950b5c812bd56d86613d06d3c01198a6243de8445d7594c108d2", contract_info_tx_hash = "0x55b662d0573161095ff03de5cd0179e97a016dc4cf5a4e02e0d54d547a380277", contract_info_index = "0"})
Prelude Dapp.Vote> let Just contract_info = it
Prelude Dapp.Vote> runVote "0x" contract_info
privkey
<<<user input>>> 0x...   (privkey for 0x4a88cef22e4e71c48c40da51c1d6bd16daa97aa7)
input capacity
<<<user input>>> 20000000000
Just "0x978b1ecf5f531184a517b291b350727b6cd5605265abb466423c86ca859a44d3"
Prelude Dapp.Vote> let Just vote_tx = it
Prelude Dapp.Vote> runVote "0xbb" contract_info
privkey
<<<user input>>> 0x...   (privkey for 0xa47f8029997fcc67aff87384daac404f39e31ceb)
input capacity
<<<user input>>> 20000000000
Just "0xd5f5ca6f47ff75e172fe8be72c4c3b8ae3ac9d568ab7d6d5fde44baf42e0d13e"
Prelude Dapp.Vote> let Just vote1_tx = it
Prelude Dapp.Vote> runVote "0x" contract_info
privkey
<<<user input>>> 0x...   (privkey for 0x96f4093cf179aaa369379402d74f70090fae11ec)
input capacity
<<<user input>>> 20000000000
Just "0xa903365c01e8db1c71f3cad27501f01f3b67039e1653ca5185ba4880453cae79"
Prelude Dapp.Vote> let Just vote2_tx = it
Prelude Dapp.Vote> runReVote "0xaa" contract_info vote_tx
privkey
<<<user input>>> 0x...   (privkey for 0x4a88cef22e4e71c48c40da51c1d6bd16daa97aa7)
Just "0xaa579390b6fcde0df064743b98527f5e562ba257d2e8e82c764f9598b8e72fd1"
Prelude Dapp.Vote> let Just vote_tx = it
*Type Dapp.Vote> runSumVots [vote_tx, vote1_tx, vote2_tx] contract_info config_info "0x4a88cef22e4e71c48c40da51c1d6bd16daa97aa7"
privkey
<<<user input>>> 0x...   (privkey for 0x4a88cef22e4e71c48c40da51c1d6bd16daa97aa7)
Just (Transaction { ...
*Type Dapp.Vote> let Just sum_tx = it
*Type Dapp.Vote> runSumVots [vote_tx, vote1_tx, vote2_tx] contract_info config_info "0x4a88cef22e4e71c48c40da51c1d6bd16daa97aa7"
privkey
<<<user input>>> 0x...   (privkey for 0xa47f8029997fcc67aff87384daac404f39e31ceb)
Just (Transaction { ...
*Type Dapp.Vote> let Just sum1_tx = it
*Type Dapp.Vote> runSumVots [vote_tx, vote1_tx, vote2_tx] contract_info config_info "0x4a88cef22e4e71c48c40da51c1d6bd16daa97aa7"
privkey
<<<user input>>> 0x...   (privkey for 0x96f4093cf179aaa369379402d74f70090fae11ec)
Just (Transaction { ...
*Type Dapp.Vote> let Just sum2_tx = it
*Type Dapp.Vote> runMerge [sum_tx, sum1_tx, sum2_tx]
Just "0xff729c2efd4ff038086f64e90c9a1f4d16ac3222d8b00919678002372d978deb"
```

