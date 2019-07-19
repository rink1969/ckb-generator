# 问题

ckb提供了图灵完备的智能合约能力。但是它采用的是Cell Model（更类似比特币的UTXO Model，而不是以太坊的Account Model）。

Cell Model下开发智能合约非常麻烦，需要开发者了解很多技术细节。主要原因是Cell Model比Account Model更底层，可以认为Account Model（以太坊）是加了一层简单的ORM的，而Cell Model完全没有任何ORM。

另外链下计算，链上验证的方式，使得Dapp的开发分为两部分。

一部分是客户端，要实现完整的Dapp逻辑。从live cell集合中选择合适的cell出来，拿到对应的transaction hash和index，作为inputs。然后根据Dapp逻辑，从inputs计算出对应的outputs，组成transaction。接着对transaction进行签名，可能是一个人签名，也可能是多个人一起签名，还要对签名进行一些组合等操作。最后将交易发送到链上。

另外一个部分是智能合约（ckb里面遵循比特币的习惯叫script）。也要实现完整的Dapp逻辑，对交易的签名和内容进行验证。

我们可以看到，实现一个Dapp需要把同样的逻辑实现两编。目前合约开发用C语言，客户端用ruby或者java。导致门槛比较高，而且实现的时候容易出错。

# 目标

本项目的目的是设计一个框架，使得用户可以不用关心底层技术细节，能够方便快捷的搭建一个Dapp。

针对需要把合约逻辑实现两遍的问题，实现了对应的DSL，用户只要使用该DSL来描述Dapp的逻辑，就可以自动生成合约端和客户端代码。

# 依赖

1. SDK。依赖ckb的ruby sdk，框架本身没有太多链相关的技术细节，比如算交易Hash，算LockHash，各种查询等等都是直接调用SDK。
2. riscv交叉编译工具链。框架生成的合约代码为C代码，需要编译成ELF。

# 示例

目前有两个示例：

1. always_success 最简单的合约，直接返回成功。
2. vote 投票加多签的示例。

### AlwaysSuccess

代码在 `src/Dapp/AlwaysSuccess.hs`

Dapp流程设计如下：

1. 从SystemScript中转移一些Capacity（挖矿得到）到always_success合约中
2. 自己给自己转账，循环。。。

实际运行情况如下：

```haskell
cabal new-repl
*Type> :m Dapp.AlwaysSuccess 
Prelude Dapp.AlwaysSuccess> runAlwaysSuccess 
Begin to run Dapp always_sucess!
Deploy contract always_success!
Please input privkey:
<<<user input>>> 0x...
Move some capacity from system_script to contract always_sucess!
Please input sender user privkey:
<<<user input>>> 0x...
Please input Arg or Input "e" to end of input
<<<user input>>> 0x4a88cef22e4e71c48c40da51c1d6bd16daa97aa7
Please input Arg or Input "e" to end of input
<<<user input>>> e
input capacity
<<<user input>>> 100000000000
data in output
<<<user input>>> 
Start loop call contract always_sucess!
"0x71788b6fb496b26950834ad58845055e7469c973a5c994c9619c329567787b61"
Loop call contract always_sucess!
Press Enter to continue...Input "e" to exit loop...
<<<user input>>>
"0xda795ab4e0a184ae8d0f4872c8226e9fb4a44969413a3b4c72b080ecdd51816b"
Loop call contract always_sucess!
Press Enter to continue...Input "e" to exit loop...
e
Just "0xda795ab4e0a184ae8d0f4872c8226e9fb4a44969413a3b4c72b080ecdd51816b"
```

### Vote

代码在`src/Dapp/Vote.hs`

Dapp流程设计如下：

1. 三个帐号 Voter1/Voter2/Voter3，保证这些帐号里有一些ckb。
2. 使用多签来进行唱票，使用以上三个帐号进行2/3多签。
3. 三个帐号分别从SystemScript中转移一些Capacity到vote合约。转账交易中的data为空表示投No，不为空表示投Yes。
4. 唱票交易是将以上三笔投票交易作为inputs，output的data里存放唱票结果。两个Word8分别表示总票数和投Yes的票数。
5. 三个帐号分别构造唱票交易，并分别对其签名。
6. 最后将签名汇聚到一起，形成一个多签交易，然后发送到链上。三人中有两个人唱票结果一致才可以验证通过上链。

实际运行情况如下：

```haskell
cabal new-repl
*Type> :m Dapp.Vote
Prelude Dapp.Vote> buildConfigData
Prelude Dapp.Vote> runVote 
Begin to run Dapp vote!
Deploy config data (Make sure has run buildConfigData)
privkey
<<<user input>>> 0x...
Deploy contract vote
Please input privkey:
<<<user input>>> 0x...
Begin to vote!
Empty data means No, otherwise Yse!
Voter1 ready to vote!
Please input sender user privkey:
<<<user input>>> 0x...   -- privkey of Voter1
Please input Arg or Input "e" to end of input
<<<user input>>> 0x4a88cef22e4e71c48c40da51c1d6bd16daa97aa7  -- blake160 of Voter1
Please input Arg or Input "e" to end of input
<<<user input>>> e
input capacity
<<<user input>>> 20000000000
data in output
<<<user input>>>                   -- Voter1 vote No
Voter2 ready to vote!
Please input sender user privkey:
<<<user input>>> 0x...   -- privkey of Voter2
Please input Arg or Input "e" to end of input
<<<user input>>> 0xa47f8029997fcc67aff87384daac404f39e31ceb  -- blake160 of Voter2
Please input Arg or Input "e" to end of input
<<<user input>>> e
input capacity
<<<user input>>> 20000000000
data in output
<<<user input>>> bb               -- Voter2 vote Yes
Voter3 ready to vote!
Please input sender user privkey:
<<<user input>>> 0x...   -- privkey of Voter3
Please input Arg or Input "e" to end of input
<<<user input>>> 0x96f4093cf179aaa369379402d74f70090fae11ec  -- blake160 of Voter3
Please input Arg or Input "e" to end of input
<<<user input>>> e
input capacity
<<<user input>>> 20000000000
data in output
<<<user input>>>                  -- Voter3 vote No
Voter1 want to modify his vote!
input new vote data:
<<<user input>>> aa               -- Voter1 change to vote Yes
privkey
<<<user input>>> 0x...   -- privkey of Voter1
We need complete Multi-Signatures!
Voter1 sign for sum_tx!
privkey
<<<user input>>> 0x...   -- privkey of Voter1
Voter2 sign for sum_tx!
privkey
<<<user input>>> 0x...   -- privkey of Voter2
Voter3 sign for sum_tx!
privkey
<<<user input>>> 0x...   -- privkey of Voter3
Just "0x05a7651dccc2863416db3a0a5c35f71ad6bc19bd7f9ec7a4f0b9e2ac7bb4cd93"
```

