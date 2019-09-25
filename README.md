# 问题

ckb提供了图灵完备的智能合约能力。但是它采用的是Cell Model（更类似比特币的UTXO Model，而不是以太坊的Account Model）。

Cell Model下开发智能合约非常麻烦，需要开发者了解很多技术细节。主要原因是Cell Model比Account Model更底层，可以认为Account Model（以太坊）是加了一层简单的ORM的，而Cell Model完全没有任何ORM。

另外链下计算，链上验证的方式，使得Dapp的开发分为两部分。

一部分是客户端，要实现完整的Dapp逻辑。从live cell集合中选择合适的cell出来，拿到对应的transaction hash和index，作为inputs。然后根据Dapp逻辑，从inputs计算出对应的outputs，组成transaction。接着对transaction进行签名，可能是一个人签名，也可能是多个人一起签名，还要对签名进行一些组合等操作。最后将交易发送到链上。

另外一个部分是智能合约（ckb里面遵循比特币的习惯叫script）。也要实现完整的Dapp逻辑，对交易的签名和内容进行验证。

我们可以看到，实现一个Dapp需要把同样的逻辑实现两编。目前合约开发用C语言，客户端用ruby或者java。导致门槛比较高，而且实现的时候容易出错。

# 目标

本项目的目的是设计一个框架，使得用户可以不用关心底层技术细节，能够方便快捷的开发一个Dapp。

针对需要把合约逻辑实现两遍的问题，实现了对应的DSL，用户只要使用该DSL来描述Dapp的逻辑，就可以自动生成合约端和客户端代码。

# 依赖

1. Haskell 8.6.5。建议使用[ghcup](https://gitlab.haskell.org/haskell/ghcup)安装。
2. SDK。参见`ruby/README.md`。
3. 合约编译环境。参见`contract/README.md`。

# 模块

代码分为三大部分：

1. contract目录下为合约的依赖和组件代码。
2. ruby目录下是CKB官方Ruby SDK的简单封装。
3. src下为框架本身的haskell代码。

框架定位在SDK之上，主要描述Dapp的逻辑。本身不涉及太多链的技术细节，比如算交易Hash，算LockHash，各种查询等等都是直接调用Ruby代码实现。

`Type`模块是基本的数据结构和类型定义，再加上一些相关的辅助函数。大部分数据结构都实现了Json的序列化和反序列化，以便框架以命令输出文本方式与Ruby SDK进行通信。

`Call`模块实现调用外部命令并获取命令输出，用来实现调用Ruby代码和编译合约功能。

`CodeGen`模块实现了生成合约代码的功能，生成的代码为C代码。

`LockScript`是用来描述lock script逻辑的DSL，它有两个解释器，一个将其解释为client端的交易处理函数；另一个将其解释为验证逻辑，用于生成合约代码。

`EDSL`为上层DSL，用来描述Dapp的流程，比如用户输入，流程选择，基于SDK的各种查询，发送动作。

# 设计

### DSL

选择使用FreeMonad实现EDSL，即Monadic的DSL。

好处是实现简单，灵活，比较适合Poc阶段使用。

其本质就是要把业务逻辑拆分成一系列Operators，这些Operators足够灵活和稳定，能够通过组合满足各种逻辑业务的描述。有点像是构造了一个面向业务逻辑的指令集。

### 上层的DSL

之前思考过Dapp和SDK的关系。有两种方式，一种是SDK做成一个功能丰富的钱包，对于Dapp特有的合约，提供插件来解析/操作Dapp相关的数据；另外一种是SDK做成小的纯封装，每个Dapp是依赖SDK的一个单独应用。

本框架走的是后面这种思路。

因此DSL的设计主要作用是隔离SDK，在设计的过程中，梳理了Dapp对SDK功能需求。

Operators列表如下：

```haskell
data Operator next =
  GetUserInfo Key (UserInfo -> next)
  | GetHDUserInfo Int (UserInfo -> next)
  | LockHash (Hash, HashType, [Arg]) (Hash -> next)
  | QueryLiveCells (Hash, Int) (RetQueryLiveCells -> next)
  | GetLiveCellByTxHashIndex (Hash, Index) (CellWithStatus -> next)
  | DeployContract (UserInfo, Path) (ContractInfo -> next)
  | Sign (UserInfo, Transaction) ([Witness] -> next)
  | SystemScript (ContractInfo -> next)
  | SendTransaction (UserInfo, Transaction) (Hash -> next)
  | SendRawTransaction Transaction (Hash -> next)
  | Ask String (String -> next)
  | Display String next
  | MkDappInfo (Name, Maybe (LockScript())) (DappInfo -> next)
```

userinfo由私钥以及派生出来的包含了pubkey，blake160，address的用户信息。

在CKB本身是不包含任何加密算法的，一切都在合约层实现。Dapp可以在LockScript中使用任意的签名/哈希算法，但是这也会导致用户信息多种多样。

CKB默认提供了一个SystemScript合约。单从合约层面讲，这个合约没有任何特殊的地方。但是它的特殊之处在于，挖矿奖励默认是用这个合约锁定的，也就是说CKB里面所有的原生token一开始都是由这个合约锁定的。这就导致用户在使用Dapp的时候，一定会需要跟系统合约打交道。因此这里就简单处理，用户信息都直接跟系统合约保持一致。

---

Cell Model从简单操作角度来说，跟UTXO是一致的，都可以类比成加锁的箱子。userinfo是钥匙，Sign是解锁，交易是解inputs的旧锁，加outputs的新锁。

因此一个交易首先需要收集input，其有两种来源。

一种是在链上数据里查找，目前CKB只支持用lockhash做关键字查找。而前面说过，框架不想涉及太多技术细节，因此计算lockhash是单独一个Operator，直接调用SDK实现。

查找的Operator就是`QueryLiveCells`，它有两个参数。第一个是前面提到的lockhash，第二个是Capacity。为什么需要Cpacity作为参数？一个原因是最简单的转账交易，会根据需要转账的金额去搜足够的LiveCells，对于复杂的交易，Capacity是为了保证查询不会花费太长时间，起到类似Eth中Gas的作用。

另外一种来源是直接指定某个交易产生的Cell，这个由`GetLiveCellByTxHashIndex`实现，参数是交易哈希和output index。

---

`DeployContract`更多的是用来将一些事先生成好的数据放到链上的一个Cell里。比如Vote里的config data。

Dapp相关的合约其实是由`MkDappInfo`自动生成，编译，然后部署。

---

`Sign` 前面讲过，是SystemScript这个系统合约的解锁方法。

Dapp里的合约，是后部署的，会在部署的时候得到Contract相关的Info。而系统合约是链一开始的时候就已经部署好了，就需要 `SystemScript`专门来获取相关的信息。这也有助于后续将系统合约和Dapp中的合约平等看待，简化组件设计。

 `SendTransaction`是系统合约的解锁函数。

---

`Ask`和`Display`是处理用户输入和一些信息的输出。

---

新增加`GetHDUserInfo`。

现有的代码有个问题，就是部署合约是放在当前账户下的，相当于自己给自己转账。

但是因为部署后的合约也是本账户下的一个LiveCell，后续操作的时候可能会把合约选为input消费掉，导致后面的合约调用失败。

这里想到的一个解决方案是使用HD钱包技术，这样一个seed就可以掌控一系列的账户。 

`GetHDUserInfo`的参数是index，即这一系列账户的下标。seed自动创建并保存在`~/.ckb-generator`目录下。

将合约或者投票等里面有不同类型数据的Cell放置到不同的账户里面。

这样就可以避免无意中将还需要保留的LiveCell花费掉。

### 下层DSL

最开始的时候其实是想在一层DSL里面解决所有问题的。但是后来发现所有的Operator放在一起比较乱，类型会爆炸，因此独立出了单独的DSL。

下层DSL聚焦于生成合约代码和lock script func（其实是解锁+逻辑处理，但是还沿用这个称呼）。

两层之间的分界的标准，就是看构造交易的过程中，哪些内容是需要合约来校验的（下层），哪些是用户可以随便填的（上层）。

比如转账交易，只有witness是合约需要校验的。转账金额（Capacity），收款人（Script）都是用户指定的，其他的字段比如data更是完全随意。

整个客户端的操作其实就是在构造交易。因此先通过交互的方式，让用户填充合约不校验的内容，得到一个不完整的交易。lock script func是一个Transaction->Transaction的函数，其实就是将剩下的部分完成，得到一个完整的，可以直接sendRawTransaction的交易。

---

下层DSL的难点是如何将合约层的校验逻辑和client端对应的计算逻辑用同样的DSL代码描述出来，因为两者差别太大了。

我之前想了两种解决方案：

1. DSL设计偏重于合约层，设计成一种约束语言（就像Prolog），然后client端则是对应约束求解器。
2. DSL设计偏重于client端，描述计算逻辑，然后合约层采用类似Assert的形式，生成校验代码。

这两种方案技术难度都比较大，需要定义一套比较完整的编程语言。

当前为了简化，DSL是Monadic的DSL，因此设计成了组件的形式。

将合约逻辑划分成一些可复用的组件，比如binaryVote，单签，多签等等。

合约只能由这些组件通过And或者Or来组合。

因为一个合约可能是由两种操作合并而成。就像Vote，可以是修改投票，也可以是唱票。另外一个例子是HTLC，锁定的资产，既可以由原来的持有人取回，也可以由对方取走。这就是Or的逻辑。

其实有点类似以太坊，一个合约里面有多个函数，由用户传入FuncHash来选择调用那段处理逻辑。

当前实现简化为每个组件只有一个无参数的入口函数。每次合约执行会把所有的代码都执行一遍，组件自行保证不该它运行的情况，它一定会报错退出，即每次只有一个分支会返回成功，其他分支都返回失败。

And用于一个分支由两个组件组成。比如Vote的唱票操作，就是由binaryVote和多签组成，需要两个组件都返回成功，交易才被接受。

针对每个组件，事先实现好合约层的C代码和client端的交易处理代码。

### 价值流转

如前所述，我们一般把Cell Model下的编程模型设计为加锁解锁。这其实是一种非常低级的模型，难以理解，操作复杂，容易出错。

我觉得更上层的一个模型是`价值流转`。

以Vote为例：

用户投票的时候，其实就是价值从系统Dapp（其实就是系统合约）流转到Vote这个Dapp。

修改投票则是价值从Vote流转到Vote自己。

唱票则是价值从Vote流回系统Dapp。

流转的模式有：

update：在一个Dapp内部流转。当然具体动作会细分成很多情况。比如always_success就不做任何事情；修改投票就是签名。

退出：即transafer，这个就跟业务非常相关了。

这些具体的动作会对应一系列的组件（上层DSL），当然这里面会关联到前面所述的下层DSL的组件。因为价值流转必然会涉及到合约操作。

### 安全

最开始讲到Cell Model是链下计算，链上验证。而且通常链上验证都是采用Assert模式，重复链下计算来校验。

可能会给人一种只要实现一边，另外一边抄下作业就行了的错觉。

但是其实链上链下的关注点非常不一样。

链下需要额外考虑一些用户体验方面的问题。比如转账，如何挑选input就是一门很大的学问，策略不好，可能会产生很多碎片cell等。对应的在安全方面，链下就不需要考虑那么多，哪怕代码是概率性出错也没关系，反正上链的时候错误的数据会被拦下来。

链上计算是确定性的闭包计算。但是对应的安全方面就要非常注意了。因为合约只有一份，但是交易里提交上来的数据可能是各种各样不安全的链下代码生成的。所以一定要处理好各种可能的异常情况。

在这方面其实跟采用Account Model的以太坊并没有太大的区别。

从这方面的考虑，采用组件和DSL设计其实有很多好处。

DSL可以保证用户只能使用框架提供的原语，有点像是一层虚拟机。

组件化，只要每个组件检查自己所需的前提条件，处理自己的异常情况即可。而且组件是通用的，经过各种场景的检验，更容易让安全的组件脱颖而出。

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
Empty data means No, otherwise Yes!
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

