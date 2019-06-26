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

按照第一部分的描述，这里可以分成几个部分。

### cell查询

从live cell集合中选择合适的cell出来，返回结果是inputs数组。会有如下不同的情况：

1. 为了得到指定数量的capacity。过滤条件是lockhash（保证我能花）和capacity总量（大于等于指定的数量，大于的时候后面要生成找零的output）。可能还有些反向的过滤条件，需要跳过一些live cell，比如已经被还未上链的交易挑中的live cell。后端实现可以直接用SDK实现好相应的函数。
2. 寻找指定的live cell。过滤条件是相关的tx hash和index。后端实现可以是封装好的函数。
3. 某个构造好但是尚未上链的交易的output cell。需要交易结构变量和对应的index。后端实现可以是封装好的函数。

很多情况下过滤条件来自要构造的output，所以其实代码流程是先去构造output（主要是capacity，data和args，暂时先不考虑type），然后根据需要提取出过滤条件，再来筛选live cell。

### 业务逻辑

根据inputs里的内容，以及额外的参数，构造出outputs。有如下不同情况：

1. 部署合约的时候，参数是ELF文件的内容。output的data是ELF的二进制内容；capaticy根据data长度计算得到（同时也是查询条件）；lock一般使用钱包的lock（即code hash是system script，args是私钥对应的blake160）
2. setup/vote/lock是一类操作，主要是将已经部署好的合约作为lock加到一个cell上面。再配以其他参数，用户指定的lock args，setup会有用户指定的capacity，vote会有用户指定的data。
3. 唱票，根据inputs的data生成output的data。
4. call，改票，exit和unlock操作。output直接复制input的内容，中间可能有些处理。改票是改data，exit是改lock，unlock是改data和lock。有些是从未上链的交易开始构造的。

### 签名逻辑

使用私钥对交易进行签名，或者对签好的witness进行操作。

1. 简单的签名略过。
2. witness的操作主要是为了多签，合并多个人签出来的witnesses。

### 其他

比如询问用户输入，初始化钱包，发送交易。

# Operator 列表

```haskell
data Operator next =
    SendTx String next
  | BuildTx String (String -> next)
  | GatherTx Int (String -> next)
  | Input (String -> next)
```

