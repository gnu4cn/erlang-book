# 一个套接字应用

这个附录专门介绍 [使用 `lib_chan` 控制进程](../part-iii/Ch14-distributed_programming.md#使用-lib_chan-控制进程) 中，提到的 `lib_chan` 这个库的实现。`lib_chan` 的代码实现了 TCP/IP 上的一个完整网络通信层，提供身份验证与 Erlang 项的流传输两种功能。在我们掌握 `lib_chan` 中用到的设计原理后，我们应能按需定制我们自己的通信基础设施，并将其作为 TCP/IP 上的一层。

就其本身而言，`lib_chan` 是个构建分布式系统的有用组件。


为使这个附录自成一体，其中有一些与 [使用`lib_chan` 控制进程](../part-iii/Ch14-distributed_programming.md#使用-lib_chan-控制进程) 中不少重复的材料。


此附录中的代码，是我（作者）迄今为止引入的最复杂代码之一，因此当咱们在初次阅读时无法完全理解时，请无焦虑。当咱们只打算使用 `lib_chan` 这个库，而不关心其工作原理时，那么就阅读第一个小节，并跳过其余内容。
