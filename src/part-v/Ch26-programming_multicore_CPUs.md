# 多核 CPU 编程

![现代 CPU](../images/CPU.png)

*我们怎样编写在多核心 CPU 上运行更快的程序呢？这一切都与可变状态和并发有关*。

在过去（二十多年前），有两种并发模型。

- 共享状态的并发
- 消息传递的并发

编程界走了一条路（朝着共享状态）。Erlang 社区则走的是另一条路。(很少有其他语言循着 “消息传递的并发” 这条道路；这些别的语言有 [Oz](https://en.wikipedia.org/wiki/Oz_(programming_language)) 及 [Occam](https://en.wikipedia.org/wiki/Occam_(programming_language))）。





## 并行序列代码

