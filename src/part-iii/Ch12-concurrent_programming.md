# 并发编程


在我们了解顺序 Erlang 后，编写并发程序就很容易了。我们只需要三个新的原语：`spawn`、`send` 和 `receive`。`spawn` 会创建一个并行进程，`send` 会发送一条消息给某个进程，`receive` 则会接收消息。


Erlang 的并发基于 *进程*。这是一些小型、独立，可运行 Erlang 函数的虚拟机。


我（作者）敢肯定咱们以前遇到过进程，只不过是在操作系统语境下。*在 Erlang 中，进程属于编程语言而非操作系统*。这意味着 Erlang 的进程，在任何操作系统上，都将有着同样的逻辑行为，因此我们可编写出在任何支持 Erlang 的操作系统上运行，可移植的并发代码。


在 Erlang 中：


- 创建和销毁进程极快；
- 在进程间发送消息极快；
- 进程在所有操作系统上行事方式相同；
- 我们可以有海量的进程；
- 进程不共用内存，而是完全独立；
- 进程交互的唯一方式是消息传递。


出于这些原因，Erlang 有时被称为 *纯消息传递语言*。


若咱们以前没有以进程编程过，咱们可能曾听说进程编程相当困难的谣言。咱们可能曾听说过内存违例、竞赛条件、共享内存损坏等恐怖故事。在 Erlang 中，以进程编程非常简单。

> *知识点*：

- pure message passing language
- memory violations
- race conditions
- shared-memory corruption


## 并发原语


我们曾学过的关于顺序编程的所有知识，对并发编程仍然适用。我们必要要做的，只是增加以下的原语：


- `Pid = spawn(Mod, Func, Args)`
- `Pid = spawn(Fun)`
- `Pid ! Message`
- `receive ... end`
