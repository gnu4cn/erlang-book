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


我们曾学过的关于顺序编程的所有知识，对并发编程仍然适用。我们必要要做的，只是增加以下原语：


- `Pid = spawn(Mod, Func, Args)`


会创建一个执行 `apply(Mod, Func, Args)` 的新并发进程。新进程会与调用者并行运行。`spawn` 会返回一个 `Pid`（*process identifier* 缩写）。咱们可使用某个 `Pid` 向该进程发送消息。请注意，带有 `length(Args)` 元数的函数 `Func`，必须要从模组 `Mod` 导出。


在某个新进程被创建时，定义代码的模组 *最新* 版本被使用。


- `Pid = spawn(Fun)`


创建一个运行 `Fun()` 的新并发进程。这种形式的 `spawn` 总是会使用要求值 `fun` 的当前值，而且这个 `fun` 无需从模组导出。

两种形式 `spawn` 的本质区别，与动态代码升级有关。如何在两种 `spawn` 形式中选择，将在 [12.8 节 “以 MFA 或 Fun 下蛋](#以-MFA-或-Fun-下蛋) 中讨论。


- `Pid ! Message`

向标识符为 `Pid` 的进程发送信息。消息发送是异步的。发送者不会等待，而是继续其正在进行的事情。`!` 就是所谓的 *发送* 运算符。


`Pid ! M` 被定义为 `M`。 因此，`Pid1 ！Pid2 ！... ! Msg` 表示将消息 `Msg` 发送给 `Pid1`、`Pid2` 等全部进程。


- `receive ... end`


接收已发送给某个进程的一条信息。其有着如下语法：


```erlang
receive
    Pattern1 [when Guard1] ->
        Expression1;
    Pattern2 [when Guard2] ->
        Expression2;
    ...
end
```


当某条消息到达进程时，系统会尝试将其与 `Pattern1` （在可能的条件 `Guard1` 下）匹配；当匹配成功时，系统会执行 `Expression1`。


当第一个模式不匹配时，系统会尝试 `Pattern2`，以此类推。当没有模式匹配时，该条消息会被保存用于稍后处理，同时进程会等待下一消息。这在 [12.5 小节 “选择性接收”](#选择性接收) 中会详细描述。


接收语句中用到的模式和条件，与我们在定义函数时使用的模式和条件，有着完全相同的语法形式和含义。


就是这样。咱们不需要线程、加锁、信号量及人为控制。


> *知识点*：

- threading
- locking
- semaphore
- artificial control



到目前为止，我们只是简单介绍了 `spawn`、`send` 和 `receive` 的具体工作原理。当某个 `spawn` 命令被执行时，系统会创建出一个新进程。每个进程都有个在该进程被创建时，与之同时创建的关联邮箱。

当咱们将一条消息发送给某个进程时，这条消息会被放入该进程的邮箱。只有在咱们的程序执行一条接收语句时，这个邮箱才会被检查。


使用这三条原语，我们可将 [4.1 节 “模组乃我们存储代码之处](../part-ii/Ch04-modules_and_functions.md#模组乃我们存储代码之处) 中的 `area/1` 函数，重写为一个进程。只是提醒咱们，定义 `area/1` 函数的代码是这样的：


```erlang
{{#include ../../projects/ch12-code/geometry.erl:5:}}
```


现在，我们将把这样行数，重写为一个 *进程*。为完成这一目的，我们会取出作为 `area` 函数参数的三种模式，重新排列他们，形成一个接收语句中的模式。


```erlang
{{#include ../../projects/ch12-code/area_server0.erl}}
```


我们可创建一个在 shell 下运行 `loop/0` 的进程。


```erlang
1> Pid = spawn(area_server0, loop, []).
<0.85.0>
2> Pid ! {rectangle, 6, 10}.
Area of rectangle is 60
{rectangle,6,10}
3> Pid ! {square, 12}.
Area of square is 144
{square,12}
4> Pid ! {circle, 4}.
Area of circle is 50.26544
{circle,4}
```


在第 1 行中，我们创建了个新的并行进程。`spawn(area_server, loop, [])` 创建了个会执行 `area_server:loop()` 的并行进程；他返回了 `Pid`，其被打印作 `<0.85.0>`。


在第 2 行，我们将一条消息发送了给这个进程。这条消息与 `loop/0` 中 `receive` 语句的第一个模式匹配：


```erlang
{{#include ../../projects/ch12-code/area_server0.erl:6:10}}
```


收到一条消息后，该进程打印了矩形的面积。最后，shell 打印了 `{rectangle, 6, 10}`。这是因为 `Pid ！Msg` 的值被定义为 `Msg`。


## 引入客户端-服务器


客户端-服务器架构属于 Erlang 的核心。传统上，客户端-服务器架构涉及某个将客户端与服务器分隔开的网络。大多数情况下，存在多个客户端实例和单个服务器实例。*服务器* 这个词,通常会让人联想到在专门机器上运行的一些重量级软件。


在我们的情形下，涉及到的则是一种更为轻量的机制。客户端-服务器架构中的客户端和服务器，是一些独立进程，同时客户端和服务器间的通信，使用了普通 Erlang 消息传递。客户端和服务器可运行在同一台机器上，或运行在两台不同机器上。


*客户端* 和 *服务器* 这两个词汇，指的是这两个进程扮演的角色；客户端总是通过向服务器发送一个 *请求*，启动一次计算。服务器则会计算出一个回复，并将一次 *响应* 发送给客户端。


我们来编写咱们的首个客户端-服务器应用。我们将以对上一小节中曾编写的程序，做一些小修改开始。


在上一程序中，我们只需将一个请求，发送给接收并打印该请求进程。现在，我们打算做的是，将一次响应发送给发送原始请求的那个进程。问题是我们不知道要向谁发送这个响应。要发送某个响应，客户端就必须包含一个服务器可回复的地址。这就好比寄一封信给某人 -- 当咱们想要得到回复时，咱们最好在信中附上咱们的地址！


因此，消息发送方必须包含一个回复地址。这可通过修改这里完成：


```erlang
Pid ! {rectangle, 6, 10}
```

改为下面这样：


```erlang
Pid ! {self(), {rectangle, 6, 10}}
```


其中 `self()` 是这个客户端进程的 PID。


要响应该请求，我们必须修改接收请求的代码：


```erlang
{{#include ../../projects/ch12-code/area_server0.erl:6:10}}
```


为下面的代码：


```erlang
{{#include ../../projects/ch12-code/area_server1.erl:11:15}}
```


请注意我们现在将咱们的计算结果，发送回由 `From` 参数所标识进程的方式。由于客户端将这个参数，设置为了其自己的进程 ID，因此他将收到结果。


发送请求的进程，通常称为 *客户端*。接收请求并回复客户端的进程，称为 *服务器*。


此外，确保发送给某个进程的每条信息都能被真正接收，是种好的做法。当我们将一条与原始接收语句中的三种模式，都不匹配的消息发送给该进程时，那么该消息最终将进入该进程的邮箱，而永远不会被接收。为处理这一情况，我们要在接收语句末尾，添加了一个保证会匹配发送到该进程任何消息的子句。


最后，我们添加了个名为 `rpc`（*remote procedure call* 的缩写），封装了向某个服务器发送请求，并等待响应的小实用工具。


[`area_server1.erl`](http://media.pragprog.com/titles/jaerlang2/code/area_server1.erl)


```erlang
{{#include ../../projects/ch12-code/area_server1.erl:5:9}}
```


把全部这些放在一起，我们得到下面的代码：



[`area_server1.erl`](http://media.pragprog.com/titles/jaerlang2/code/area_server1.erl)


```erlang
{{#include ../../projects/ch12-code/area_server1.erl}}
```


我们可在 shell 中对此实验。


```erlang
1> Pid = spawn(area_server1, loop, []).
<0.85.0>
2> area_server1:rpc(Pid, {rectangle,6,8}).
48
3> area_server1:rpc(Pid, {circle,6}).
113.09723999999999
4> area_server1:rpc(Pid, {square,6}).
36
5> area_server1:rpc(Pid, socks).
{error,socks}
```

这段代码有个小问题。在函数 `rpc/2` 中，我们将某次请求发往服务器，然后等待一次响应。*但我们等待的不是该服务器的一次响应**，我们在等待任何消息。当这个客户端在等待这个服务器的响应期间，某个别的进程向他发送了一条消息时，他就会这条消息，错误地解析为是来自该服务器的响应。通过将其中的接收语句形式，修改为下面这样：


```erlang
loop() ->
    receive
        {From, ...} ->
            From ! {self(), ...},
            loop()
        ...
    end.
```


并通过将 `rpc` 修改为下面这样，纠正这个问题


```erlang
{{#include ../../projects/ch12-code/area_server2.erl:5:9}}
```

当我们调用函数 `rpc` 时，`Pid` 被绑定到某个值，因此在模式 `{Pid, Response}` 中，`Pid` 是绑定值，`Response` 是未绑定值。这个模式将只匹配包含两个元素元组的消息，其中第一个元素是 `Pid`。所有其他消息都将被排队。（接收提供了叫做 *选择性接收*，我（作者）将在这个小节后介绍）。在这种改变下，我们得到了下面的代码：


[`area_server2.erl`](http://media.pragprog.com/titles/jaerlang2/code/area_server2.erl)

```erlang
{{#include ../../projects/ch12-code/area_server2.erl}}
```


这会如预期那样工作。


```erlang
1> Pid = spawn(area_server2, loop, []).
<0.85.0>
2> area_server2:rpc(Pid, {circle,5}).
78.53975
```


有一项我们可做的最后改进。我们可将 `spawn` 和 `rpc`，*隐藏* 于该模组 *内部*。请注意，我们还必须导出该模组中 `spawn` 的参数（即 `loop/0`）。这是一种好做法，因为我们可在不改变客户端代码的情况下，改变服务器的内部细节。最后，我们得到了这段代码：


[`area_server_final`](http://media.pragprog.com/titles/jaerlang2/code/area_server_final.erl)


```erlang
{{#include ../../projects/ch12-code/area_server_final.erl}}
```

要运行这个程序，我们调用的是 `start/0` 和 `area/2` 函数（之前我们调用的是 `spawn` 和 `rpc`）。这些名称更准确地描述了服务器的功能。

```erlang
1> Pid = area_server_final:start().
<0.85.0>
2> area_server_final:area(Pid, {rectangle, 10, 8}).
80
3> area_server_final:area(Pid, {circle, 4}).
50.26544
```

那么现在我们已经构建了一个简单的客户端-服务器模组。我们只需要三个原语：`spawn`、`send` 和 `receive`。这种模式会以大大小小的变化反复出现，但底层思想始终如一。


## 进程是廉价的


