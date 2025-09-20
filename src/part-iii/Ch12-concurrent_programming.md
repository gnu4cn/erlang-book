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
>
>- pure message passing language
>- memory violations
>- race conditions
>- shared-memory corruption


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
>
>- threading
>- locking
>- semaphore
>- artificial control



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

此时，咱们可能会担心性能问题。毕竟，当我们正创建成百上千个 Erlang 进程时，我们肯定在付出一定代价。我们来看看要付出多少代价。


我们将执行一系列下蛋，创建出大量进程，并计算其要多长时间。下面是这个程序；请注意，这里我们使用了 `spawn(Fun)`，而且被生出的函数不必从该模组中导出：


[`processes.erl`](http://media.pragprog.com/titles/jaerlang2/code/processes.erl)


```erlang
{{#include ../../projects/ch12-code/processes.erl}}
```


下面是我（作者）在目前使用的一台 2.90GHz 英特尔酷睿 i7 双核处理器，8GB 内存，运行 Ubuntu 的电脑上，获取到的结果：

```erlang
1> processes:max(20000).
Maximum allowed processes: 262144
Process spawn time = 3.0 (3.4) microseconds
ok
2> processes:max(30000).
Maximum allowed processes: 262144

=ERROR REPORT==== 14-May-2013::09:32:56 ===
Too many processes

** exception error: a system limit has been reached
...
```


> **译注**：译者在一台 Interl Core i5-8260，8GB 内存，Windows 11 Enterprise 计算机上的结果如下。
>
>```erlang
>1> processes:max(20000).
>Maximum allowed processes: 1048576
>Process spawn time = 0.75 (1.55) microseconds
>ok
>2> processes:max(3000000).
>Maximum allowed processes: 1048576
>
>=ERROR REPORT==== 16-Sep-2025::10:52:31.083000 ===
>Too many processes
>
>Error in process <0.27263048.5> with exit value:
>{system_limit,[{erlang,spawn_link,
>                       [erlang,apply,[#Fun<shell.1.25725971>,[]]],
>                       [{error_info,#{module => erl_erts_errors}}]},
>               {erlang,spawn_link,1,[{file,"erlang.erl"},{line,10461}]},
>               {shell,get_command,6,[{file,"shell.erl"},{line,459}]},
>               {shell,server_loop,8,[{file,"shell.erl"},{line,338}]}]}
>...
>```

生成 20,000 个进程平均需要 3.0us/进程的 CPU 时间和 3.4us 的延时（挂钟）时间。


请注意，我（作者）使用了 `erlang:system_info(process_limit)` 这个 BIF， 找到允许的最大进程数。其中一些进程是保留的，所以咱们的程序实际上无法用到这个数量。当我们超过这个系统限制时，系统就会拒绝启动更多进程，并生成一份错误报告（第 2 条命令）。


系统限制被设置为 262 144 个进程；要超过此限制，咱们必须以 `+P` 开关启动 Erlang 仿真器，如下所示：


```erlang
$ erl +P 3000000
Erlang/OTP 28 [erts-16.0.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Hi, I'm in your .erlang file
Eshell V16.0.2 (press Ctrl+G to abort, type help(). for help)
1> processes:max(500000).
Maximum allowed processes: 4194304
Process spawn time = 0.532 (2.61) microseconds
ok
2> processes:max(1000000).
Maximum allowed processes: 4194304
Process spawn time = 0.609 (2.925) microseconds
ok
3> processes:max(2000000).
Maximum allowed processes: 4194304
Command is taking a long time, type Ctrl+G, then enter 'i' to interrupt
Process spawn time = 0.898 (4.103) microseconds
ok
4> processes:max(3000000).
Maximum allowed processes: 4194304
Command is taking a long time, type Ctrl+G, then enter 'i' to interrupt
Process spawn time = 1.2033333333333334 (4.601) microseconds
ok
```

在前面的示例中，实际选取的值，是大于所提供参数的下一个最大的 2 的幂。实际值可以通过调用 `erlang:system_info(process_limit)` 获取到。我们可以看到，随着我们增加进程数量，进程的生成时间也在增加。当我们继续增加进程数量时，我们将达到我们耗尽物理内存的某个点，系统将开始把物理内存交换到磁盘，而运行速度将大幅减慢。


当咱们正编写某个用到大量进程的程序时，那么最好找出在系统开始将内存交换到磁盘前，物理内存可容纳多少个进程，从而确保咱们的程序将运行在物理内存中。


正如咱们所看到的，创建大量进程相当快。若咱们是名 C 或 Java 程序员，咱们可能会犹豫是否要使用大量进程，而且咱们还必须考虑管理他们。在 Erlang 中，创建进程简化了编程，而不是令其复杂化。


## 带超时的接收


有时，某个接收语句可能会一直等待一条，永不会到来的消息。这可能有数种原因。例如，我们的程序种可能有逻辑错误，或者要向我们发送消息的进程，可能在其发送消息前崩溃了。为避免这个问题，我们可将一个超市，添加到接收语句。这会设置一个该进程将等待的接收消息最长时间。该语法如下：


```erlang
receive
    Pattern1 [when Guard1] ->
        Expressions1;
    Pattern2 [when Guard2] ->
        Expressions2;
    ...
after Time ->
          Expressions
end
```


当在进入接收表达式后的 `Time` 毫秒内，没有匹配的消息到达时，那么该进程将停止等待消息，并计算 `Expressions`。


### 只有超时的接收


咱们可以编写个只包含超时的 `receive` 语句。利用这点，我们可以定义一个暂停当前进程 `T` 毫秒的函数 `sleep(T)`。


[`lib_misc.erl`](http://media.pragprog.com/titles/jaerlang2/code/lib_misc.erl)

```erlang
{{#include ../../projects/ch12-code/lib_misc.erl:98:101}}
```


### 带超时值零的接收


`0` 的超时值，会导致超时的主体立即发生，但在此之前，系统会尝试匹配进程邮箱中任何的模式。我们可以利用这点，定义一个可彻底清空进程邮箱中全部消息的函数 `flush_buffer()`。


[`lib_misc.erl`](http://media.pragprog.com/titles/jaerlang2/code/lib_misc.erl)


```erlang
{{#include ../../projects/ch12-code/lib_misc.erl:104:108}}
```


在没有其中的超时子句下，`flush_buffer` 就会永远暂停，而不会在进程邮箱为空时返回。我们还可以使用零的超时，实现某种形式的 “优先接收” ，如下所示：


[`lib_misc.erl`](http://media.pragprog.com/titles/jaerlang2/code/lib_misc.erl)


```erlang
{{#include ../../projects/ch12-code/lib_misc.erl:110:}}
```


当进程邮箱中 *没有* 与 `{alarm, X}` 匹配的消息时，则 `priority_receive` 将接收邮箱中的第一条信息。当邮箱中没有任何信息时，他将暂停于接收尽头，并返回其收到的第一条信息。当有条与 `{alarm, X}` 匹配的消息时，那么该消息会立即返回。请记住，只有在对邮箱中的所有条目，执行模式匹配后，`after` 小节才会被检查。


若没有这个 `after 0` 语句，那么告警消息将不会被首先匹配。


*注意*：在优先接收下使用大型邮箱，效率相当低，因此当咱们准备运用这种技术时，要确保咱们的进程邮箱不太大。


### 带无穷大超时值的接收

当接收语句中的超时值为 `infinity` 这个 原子时，那么该超时将 *永不* 会触发。这对于那些在接收语句外计算超时值的程序，可能很有用。有时，该计算可能打算返回一个具体超时值，而其他时候则可能想要让接收永远等待。



### 实现一种计时器


运用接收超时，我们可实现一种简单的计时器。

函数 `stimer:start(Time, Fun)` 将在 `Time` ms 后，计算 `Fun`（一个零参数的函数）。他会返回一个在必要时可用于取消该计时器的句柄（一个 PID）。


[`stimer.erl`](http://media.pragprog.com/titles/jaerlang2/code/stimer.erl)


```erlang
{{#include ../../projects/ch12-code/stimer.erl}}
```


我们可如下测试这个程序：


```erlang
1> Pid = stimer:start(5000, fun() -> io:format("timer event~n") end).
<0.85.0>
timer event
```


这里我（作者）等了五秒钟以上，因此计时器会触发。现在，我将启动一个计时器，并在计时结束前取消他。

```erlang
2> Pid1 = stimer:start(25000, fun() ->io:format("timer event~n") end).
<0.87.0>
3> stimer:cancel(Pid1).
cancel
```


超时与定时器，是许多通信协议实现的核心。当我们等待某条消息时，我们不希望永远等待下去，因此我们会如同在示例中那样，添加一个超时。



## 选择性接收

`receive` 这个原语，被用于提取进程邮箱中的消息，但他所做的不仅仅是简单模式匹配；他还会将那些未匹配消息排成队列用于稍后处理，并管理超时。下面的语句：


```erlang
receive
    Pattern1 [when Guard1] ->
        Expressions1;
    Pattern2 [when Guard2] ->
        Expressions2;
    ...
after
    Time ->
        ExpressionsTimeout
end
```


会如下工作：


1. 当我们输入某个 `receive` 语句时，我们就启动了个计时器（但只有在表达式中有个 `after` 小节时）；

2. 取进程邮箱中的第一条消息，并尝试将其与 `Pattern1`、`Pattern2` 等匹配。当匹配成功时，这条消息将自邮箱种移除，同时该模式后的表达式会被计算；

3. 当这个 `receive` 语句中没有与邮箱中第一条消息匹配的模式时，那么这第一条信息会从邮箱中移除，并放入一个 “保存队列”。然后尝试进程邮箱中的第二条信息。这个过程会一直重复进行，直到找到一条匹配消息，或邮箱中的所有消息检查完毕；

4. 当邮箱中的消息都不匹配时，那么这个进程将暂停，并在下次有新消息放入邮箱时，重新安排执行时间。当有新信息到达时，保存队列中的信息不会被重新匹配；而只有这条新消息才会被匹配；


5. 一旦匹配到某条消息，那么已放入保存队列的所有信息，都会按他们到达进程的顺序，重新进入邮箱。当设置了计时器时，则计时器会被清零；


6. 当计时器在我们等待某条消息时超时，则会计算表达式 `ExpressionsTimeout`，并会把全部保存的消息，按照他们到达进程的顺序放回邮箱。



## 注册的进程


当我们打算向某进程发送一条消息时，我们就需要知道他的 PID，但在进程创建时，只有父进程才知道该 PID。系统中的其他进程，都不清楚该进程。通常这会带来不便，因为这个 PID 必须要被发送给系统中，所有希望与该进程通信的进程。另一方面，这也非常 *安全*；当咱们没有透露某个进程的 PID 时，其他进程就无法以任何方式与之交互。


Erlang 有种 *发布* 进程标识符的方法，这样系统中任意进程，都可以与这个进程通信。这样的进程被称为 *注册的进程*。有四个用于管理注册进程的 BIF。


- `register(AnAtom, Pid)`

    以名字 `AnAtom` 注册进程 `Pid`。当 `AnAtom` 已用于注册某个进程，该次注册就会失败。

- `unregister(AnAtom)`

    移除任何与 `AnAtom` 关联的注册。

    *注意*：当某个已注册的进程死亡时，他将被自动取消注册。


- `whereis(AnAtom) -> Pid | undefined`

    查明 `AnAtom` 是否已被注册。返回进程标识符 `Pid`，或在没有进程与 `AnAtom` 关联时，返回原子 `undefined`。

- `registered() -> [AnAtom::atom()]`

    返回系统中所有已注册进程的列表。


使用 `register`，我们可修订 [`area_server0.erl` 代码]() 中的示例，同时我们可尝试注册我们所创建进程的名字。


```erlang
1> Pid = spawn(area_server0, loop, []).
<0.85.0>
2> register(area, Pid).
true
```

名称注册了后，我们就可以如下，发送给他消息：


```erlang
3> area ! {rectangle, 4, 5}.
Area of rectangle is 20
{rectangle,4,5}
```



我们可使用 `register`，创建一个表示时钟的注册进程。


[`clock.erl`](http://media.pragprog.com/titles/jaerlang2/code/clock.erl)


```erlang
{{#include ../../projects/ch12-code/clock.erl}}
```


这个时钟会欢快地滴答作响，直到咱们将他停下。


```erlang
6> clock:start(5000, fun() -> io:format("TICK ~p~n", [erlang:now()]) end).
true
TICK {1758,73828,577785}
TICK {1758,73833,590368}
TICK {1758,73838,593939}
TICK {1758,73843,607648}
TICK {1758,73848,619206}
7> clock:stop().
stop
```


## 尾部递归略记


请看早先我们编写的面积服务器中那个接收循环：


[`area_server_final.erl`](http://media.pragprog.com/titles/jaerlang2/code/area_server_final.erl)



```erlang
{{#include ../../projects/ch12-code/area_server_final.erl:16:30}}
```


当咱们仔细看时，就会发现当我们每次收到一条消息时，都会处理这条消息，然后立即再次调用 `loop()`。这样的过程,叫做 *尾递归，tail-recursive*。尾递归函数可被编译，从而某个语句序列中的最后一次函数调用，可被替换为简单跳转至被调用函数的起始位置。这意味着尾递归函数，可在不消耗栈空间下，永久循环。


设想我们写的是如下（不正确）代码：


```erlang
loop() ->	                                        %% Line 1
    receive
        {From, {rectangle, Width, Ht}} ->
            From ! {self(), Width*Ht},
            loop(),	                                %%      5
            someOtherFunc();
        {From, {circle, R}} ->
            From ! {self(), 3.14159 * R * R},	    %%	   10
            loop();
            ...
    end
end
```


在第 5 行处，我们调用了 `loop()`，但编译器必须考虑到 “在调用 `loop()` 之后，我必须返回到这里，因为我必须在第 6 行调用 `someOtherFunc()`”。因此，编译器会将 `someOtherFunc` 的地址压入栈上，然后跳至 `loop` 的起点。这样做的问题是，`loop()` 永不会返回；相反，他只会永远循环下去。因此，当我们每次经过第 5 行时，就会有另一个返回地址被压入控制栈，而最终系统会耗尽内存空间。


要避免这种情况很容易；当咱们要编写一个永不返回的函数 `F`（如 `loop()`）时，要确保咱们在调用 `F` *后*，不再调用任何函数，且不要在列表或元组构造函数中使用 `F`。


> **并发程序模板**
>
> 当我（作者）要编写某个并发程序时，我几乎总是以下面这样的代码开始：
>
>```erlang
>-module(ctemplate).
>-compile(export_all).
>
>
>start() ->
>    spawn(?MODULE, loop, []).
>
>
>rcp(Pid, Request) ->
>    Pid ! {self(), Request},
>    receive
>        {Pid, Response} -> Response
>    end.
>
>
>loop(X) ->
>    receive
>        Any ->
>            io:format("Received: ~p~n", [Any]),
>            loop(X)
>    end.
>```
>
> 其中的接收循环，只是个接收并打印我发送给他任何信息的任意空循环。随着我不断开发程序，我将开始发送消息到进程。由于我以接收循环中没有与这些信息匹配的模式开始，因此我将从该接收语句底部的代码处，得到打印输出。当这种情况发生时，我就会将某个匹配模式添加到接收循环，并重新运行程序。这种技巧在很大程度上决定了我编写程序的顺序：我会首先编写一个小的程序，然后慢慢扩大他，在编写时测试。



## 以 MFAs 或 Funs 生成进程


以明确模组、函数名及参数列表（称为 MFA）生成一个函数，是确保在模组被使用期间，当其被编译时，咱们的运行进程将以模组代码最新版本，正确更新的适当方式。动态代码升级机制，不适用于生成的 funs。其只适用于显式命名的 MFA。详情请参阅 [8.10 节 *动态代码加载*](../part-ii/Ch08-the_rest_of_sequential_erlang.md#动态代码加载)。


当咱们不关心动态代码升级时，或者咱们确信咱们的程序将来永不会被修改时，那就使用 `spawn(Fun)` 形式的 `spawn`。在存疑时，就要使用 `spawn(MFA)`。


就是这样 -- 咱们现在可以编写并发程序了！


接下来，我们将学习错误恢复，看看我们如何利用另外三个概念，编写容错的并发程序：链接、信号与捕获进程的退出。这就是我们将在下一章看到的内容。


## 练习


1. 请编写一个函数 `start(AnAtom, Fun)`，将 `AnAtom` 注册为 `spawn(Fun)`。要确保咱们的程序在两个并行进程同时运行 `start/2` 时正常运行。在这种情况下，咱们必须保证其中一个进程成功，另一进程失败；

2. 请使用 [12.3 小节 *进程是廉价的*](#进程是廉价的) 中的程序，测量咱们自己机器上的进程生成时间。请绘制进程数量与进程创建时间的图形。咱们可从图中，得出什么结论？


3. 请编写一个环基准测试。在一个环中创建 `N` 个进程。沿该环上发送一条消息 `M` 次，因此总共有 `N * M` 条消息被发送。根据 `N` 和 `M` 的不同值，计算此操作所需时间。

    请用咱们熟悉的其他编程语言，编写一个类似程序。并比较结果。请撰写一篇博客，并将结果发布到互联网上！
