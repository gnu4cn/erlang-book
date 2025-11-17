# SEE 实现细节


启动一切的 shell 脚本（`see`） 如下：


```sh
{{#include ../../../projects/appendix/see.sh}}
```


> **译注**：译者在以命令 `sh see.sh` 运行这个 shell 脚本时，得到的输出如下：
>
> ```console
> $ sh see.sh
> (no logger present) unexpected logger message: {log,error,"Error in process ~p with exit value:~n~p~n",[<0.10.0>,{{case_clause,{ok,[[]]}},[{see,get_module_name,0,[{file,"/home/hector/erlang-book/projects/appendix/src/see.erl"},{line,256}]},{see,main,0,[{file,"/home/hector/erlang-book/projects/appendix/src/see.erl"},{line,46}]},{init,eval_script,2,[]},{init,do_boot,3,[]}]}],#{error_logger=>#{emulator=>true,tag=>error},pid=><0.10.0>,time=>1763345772061178,gl=><0.0.0>}}
> Runtime terminating during boot ({{case_clause,{ok,[[]]}},[{see,get_module_name,0,[{file,"/home/hector/erlang-book/projects/appendix/src/see.erl"},{line,256}]},{see,main,0,[{file,"/home/hector/erlang-book/projects/appendix/src/see.erl"},{line,46}]},{init,eval_script,2,[]},{init,do_boot,3,[]}]})
>
> Crash dump is being written to: erl_crash.dump...done
> ```


## SEE 的主程序

当系统启动时，`see:main()` 会被执行，而当这个函数终止时，那么系统将停止运行。`see:main()` 代码如下：


```erlang
{{#include ../../../projects/appendix/src/see.erl:34:48}}
```


他会启动五个服务器（`io`、`code` ......），加载错误处理器，找出要运行模组的名字，加载该模组，然后运行该模组中的代码。`run(Mod)` 会生成链接 `Mod:main()`，并等待其终止。当其终止时，`stop_system` 会被调用。


```erlang
{{#include ../../../projects/appendix/src/see.erl:50:52}}
```

这个函数 `main/0` 会启动许多不同服务器。在详细介绍各个服务器工作原理前，我们将看看用于构建这些客户端服务器的通用框架。


## SEE 中的客户端服务器模式

要创建某个服务器，我们就要调用 `make_server(Name,Fun1,Fun2)`。`Name` 是该服务器的全局名字，`Fun1()` 预期要返回 `State1`，即该服务器的初始状态。当 `Fun2(State, Query)` 以一个远程过程调用被调用时，其应返回 `{Reply, State1}`；或在以一个 `cast` 调用时，其应只返回 `State1`。`make_server` 代码如下：

```erlang
{{#include ../../../projects/appendix/src/see.erl:154:159}}
```


其中的服务器循环如下：


```erlang
{{#include ../../../projects/appendix/src/see.erl:161:182}}
```


要查询服务器，我们就要使用 `rpc`（*远程过程调用* 的简称），其代码如下：

```erlang
{{#include ../../../projects/appendix/src/see.erl:184:191}}
```


请注意服务器循环与 `rpc` 中代码的交互方式。服务器中的处理器函数受 `catch` 保护，当服务器上某个异常抛出时，一条 `{Name,exit,Why}` 消息会被发送回客户端。当该消息被客户端接收到时，客户端上一条异常会通过执行 `exit(Why)` 抛出。


这样做的最终结果，便是在客户端上抛出一个异常。要注意的是，在客户端向服务器发送了一次服务器无法处理的查询情形下，服务器会继续保持其原有状态。


因此，就服务器而言，这些远程过程调用的函数，就如同一些 *事务*。要么他们全部工作，要么服务器会回滚到该次远程过程调用前的状态。

当我们只打算发送一条信息到服务器，而对回复不感兴趣时，我们会调用 `cast/2`。


```erlang
{{#include ../../../projects/appendix/src/see.erl:193:194}}
```


通过发送给服务器在服务器循环中使用的不同 fun，我们便可改变其行为。


```erlang
{{#include ../../../projects/appendix/src/see.erl:196:197}}
```

回顾当我们启动服务器时，初始数据结构通常是个常量。我们可定义返回某个在求值时返回 `C` 的函数 `const(C)`。


```erlang
{{#include ../../../projects/appendix/src/see.erl:199}}
```


现在，我们把注意力转向那些单个的服务器。


## 代码服务器


代码服务器是通过执行下面的函数启动的：


```erlang
{{#include ../../../projects/appendix/src/see.erl:154:159}}
```


其中 `load_module(Mod)` 被实现为到这个代码服务器的一个远程过程调用。


```erlang
{{#include ../../../projects/appendix/src/see.erl:54:55}}
```


代码服务器的全局状态，仅为 `[Mod]`，即所有已被加载模组的列表。(这个列表的初始值为 `[init, erl_prim_loader]`。这些模组属于 *预加载* 并被编译到 Erlang 运行时系统的内核中。）


该服务器的处理器函数 `see:handle_code/2` 如下：

```erlang
{{#include ../../../projects/appendix/src/see.erl:60:73}}
```

而 `primLoad/1` 完成了加载：

```erlang
{{#include ../../../projects/appendix/src/see.erl:75:89}}
```


## 错误日志记录器


`log_error(What)` 函数会记录错误 `What` 于标准输出上；这被实现为一个 `cast`。


```erlang
{{#include ../../../projects/appendix/src/see.erl:91}}
```


对应的服务器处理器函数如下：


```erlang
{{#include ../../../projects/appendix/src/see.erl:93:95}}
```


请注意，这个错误处理器的全局状态，是个表示已发生错误总数的整数 `N`。

## 挂起问题

**The Halt Demon**<sup>1</sup>



当系统挂起时，这个挂起函数/问题即被调用。执行 `on_halt(Fun)` 会建立一种在系统挂起时，`Fun()` 将被执行的一种条件。将系统挂起，是经由调用函数 `stop_system()` 完成的。

> **译注**：the halt demon 疑似 halting problem 的误解。或者叫挂起守候进程/循环。
>
> 参考：
>
> - [Halting problem](https://en.wikipedia.org/wiki/Halting_problem)


```erlang
{{#include ../../../projects/appendix/src/see.erl:97:98}}
```


这方面的服务器处理器代码如下：


```erlang
{{#include ../../../projects/appendix/src/see.erl:100:109}}
```


## I/O 服务器

I/O 服务器允许对 STDIO 的。`read()` 会读取标准输入上的一行，而 `write(String)` 会将一个字符串写到标准输出。


```erlang
{{#include ../../../projects/appendix/src/see.erl:111:112}}
```


I/O 服务器的初始状态，是经由执行 `start_io/0` 获得的。


```erlang
{{#include ../../../projects/appendix/src/see.erl:114:117}}
```


同时 I/O 处理器如下：

```erlang
{{#include ../../../projects/appendix/src/see.erl:119:134}}
```


I/O 服务器的状态为 `{Flag, Port}`，其中当 `eof` 已遇到时 `Flag` 便为 `true`；否则其为 `false`。


## 环境服务器

函数 `env(E)` 被用于找出环境变量 `E` 的值。


```erlang
{{#include ../../../projects/appendix/src/see.erl:136}}
```


该服务器如下：

```erlang
{{#include ../../../projects/appendix/src/see.erl:138:139}}
```

该服务器的初始状态，由执行以下函数找到：

```erlang
{{#include ../../../projects/appendix/src/see.erl:141:149}}
```


## 全局进程的支持

我们需要保持进程存活，以及注册一些全局名字的几个例程。

`keep_alive(name, Fun)` 会创建一个名为 `Name` 的注册进程。其经由执行 `Fun()` 启动，且当该进程死亡时，其会被自动重启。


```erlang
{{#include ../../../projects/appendix/src/see.erl:201:204}}
```


`make_global(Name,Fun)` 会检查是否存在一个注册名为 `Name` 的全局进程。当没有进程，他会生成一个执行 `Fun()` 的进程，并将其以 `Name` 注册。


```erlang
{{#include ../../../projects/appendix/src/see.erl:206:227}}
```


## 进程的支持


`on_exit(Pid, Fun)` 会链接到 `Pid`。当（进程） `Pid` 以原因 `Why` 退出时，那么 `Fun(Why)` 会被执行。


```erlang
{{#include ../../../projects/appendix/src/see.erl:229:237}}
```

`every(Pid,Time,Fun)` 亦会链接到 `Pid`；然后每隔一段时间 `Time` 执行一次 `Fun()`。当 `Pid` 退出时，则该进程会停止。


```erlang
{{#include ../../../projects/appendix/src/see.erl:239:253}}
```


## 实用工具


`get_module_name()` 会获取命令行上的模组名字。

```erlang
{{#include ../../../projects/appendix/src/see.erl:255:261}}
```

