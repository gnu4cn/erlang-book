# 一个套接字应用

这个附录专门介绍 [使用 `lib_chan` 控制进程](../part-iii/Ch14-distributed_programming.md#使用-lib_chan-控制进程) 中，提到的 `lib_chan` 这个库的实现。`lib_chan` 的代码实现了 TCP/IP 上的一个完整网络通信层，提供身份验证与 Erlang 项的流传输两种功能。在我们掌握 `lib_chan` 中用到的设计原理后，我们应能按需定制我们自己的通信基础设施，并将其作为 TCP/IP 上的一层。

就其本身而言，`lib_chan` 是个构建分布式系统的有用组件。


为使这个附录自成一体，其中有一些与 [使用`lib_chan` 控制进程](../part-iii/Ch14-distributed_programming.md#使用-lib_chan-控制进程) 中不少重复的材料。


此附录中的代码，是我（作者）迄今为止引入的最复杂代码之一，因此当咱们在初次阅读时无法完全理解时，请无焦虑。当咱们只打算使用 `lib_chan` 这个库，而不关心其工作原理时，那么就阅读第一个小节，并跳过其余内容。


## 一个示例


我们将从一个展示如何使用 `lib_chan` 的简单示例开始。我们将创建一个能够计算阶乘和斐波那契数的简单服务器。我们将以口令保护该服务器。

这个服务器将运行在端口 2233 上。

我们将采取四个步骤，来创建这个服务器。


1. 编写一个配置文件；
2. 编写这个服务器的代码；
3. 启动该服务器；
4. 透过网络访问该服务器。


### 步骤 1：编写配置文件


下面时咱们示例的配置文件。


```erlang
{{#include ../../projects/appendix/src/config1}}
```

这个配置文件有数个如下形式的 `service` 元组：


```erlang
{service, <Name>, password, <P>, mfa, <Mod>, <Func>, <ArgList>}
```

这些参数由原子 `service`、`password` 和 `mfa` 分隔。`mfa` 是 *模组*、*函数*、*参数* 的缩写，表示接下来三个参数应被解释为模组名字、函数名字以及某个函数调用的参数列表。


在咱们的示例中，配置文件指定了一项将在端口 2233 上可用的名为 `math` 的服务。这项服务受密码 `qwerty` 保护。其在名为 `mod_math` 的模组中实现，并将以调用 `mod_math:run/3` 启动。`run/3` 的第三个参数将是 `[]`。


### 编写服务器的代码

这个数学服务器的代码看起来像下面这样：

```erlang
{{#include ../../projects/appendix/src/mod_math.erl}}
```

当某个客户端连接到端口 2233 并请求名为 `math` 的服务时，`lib_auth` 将认证该客户端。当口令正确时，就通过调用函数 `mod_math:run(MM, ArgC, ArgS)`，生成一个处理器进程。其中 `MM` 是 *中间人* 的 PID，`ArgC` 来自客户端，`ArgS` 来自配置文件。


当客户端发送一条消息 `X` 到服务器时，该消息将以一条 `{chan, MM, X}` 消息到达。当客户端死去或连接出错时，服务器将被发送一条 `{chan_closed, MM}` 消息。要发送一条消息 `Y` 到客户端，服务器执行了 `MM ! {send, Y}`，而要关闭信道，他执行了 `MM ! close` 操作。

这个数学服务器很简单；他只会等待一条 `{chan, MM, {factorial, N}}` 消息，随后通过执行 `MM ! {send, fac(N)}` 发送计算结果给客户端。


### 启动服务器


我们会如下启动该服务器：

```console
1> lib_chan:start_server("./config1").
lib_chan starting:"./config1"
ConfigData=[{port,2233},{service,math,password,"qwerty",mfa,mod_math,run,[]}]
true
```

> **译注**：译者采用了 Rebar3 构建工具的文件/目录结构。
>
> ```console
> $ tree -L 2 ~/erlang-book/projects/appendix
> /home/hector/erlang-book/projects/appendix
> ├── application_template.full
> ├── _build
> │   └── default
> ├── config1
> ├── elog4.config
> ├── gen_server_template.full
> ├── Makefile
> ├── rebar.lock
> ├── src
> │   ├── lib_chan_cs.erl
> │   ├── lib_chan.erl
> │   ├── lib_chan_mm.erl
> │   ├── math_server_app.erl
> │   ├── math_server.app.src
> │   ├── math_server.erl
> │   └── mod_math.erl
> └── supervisor_template.full
>
> 4 directories, 14 files
> ```
>
> Rebar3 需要 `src` 目录，以及 `some_name.app.src` 及其对应的 `some_name.erl` 两个文件，即可在目录下执行 `rebar3 compile` 命令，构建目标将位于 `_build/default/lib` 下。
>
> 然后即可执行命令 `erl -boot start_sasl -config elog4 -smp +S 12 -pa _build/default/lib/math_server/ebin` 启动 Erlang shell，并在其下执行上面的命令。








### `lib_chan_mm`：中间人
