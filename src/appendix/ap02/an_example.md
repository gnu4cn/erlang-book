# 一个示例


我们将从一个展示如何使用 `lib_chan` 的简单示例开始。我们将创建一个能够计算阶乘和斐波那契数的简单服务器。我们将以口令保护该服务器。

这个服务器将运行在端口 2233 上。

我们将采取四个步骤，来创建这个服务器。


1. 编写一个配置文件；
2. 编写这个服务器的代码；
3. 启动该服务器；
4. 透过网络访问该服务器。


## 步骤 1：编写配置文件


下面时咱们示例的配置文件。


```erlang
{{#include ../../../projects/appendix/config1}}
```

这个配置文件有数个如下形式的 `service` 元组：


```erlang
{service, <Name>, password, <P>, mfa, <Mod>, <Func>, <ArgList>}
```

这些参数由原子 `service`、`password` 和 `mfa` 分隔。`mfa` 是 *模组*、*函数*、*参数* 的缩写，表示接下来三个参数应被解释为模组名字、函数名字以及某个函数调用的参数列表。


在咱们的示例中，配置文件指定了一项将在端口 2233 上可用的名为 `math` 的服务。这项服务受密码 `qwerty` 保护。其在名为 `mod_math` 的模组中实现，并将以调用 `mod_math:run/3` 启动。`run/3` 的第三个参数将是 `[]`。


## 编写服务器的代码

这个数学服务器的代码看起来像下面这样：

```erlang
{{#include ../../../projects/appendix/src/mod_math.erl}}
```

当某个客户端连接到端口 2233 并请求名为 `math` 的服务时，`lib_auth` 将认证该客户端。当口令正确时，就通过调用函数 `mod_math:run(MM, ArgC, ArgS)`，生成一个处理器进程。其中 `MM` 是 *中间人* 的 PID，`ArgC` 来自客户端，`ArgS` 来自配置文件。


当客户端发送一条消息 `X` 到服务器时，该消息将以一条 `{chan, MM, X}` 消息到达。当客户端死去或连接出错时，服务器将被发送一条 `{chan_closed, MM}` 消息。要发送一条消息 `Y` 到客户端，服务器执行了 `MM ! {send, Y}`，而要关闭信道，他执行了 `MM ! close` 操作。

这个数学服务器很简单；他只会等待一条 `{chan, MM, {factorial, N}}` 消息，随后通过执行 `MM ! {send, fac(N)}` 发送计算结果给客户端。


## 启动服务器


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


## 透过网络访问服务器


我们可以一台机器上，测试此代码。


```erlang
2> {ok, S} = lib_chan:connect("localhost",2233,math,"qwerty",{yes,go}).
{ok,<0.100.0>}
mod_math: run starting
ArgC = {yes,go} ArgS = []
3> lib_chan:rpc(S, {factorial,20}).
2432902008176640000
4> lib_chan:rpc(S, {fibonacci,15}).
610
5> lib_chan:disconnect(S).
close
mod_math stopping
```


