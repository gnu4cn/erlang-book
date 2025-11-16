# `lib_chan` 工作原理


`lib_chan` 是使用四个模组中的代码构建的。

- `lib_chan` 充当 “主模组”。程序员需要了解的例程，仅限 `lib_chan` 中导出那些。其他三个模组（将在下文讨论）在 `lib_chan` 实现内部用到；
- `lib_chan_mm` 会编码及解码 Erlang 消息，并管理套接字通信；
- `lib_chan_cs` 会建立该服务器，并管理客户端连接。他的主要任务之一，是限制客户端同时连接的最大数量；
- `lib_chan_auth` 包含着简单挑战/响应式验证的代码。


## `lib_chan`


`lib_chan` 有着如下结构：


```erlang
-module(lib_chan).


start_server(ConfigFile) ->
    %% read configuration file - check syntax
    %% call start_port_server(Port, ConfigData)
    %% where Port is the required Port and ConfigData
    %% contains the configuration data

start_port_server(Port, ConfigData) ->
    lib_chan_cs:start_raw_server(Port,
				fun(Socket) ->
					start_port_instance(Socket, ConfigData) end,
    %% lib_chan_cs manages the connection
    %% when a new connection comes the fun which is an
    %% argument to start_raw_server will be called


start_port_instance(Socket, ConfigData) ->
    %% this is spawned when the client connects
    %% to the server. Here we setup a middle man,
    %% then perform authentication. If everything works call
    %% really_start(MM, ArgC, {Mod, Func, ArgS})
    %% (the last three arguments come from the configuration file


really_start(MM, ArgC, {Mod, Func, ArgS}) ->
    apply(Mod, Func, [MM, ArgC, ArgS]).


connect(Host, Port, Service, Secret, ArgC) ->
    %% client side code
```

## `lib_chan_mm`：中间人

`lib_chan_mm` 实现了一个中间人。他对应用隐藏了套接字通信，将 TCP 套接字上的数据流转换成 Erlang 消息。中间人负责组装消息（其可能已被分片），以及将 Erlang 项编码和解码为可在套接字上发送和接收的字节流。


![中间人下的套接字通信](../images/lib_chan_mm.png)

<a name="fig-8"></a>
**图 8** -- **中间人下的套接字通信**


现在快速浏览一下 [图 8，*中间人下的套接字通信](#fig-8) 的好时机，其展示了我们的中间人架构。当机器 `M1` 上的某个进程 `P1` 想要发送一条消息 `T` 到机器 `M2` 上的进程 `P2`时，他会执行 `MM1 ！{send, T}`。`MM1` 充当 `P2` 的 *代理*。发送到 `MM1` 的任何消息，都会被编码并写入套接字，并被发送到 `MM2`。`MM2` 会解码他在套接字上接收到的任何东西，并将消息 `{chan, MM2, T}` 发送给 `P2`。


在机器 `M1` 上，进程 `MM1` 充当了 `P2` 的代理，而在 `M2` 上，进程 `MM2` 则会充当 `P1` 的代理。

`MM1` 和 `MM2` 是两个中间人进程的 PID。中间人进程的代码，看起来像下面这样：

```erlang
loop(Socket, Pid) ->
    receive
        {tcp, Socket, Bin} ->
            Pid ! {chan, self(), binary_to_term(Bin)},
            loop(Socket, Pid);
        {tcp_closed, Socket} ->
            Pid ! {chan_closed, self()};
        close ->
            gen_tcp:close(Socket);
        {send, T} ->
            gen_tcp:send(Socket, [term_to_binary(T)]),
            loop(Socket, Pid)
    end.
```

这个循环被用作套接字数据世界，与 Erlang 消息传递世界的接口。咱们可在 [`lib_chan_mm`](#lib_chan_mm) 中，找到 `lib_chan_mm` 的完整代码。相比这里给出的代码，完整代码稍微复杂一些，但原理是一样的。唯一的区别在与，我们添加了一些用于跟踪消息的代码，以及一些接口例程。


## `lib_chan_cs`


`lib_chan_cs` 负责建立客户端与服务器的通信。他导出的两个重要例程如下：


- `start_raw_server(Port, Max, Fun, PacketLength)`

    这个例程会在 `Port` 上启动一个监听连接的监听器。最多 `Max` 个同时会话被允许。`Fun` 是一个 1 元函数；当某个连接启动时，`Fun(Socket)` 会被执行。套接字通信会假设一个长度为 `PacketLength` 的数据包。


- `start_raw_client(Host, Port, PacketLength) -> {ok, Socket} | {error, Why}`

    这个例程会尝试连接到某个以 `start_raw_server` 打开的端口。


`lib_chan_cs` 的代码，遵循了 [并行服务器](../part-iv/Ch17-programming_with_sockets.md#parall_server) 中描述的模式，但此外他还会追踪同时打开连接的最大数量。这个小细节虽然在概念上很简单，却增加了二十多行看起来相当奇怪，捕获退出等的代码。像这样的代码一团糟，不过不用担心；他完成了他的工作，并对该模组的用户隐藏了复杂性。

## `lib_chan_auth`


该模组实现了一种简单形式的挑战/响应认证。挑战/响应的认证，基于与服务名名字相关联的共享秘密概念。要演示其工作原理，我们将假定有项名为 `math`，有着共享秘密 `qwerty` 的服务。

当某个客户端打算使用服务 `math` 时，那么该客户端就必须向服务器证明，他们知道共享秘密。这个过程如下进行：

1. 客户端发送一次请求到服务器，表明他想要使用 `math` 服务；

2. 服务器会计算出一个随机字符串 `C` 并将其发送给客户端。这就是 *挑战*。该字符串由 `lib_chan_ auth:make_challenge()` 函数计算得出。我们可以交互式地使用他，看看他做了些什么。

    ```erlang
    1> lib_chan_auth:make_challenge().
    "ynsbrxyuavrknszkretvjvqao"
    ```

3. 客户端收到这个字符串 (`C`) ，并计算出响应 (`R`)，其中 `R = MD5(C ++ Secret)`。`R` 是使用 `lib_chan_auth:make_response` 计算出的。下面是个示例：

    ```erlang
    2> R = lib_chan_auth:make_response(C, "qwerty").
    "58565dbdf3328f7d9b9869a2c47eb699"
    ```

4. 响应会被发回服务器。服务器收到该响应，并通过计算该响应的预期值，检查其是否正确。这是在 `lib_chan_auth:is_response_correct` 中完成的。

    ```erlang
    3> lib_chan_auth:is_response_correct(C, R, "qwerty").
    true
    ```


