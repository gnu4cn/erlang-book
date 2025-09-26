# 分布式编程


编写 Erlang 的分布式程序，只是编写并发程序中的一小步。在分布式的 Erlang 中，我们可在远端节点和机器上，生成进程。在生成了远端进程后，我们将看到所有其他原语，即 `send` 、`receive`、`link` 等，都可以像在单个节点上一样，经由网络透明地运行。


在这一章中，我们将引入我们用以编写分布式 Erlang 程序的库和原语。*分布式程序* 属于被设计在计算机网络上运行，并只能经由消息传递，协调其活动的程序。


下面是我们要编写分布式应用的一些原因：


- *性能*

    通过将程序的不同部分，分派在不同机器上并行运行，我们可以让程序运行得更快。

- *可靠性*

    经由将系统架构于多台机器上，我们可构造出容错系统。当一台机器失效时，我们可在另一机器上继续。

- *可扩展性*

    随着我们扩大某个应用规模，迟早我们都将耗尽机器性能，即使是最强大的机器。在这个阶段，我们就必添加更多的机器，以增加能力。添加新机器应是无需对应用架构大改动的简单操作。

- *内在分布式应用*

    许多应用内在就是分布式的。当我们编写某个多用户游戏，或聊天系统时，不同用户将分散在全球各地。当我们有用户集中在某一特定地理位置时，我们就会打算将计算资源，放在这些用户附近。

- *乐趣*

    我（作者）打算编写的大多数有趣程序，都是分布式的。其中许多都涉及与世界各地的人和机器交互。


### 分布式的两种模型


在本书中，我们将讨论分布式的两种主要模型。


- *分布式 Erlang*

    在分布式 Erlang 下，程序被编写为运行于 Erlang *节点*。所谓节点，是以其自己的地址空间与进程集，包含了完整虚拟机的独立 Erlang 系统。

    我们可在任何节点上生成进程，同时我们前几章中讲到的所有消息传递和错误处理原语，都可在这种单节点情形下工作。

    分布式 Erlang 的应用，运行在 *受信任* 环境中 -- 因为任何节点，都可在任何其他 Erlang 节点上，执行任何操作，这就涉及到一种高度信任。通常情况下，分布式 Erlang 应用，将运行在同一局域网的集群上，并在防火墙之后，尽管他们也可运行在开放网络中。

- *基于套接字的分布式*

    运用 TCP/IP 套接字，我们可编写出能在 *不受信任* 环境中，运行的分布式应用。与分布式 Erlang 相比，这种编程模型不那么强大，但更为安全。在 [14.6 小节 “基于套接字的并发”](基于套接字的并发) 中，我们将了解如何运用一种简单的基于套接字的并发机制构造应用。



当咱们回想前面的章节时，咱们就会记得，我们构建程序的基本单元，便是进程。编写分布式 Erlang 程序非常简单；我们只须在正确的机器上，生成咱们的进程，然后一切都会像以前一样运作。


我们都习惯于编写顺序程序。编写分布式程序通常要困难得多。在本章中，我们将学习编写简单分布式程序的一些技巧。尽管这些程序很简单，但却非常有用。


我们将从一些小的示例开始。为此，我们将只需掌握两件事；然后我们就可以构造我们的第一个分布式程序。我们将学习怎样启动一个 Erlang 节点，以及怎样在某个远程 Erlang 节点上，执行一次远程过程调用。


> *知识点*：
>
> - an Erlang node
>
> - a remote procedure call, a RPC


## 编写一个分布式程序


当开发某个分布式应用时，我（作者）总是以特定顺序，于该程序上开展工作，该顺序如下：


1. 我（作者）会在常规的非分布式 Erlang 会话中，编写和测试我的程序。这便是到目前为止，我们一直都在做的，所以这还没有表现出什么新挑战；

2. 我（作者）会在 *同一台计算机* 上，运行的两个不同 Erlang 节点中，测试我的程序；

3. 我（作者）会在运行在同一局域网内，或互联网上任何地方的 *两台物理分离的计算机* 上的两个不同 Erlang 节点上，测试我的程序。


最后一步可能会有问题。当我们在同一管理域内的机器上运行时，这很少会是个问题。但当所涉及节点属于不同域的机器时，我们就会遇到连接问题，同时我们必须确保我们的系统防火墙及安全设置配置正确。


为说明这些步骤，我们将构造一个简单的名字服务器。具体来说，我们将完成下面这些事情：


- 阶段 1：在常规非分布式 Erlang 系统下，编写并测试这个名字服务器；

- 阶段 2：在同一机器的两个节点上，测试这个名字服务器；

- 阶段 3：在同一局域网内的两台不同机器上的两个不同节点上，测试这个名字服务器；

- 阶段 4：在两个不同国家，分属两个不同域的两台不同机器上，测试这个名字服务器。



## 构建名字服务器


所谓 *名字服务器*，是个在给其一个名字后，返回一个与该名字关联值的程序。我们还可以修改与某个特定名字关联的值。


我们的首个名字服务器相当简单。他不是容错的，因此当其崩溃时，他存储的所有数据都将丢失。这个练习的重点，不是构造一个容错的名字服务器，而是分布式编程技术入门。


### 阶段 1：简单名字服务器

我们的名字服务器 `kvs`，是个简单的 `Key → Value` 服务器。他有着以下接口：


- `-spec kvs:start() -> true`

    启动该服务器；这会以注册名字 `kvs`，创建出一个服务器。

- `-spec kvs:store(Key, Value) -> true`

    将 `Key` 与 `Value` 关联。

- `-spec kvs:lookup(Key) -> {ok, Value} | undefined`


    查找 `Key` 的值，当存在某个与 `Key` 关联的值时，则返回 `{ok, Value}`；否则返回 `undefined`。


这个键值服务器，是使用进程字典的 `get` 与 `put` 原语实现的，如下所示：


```erlang
{{#include ../../projects/ch14-code/socket_dist/kvs.erl}}
```


存储消息于第 6 行出发送，并在第 19 行接收。主服务器于第 17 行的 `loop` 函数处启动；他调用了 `receive` 并等待一个存储或查找消息，然后将请求数据保存在本地进程字典，或从本地进程字典中检索请求数据，并将一个回复发送回客户端。我们将以在本地测试该服务器开始，看看他是否正常工作。


```erlang
1> kvs:start().
true
2> kvs:store({location, joe}, "Stockholm").
true
3> kvs:store(weather, raining).
true
4> kvs:lookup({location, joe}).
{ok,"Stockholm"}
5> kvs:lookup({location,jane}).
undefined
6> kvs:lookup(weather).
{ok,raining}
```


到目前为止，我们都没有遇到令人不快的惊喜。接下来是第 2 步。我们来分发这个应用。


### 阶段 2：客户端在一个节点，服务器在第二个节点，不过仍是同一主机


现在，我们将在 *同一* 计算机上，启动两个 Erlang 节点。要完成这一目的，我们需要打开两个终端窗口，并启动两个 Erlang 系统。


首先，我们启动一个终端 shell，并在这个 shell 中启动一个名为 `gandalf` 的分布式 Erlang 节点；然后，我们启动这个服务器：

```erlang
$ erl -sname gandalf
...
(gandalf@ZBT7RX-L1)1> kvs:start().
true
```


其中参数 `-sname gandalf` 表示 “在本地主机上启动一个名字为 `gandalf` 的 Erlang 节点”。请注意 Erlang shell 在命令提示符前，打印 Erlang 节点名字的方式。节点名称的形式为 `Name@Host`。其中 `Name` 和 `Host` 均为原子，因此当他们包含任何非原子字符时，都必须加上引号。


*重要提示*：当咱们在自己系统上运行上面的命令时，节点名字可能不是 `gandolf@localhost`。他可能是 `gandolf@H`，其中 `H` 是咱们的本地主机名。这将取决于咱们的系统配置方式。当属于这种情形时，那么在后面的示例中，咱们都必须使用 `H` 而不是 `localhost`。


接下来我们就要启动 *第二个* 终端会话，并启动一个名为 `bilbo` 的 Erlang 节点。然后我们就可以使用库模组 `rpc`，调用 `kvs` 中的那些函数。（请注意，`rpc` 是个标准的 Erlang 库模组，其与我们先前编写的 `rpc` 函数不一样。）


```erlang
$ erl -sname bilbo
...
(bilbo@test)1> rpc:call(gandalf@test, kvs, store, [weather, fine]).
true
(bilbo@test)2> rpc:call(gandalf@test, kvs, lookup, [weather]).
{ok,fine}
```

现在可能看起来还不像回事，但我们实际上已经进行了咱们从未有过的第一次分布式计算！服务器运行在我们启动的第一个节点上，同时客户端运行在第二个节点上。


那个设置 `weather` 值的调用，是在 `bilbo` 节点上构造出的；我们可以换回 `gandalf` 节点，并检查天气的值。


```erlang
(gandalf@test)6> kvs:lookup(weather).
{ok,fine}
```


`rpc:call(Node,Mod,Func,[Arg1,Arg2,...,ArgN])` 会在 `Node` 上，执行一次 *远程过程调用*。被调用的函数为 `Mod:Func(Arg1,Arg2,...,ArgN)`。


正如我们所看到的，这个程序会如同非分布式 Erlang 情形下一样工作；现在唯一区别，是客户端运行在一个节点上，同时服务器运行在另一节点上。


下一步是要在不同的机器上，运行客户端与服务器。


### 阶段 3：同一局域网内不同机器上的客户端和服务器


我们将用到两个节点。第一个节点名为 `gandalf`，位于 `doris.myerl.example.com` 上，而第二个节点名为 `bilbo`，位于 `george.myerl.example.com` 上。在我们这样做前，我们要先在两台不同的机器上，使用 `ssh` 或 VNC 等工具启动两个终端。我们将这两个窗口，分别称为 `doris` 和 `george`。在完成后，我们就可以轻松地这两台机器上输入命令。


步骤 1 是在 `doris` 上启动一个 Erlang 节点。




```console
[hector@doris socket_dist]$ erl -name gandalf@doris.xfoss.net -setcookie abc
Erlang/OTP 26 [erts-14.2.5] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [jit:ns]

Eshell V14.2.5 (press Ctrl+G to abort, type help(). for help)
(gandalf@doris.xfoss.net)1> kvs:start().
true
(gandalf@doris.xfoss.net)2>
```

> *译注*：在 `Erlang/OTP 26` 下，需要以上面的命令启动 LAN 节点。若以命令 `erl -name gandalf -setcookie abc` 启动，会报出如下错误。在 `Erlang/OTP 25` 下则没有问题，如后面所示。
>
> ```console
> [hector@doris socket_dist]$ erl -name gandalf -setcookie abc                                                                17:22:06 [45/45]
> 2025-09-26 17:22:06.251459
>     args: []
>     label: {error_logger,info_msg}
>     format: "Can't set long node name!\nPlease check your configuration\n"
> 2025-09-26 17:22:06.251497 crash_report
>     initial_call: {net_kernel,init,['Argument__1']}
>     pid: <0.65.0>
>     registered_name: []
>     error_info: {exit,{error,badarg},[{gen_server,init_it,6,[{file,"gen_server.erl"},{line,961}]},{proc_lib,init_p_do_apply,3,[{file,"proc_l
> ib.erl"},{line,241}]}]}
>     ancestors: [net_sup,kernel_sup,<0.47.0>]
>     message_queue_len: 0
>     messages: []
> ```
>

第二步是在 `george` 上启动一个 Erlang 节点，并发送一些命令到 `gandalf`。


```console
$ erl -name bilbo -setcookie abc

Erlang/OTP 25 [erts-13.1.5] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [jit:ns]

Eshell V13.1.5  (abort with ^G)
(bilbo@george.xfoss.net)1> rpc:call('gandalf@doris.xfoss.net', kvs, store, [weather, cold]).
true
(bilbo@george.xfoss.net)2> rpc:call('gandalf@doris.xfoss.net', kvs, lookup, [weather]).
{ok,cold}
(bilbo@george.xfoss.net)3>
```

> **译注**：这里需要将 Erlang 节点名字用单引号括起来，否则将报出以下错误。
>
> ```console
> $ erl -name jack@george.xfoss.net -setcookie abc
> Erlang/OTP 25 [erts-13.1.5] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [jit:ns]
>
> Eshell V13.1.5  (abort with ^G)
> (jack@george.xfoss.net)1> rpc:call(gandalf@doris.xfoss.com, kvs, lookup, [weather]).
> * 1:23: syntax error before: '.'
> ```

> **译注**：这里使用虚拟机 `doris.xfoss.net`、`george.xfoss.net` 及 `win10.xfoss.net`。其中 `george.xfoss.net` 是 Debian12 系统，`doris.xfoss.net` 是 AlmaLinux9 系统，`win10.xfoss.net` 是 M$ Win10 系统。宿主机的 `/etc/hosts` 文件如下。
>
> ```config
> 192.168.122.133	win10.xfoss.net win10
> 192.168.122.199	debian george.xfoss.net
> 192.168.122.158	almalinux doris.xfoss.net
> ```
>
> 保证在各个主机中，都能经由 `libvirt` 管理的 `dnsmasq` 查询到其他主机的 IP 地址（及反向查询）。
>
> 其中 Debian12 和 AlmaLinux9 主机需要关闭防火墙（防火墙打开时，会出现 `{badrpc,nodedown}` 报错）。Deian12 关闭防火墙命令：`sudo ufw disable`。AlmaLinux9 关闭防火墙命令：`sudo systemctl stop firewalld`；AlmaLinux9 还需关闭 SELinux。


情况与同一台机器上的两个不同节点完全相同。

与在同一台计算机上运行两个节点的情形相比，为这种部署运作，事情要稍微复杂一些。我们必须采取四个步骤。

1. 要以 `-name` 命令参数启动 Erlang。当我们有着位于同一台机器上的两个节点时，我们使用了 “短的” 名称（正如 `-sname` 命令开关所表示的），但当两个位于不同网络上时，我们就要用 `-name`。

    当两台不同机器位于同一子网时，我们也可以对他们使用 `-sname`。当没有可用的 DNS 服务时，使用 `-sname` 也是唯一可行方法。

2. 要确保两个节点有着相同 *cookie*。这就是为何两个节点，都是以命令行参数 `-setcookie abc` 启动的原因。我们将在本章稍后的 [14.5 节 *Cookie 保护系统*](#cookie-保护系统) 中，详细介绍 cookies。*请注意*：当我们在 *同一* 机器上运行两个节点时，两个节点都可以访问同一个 cookie 文件 `$HOME/.erlang.cookie`，这就是为什么我们不必在 Erlang 命令行上，加上 cookie 的原因；


3. 要确保相关节点的完全合格主机名，为可经由 DNS 解析的。在我（作者）的示例中，域名 `myerl.example.com` 完全属于我家庭网络本地，且是由在 `/etc/hosts` 中添加条目，在本地解析；


4. 要确保两个系统有着同一个代码版本，及相同 Erlang 版本。当咱们没有这样做时，咱们可能会得到严重及神秘的错误。避免出现问题的最简单方法，就是要在各处有着相同版本的 Erlang。不同版本的 Erlang 可以一起运行，但无法保证这会生效，所以最好先检查一下。在我们的示例中，同一版本的 `kvs` 代码必须要在两个系统上可用。做到这一点有好几种方式。

    - 在我（作者）家的设置下，我有两台物理上分开，没有共享文件系统的计算机；在这里，我将 `kvs.erl` 物理拷贝到两台机器，并在启动程序前编译了 `kvs.erl`；

    - 在我的工作电脑上，我们使用的是有共享 NFS 磁盘的工作站。在这里，我只是在两台不同工作站上的共享目录下，启动 Erlang；

    - 配置代码服务器完成这个示例。我（作者）不会在这里介绍如何实现这个目的。请查看 [`erl_prim_loader` 模组的手册页](https://www.erlang.org/doc/apps/erts/erl_prim_loader.html)；

    - 使用 shell 命令 `nl(Mod)`。这会在所有连接的节点上，加载模组 `Mod`。

    *注意*：要让这种方式工作，咱们必须确保所有节点都是连接的。当节点首次尝试访问对方时，他们就成为了已连接状态。这会在咱们首次执行任何涉及远程节点的表达式时发生。而这样做的最简单方法，就是执行 `net_adm:ping(Node)`（更多详情请参见 [`net_adm` 手册页面](https://www.erlang.org/doc/apps/kernel/net_adm.html)）。

>    **译注**：下面是在 Win10 上执行 `net_adm:ping` 命令的示例输出：
>
>    ```console
>    PS C:\Users\Hector PENG> erl -name john@win10.xfoss.net -setcookie abc
>    Erlang/OTP 28 [erts-16.1] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [jit:ns]
>
>    Eshell V16.1 (press Ctrl+G to abort, type help(). for help)
>    (john@win10.xfoss.net)1> net_adm:ping('bilbo@george.xfoss.net').
>    pong
>    (john@win10.xfoss.net)2>
>    ```

成功！我们在同一局域网的两台服务器上运行了。下一步是将这些，迁移到经由互联网连接的两台计算机上。

### 阶段 4：互联网中不同主机上的客户端与服务器


原则上，这与阶段 3 中的情况相同，但现在我们必须更为关注安全。当我们在同一局域网上运行两个节点时，我们可能不必太担心安全。在大多数组织中，局域网都经由防火墙与互联网隔离。在防火墙之后，我们可以随意以某种草率方式分配 IP 地址，而常常会错误配置机器。


当我们在互联网上，连接某个 Erlang 集群中的数台机器时，我们可能会遇到防火墙不放行传入连接的一些问题。我们将必须将我们的防火墙，配置为接受传入连接。由于每个防火墙都不同，因此没有某种通用的方式完成这点。


要准备将咱们的系统用于分布式 Erlang，咱们将必须采取以下步骤：

1. 确保端口 `4369` 同时对 TCP 和 UDP 流量开放。这个端口会被名为 `epmd`（Erlang Port Mapper Daemon 的缩写）的程序用到；

2. 要选取将用于分布式 Erlang 的某个端口，或某个端口范围，并确保这些端口是放开的。当这些端口是 `Min` 和 `Max`（当咱们只想打算一个端口时，则使用 `Min = Max`）时，则要以下面的命令，启动 Erlang：

    ```console
    $erl -name ... -setcookie ... -kernel inet_dist_listen_min Min \
                                          inet_dist_listen_max Max
    ```

现在我们已经了解了，如何在 Erlang 节点集上运行程序，以及如何在同一局域网，或经由互联网运行程序。接下来，我们将了解涉及节点的一些原语。
