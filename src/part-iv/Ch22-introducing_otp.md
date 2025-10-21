# 引入 OTP

OTP 是开放电信平台，Open Telecom Platform 的缩写。这个名字实际上有误导性，因为 OTP 远比咱们想象的要通用得多。他是个应用的操作系统，也是一个用于构建大规模、容错、分布式应用的库和过程集合。他由瑞典电信公司爱立信开发，在爱立信内部用于构建容错系统。标准 Erlang 发行版包含了 OTP 的库。


OTP 包含着数种强大工具 -- 如一个完整的 Web 服务器、FTP 服务器、[CORBA ORB](https://en.wikipedia.org/wiki/Common_Object_Request_Broker_Architecture) 等等 -- 这些都是以 Erlang 编写。OTP 还包含了一些用于构建电信应用程序的先进工具，包括 H248、SNMP 及 ASN.1 到 Erlang 的交叉编译器（这些都是电信行业中常用的协议）。这里我（作者）就不多说了，咱们可在 [Erlang 网站](https://www.erlang.org/) 上，找到关于这些主题的更多信息。


当咱们打算使用 OTP 编写咱们自己的应用时，那么咱们会发现一个非常有用的核心概念，便是 OTP 的 *行为*。行为封装了一些常见行为模式 -- 请把他当成一个由某个 *回调* 模组，参数化了的应用框架。

OTP 的强大之处在于，行为本身可以提供容错、可扩展性、动态代码升级等特性。换句话说，回调的编写者不必担心容错等问题，因为这些都是由行为提供的。对于有 Java 思维的人来说，可以将行为视为 J2EE 容器。

简单地说，行为解决了问题的非功能部分，而回调解决了功能部分。这样做的好处是，问题的非功能部分（例如，如何完成实时代码升级）对所有应用都是一样的，而功能部分（由回调提供）对每个问题都不同。

在这一章中，我们将详细介绍这些行为中的一种，即 `gen_server` 模组。不过，在深入探讨 `gen_server` 工作原理的细节前，我们将从一个简单的服务器（我们所能想象到的最简单服务器）开始，然后分几个小步骤对其修改，直到我们获得完整的 `gen_server` 模组。这样，咱们就能真正理解 `gen_server` 的工作原理，并真正准备好探究其中细节。

下面时这一章的规划：

1. 编写一个 Erlang 的小型客户端-服务器程序；
2. 慢慢概括这个程序，并添加一些功能特性；
3. 移步至真正代码。


## 通用服务器之路

*这是整本书中最重要的小节，所以要读一遍、两遍、一百遍 -- 确保其中的信条深入人心*。

这一小节是有关建立抽象的；我们将查看名为 `gen_server.erl` 的一个模组。`gen` 服务器是 OTP 系统中，最常用到的抽象之一，但很多人从未深入了解 `gen_server.erl` 的工作原理。一旦咱们掌握了 `gen` 服务器的构建方式，咱们将能重复该抽象过程，建立咱们自己的抽象。


我们将编写四个小的服务器，称为 `server1`、`server2`、`server3`、`server4`，每个都会与前一个略有不同。`server4` 将类似于 Erlang 发行版中的 `gen` 服务器。目标是把问题的非功能部分，与功能部分完全分开。最后那句话现在对咱们来说可能没什么意义，但别担心 -- 其很快就会有意义了。深吸一口气吧。


### 服务器 1：基本的服务器


以下是我们的首次尝试。他是个我们可以一个回调模组，参数化的小服务器。

```erlang
{{#include ../../projects/ch22-code/server1.erl}}
```

如此少量的代码，就捕捉到了服务器的精髓。我们来编写一个 `server1` 的 *回调*。下面是个名字服务器的回调：


```erlang
{{#include ../../projects/ch22-code/name_server.erl}}
```

这段代码实际上执行两项任务。他充当了服务器框架代码调用的回调模组，并同时包含了将被客户端调用的接口例程。通常的 OTP 惯例，是将这两种功能合并在同一个模组中。

要证明其会工作，请执行以下命令：


```erlang
1> server1:start(name_server, name_server).
true
2> name_server:add(joe, "at home").
ok
3> name_server:find(joe).
{ok,"at home"}
```

现在 *停下来思考一下*。其中回调并没有并发代码，没有生成进程，没有消息发送，没有消息接收，也没有进程注册。他是纯粹的顺序代码 -- *别无其他*。*这意味着我们可在不了解底层采用的并发模型下，编写客户端-服务器模型*。


这是所有服务器的 *基本* 模式。一旦咱们掌握了这种基本 *结构*， “自己动手” 就很容易。


### 服务器 2：带有事务的服务器


下面是个当其上的某个查询导致异常时，会崩溃掉客户端的服务器：

```erlang
{{#include ../../projects/ch22-code/server2.erl}}
```

这段代码提供了服务器上的 "事务语义" -- 当处理器函数中抛出异常时，他会以 `State` 的 *初始值* 循环。但在处理器函数成功执行时，则他会以处理器函数提供的 `NewState` 值循环。


当处理器函数失败时，发送导致该失败消息的客户端，会被发送一条引起他崩溃的消息。客户端无法继续运行，因为他发送到服务器的请求，导致了处理器函数函数崩溃。但任何别的想要使用该服务器的客户端，都将不受影响。此外，当处理器中某个错误发生时，服务器的状态不会改变。

请注意，这一服务器的回调模组，与我们用于 `server1` 的回调模组 *完全* 相同。*经由改变服务器，并保持回调模组不变，我们就可以改变回调模组的非功能行为*。

*注意*：最后那句话并不完全正确。当我们从 `server1` 来到 `server2` 时，我们必须对回调模组做个小改动，即将 `-import` 声明中的名称从 `server1` 改为 `server2`。否则，不会有任何变化。

> **译注**：译者尝试运行 `name_server:find(john)` 命令，崩溃掉客户端，但仅返回了 `error` 原子。~~此行为似乎与代码预期行为不符~~。后面发现，崩溃掉的方式为执行命令 `server2:rpc(name_server, "john")`。
>
> ```erlang
> > server2:rpc(name_server, "john").
> Server name_server request "john"
> caused exception function_clause
> ** exception exit: rpc
>      in function  server2:rpc/2 (server2.erl:10)
> ```

### 服务器 3：带热代码交换的服务器


现在我们将添加热代码交换能力。大多数服务器都会执行某个固定程序，当咱们打算修改服务器行为时，咱们必须要停止服务器，然后以修改了的代码重启他。当我们打算改变服务器的行为时，我们无需停止他；我们只要发送给他一条包含着新代码的消息，他就会汲取到新代码，并以新代码与原有会话数据继续运行。这个过程称为 *热代码交换*。


```erlang
{{#include ../../projects/ch22-code/server3.erl}}
```

当我们发送给服务器一条交换代码消息时，他会将回调模组，更改为包含在该消息中的新模组。

通过某个回调模组启动 `server3`，然后动态交换该回调模组，我们就可以演示这一特性。我们无法使用 `name_server` 作为回调模组，因为我们将服务器名称硬编译进了该模组中。因此，我们要复制一份这个模组，将其称为 `name_server1`，其中我们要更改了服务器的名字。


```erlang
{{#include ../../projects/ch22-code/name_server1.erl}}
```

首先，我们以 `name_server1` 这个回调模组，启动 `server3`。


```erlang
1> server3:start(name_server, name_server1).
true
2> name_server1:add(joe, "@home").
ok
3> name_server1:add(helen, "@work").
ok
```


现在，设想我们想要查找由这个名字服务器所提供的全部名字。API 中并无任何可实现这一目的的功能 -- `name_server` 模组只有 `add` 和 `find` 两个访问例程。

我们以迅雷不及掩耳之势，打开我们的文本编辑器，编写了个新的回调模组。

```erlang
{{#include ../../projects/ch22-code/new_name_server.erl}}
```

我们编译这段代码，并告诉服务器交换其回调模组。


```erlang
4> c(new_name_server).
{ok,new_name_server}
5> server3:swap_code(name_server, new_name_server).
ack
```

现在我们就可以运行服务器上的那些新功能了。


```erlang
6> new_name_server:all_names().
[joe,helen]
```

这里，我们 *不停机更改了回调模组* -- 这是动态的代码升级，就在咱们眼前，没有任何黑魔法。

现在请停下来再思考一下。我们完成的前两项任务，通常被认为是相当困难的，事实上，也是非常困难的。带有 “事务语义” 的服务器难于编写，具有动态代码升级能力的服务器也难于编写，但这种技术却让其变得简单。


这种技术非常强大。传统上，我们认为服务器是具有状态的一些程序，当我们发送给他们消息时，他们会改变状态。服务器上的代码在最初是固定的，而当我们打算更改服务器上的代码时，我们必须停止服务器并更改代码，然后我们才能重启该服务器。在我们给出的示例中，服务器上的代码，可像咱们更改服务器状态一样，简单地被更改。在一些 *绝不* 会因为软件维护升级而停止服务产品中，我们大量使用了这种技术。


### 服务器 4：事务与热代码交换

在前两个服务器中，代码升级和事务语义是分开的。我们来把二者 *合并* 到一个服务器中。扶住咱们的帽子。


```erlang
{{#include ../../projects/ch22-code/server4.erl}}
```

这个服务器同时提供了热代码交换和事务语义。真不错。


### 服务器 5：更多乐趣

既然我们已经掌握了动态的代码更改的这一概念，那么我们就可以有更多乐趣。下面是个在咱们让他变成某种特定类型服务器前，什么也不做的服务器：


```erlang
{{#include ../../projects/ch22-code/server5.erl}}
```

当我们启动他，并随后发送给他一条 `{become, F}` 消息时，他将成为一个计算 `F()` 的 `F` 服务器。我们将启动他。


```erlang
1> Pid = server5:start().
<0.87.0>
```

我们的服务器什么也不会做，而只是等待一条 `become` 消息。

现在我们来定义一项服务器功能。其并不复杂，只是计算阶乘的一些代码。


```erlang
{{#include ../../projects/ch22-code/my_fac_server.erl}}
```

只要确保其已编译，我们就可以让进程 `<0.87.0>` 成为一个阶乘服务器。


```erlang
2> c(my_fac_server).
{ok,my_fac_server}
3> Pid ! {become, fun my_fac_server:loop/0}.
{become,fun my_fac_server:loop/0}
```

既然我们的进程已成为一个阶乘服务器，我们便可调用他。


```erlang
4> server5:rpc(Pid, {fac, 10}).
3628800
```

我们的进程将保持作为一个阶乘服务器，直到我们发送给他一条 `{become, Something}` 信息，而让他做别的事情。

正如咱们从前面这些示例中可以看到，我们可以构造一系列不同类型，具有不同语义及一些相当惊叹属性的服务器。这种技术几乎太强大了。习惯了他的充分潜力，他就可制作出能力惊人、美轮美奂的小程序。当我们在构造一些涉及数十到数百名程序员的工业规模项目时，我们可能并不真正希望代码变化太快。我们必须在通用、强大，与对商业产品有用之间取得平衡。代码在运行过程中可以不断变化出新版本，固然很美，但当某个东西出错时，调试起来就很麻烦。当我们对咱们的代码，进行了数十项动态修改，然后代码崩溃了时，找出到底是哪里出了问题并非易事。


> **PlanetLab 上的 Erlang**
>
> 几年前，当我开始从而研究工作时，我正在 PlanetLab 下工作。我可以访问 PlanetLab 网络（一个全球研究网络 [planet-lab.org](https://planetlab.cs.princeton.edu/)），因此我在 PlanetLab 的所有机器（大约 450 台）上安装了 “空的” Erlang 服务器。我并不知道要在这些机器上做些什么，所以我只是建立了服务器基础架构，以便以后做些事情。
>
> 一旦我让这层运行起来，向这些空服务器发送信息，让他们成为一些真正的服务器就很容易了。
>
> 通常做法是启动（例如）一个 web 服务器，然后安装一些 web 服务器插件。我的做法是后退一步，而安装一个空服务器。后期插件会将该空服务器，变成一个 web 服务器。当我们用完该 web 服务器后，我们可能会让其变成别的东西。



As you can see from the previous examples, we can make a range of different types of servers, with different semantics and some quite surprising properties. This technique is almost too powerful. Used to its full potential, it can yield small programs of quite surprising power and beauty. When we make industrial-scale projects with dozens to hundreds of programmers involved, we might not actually want things to be too dynamic. We have to try to strike a balance between having something general and powerful and having something that is useful for commercial products. Having code that can morph into new versions while it runs is beautiful but terrible to debug if something goes wrong later. If we have made dozens of dynamic changes to our code and it then crashes, finding out exactly what went wrong is not easy.
