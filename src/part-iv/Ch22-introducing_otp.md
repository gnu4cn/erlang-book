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
> 一旦我让这层运行了起来，那么向这些空服务器发送信息，让他们成为一些真正的服务器就很容易。
>
> 通常做法是启动（例如）一个 web 服务器，然后安装一些 web 服务器插件。我的做法是后退一步，而安装一个空服务器。后期插件会将该空服务器，变成一个 web 服务器。当我们用完该 web 服务器后，我们可能会让其变成别的东西。


这一小节中的服务器示例，实际上并不完全正确。他们这样编写，是为强调所涉及的思想，不过他们确实有一两处极小和细微的错误。我（作者）不会马上告诉咱们这些错误是什么，但我将在本章末尾给咱们一些提示。

Erlang 的 `gen_server` 模组，便是一系列逐渐复杂服务器（就像我们到目前为止在本章中编写的那些一样）的逻辑归集。

自 1998 年以来，`gen_server` 便一直用于工业产品。数百的服务器可作为某单一产品的一部分。这些服务器由程序员使用常规的顺序代码编写。全部错误处理与全部非功能性行为，均在服务器的通用部分分解。

现在，我们将发挥想象力，看看真正的 `gen_server`。


## `gen_server` 入门


我打算把咱们丢入深渊。下面是编写一个 `gen_server` 回调模组的简单三点计划：

1. 确定出回调模组名称；
2. 编写接口函数；
3. 编写回调模组中必需的 6 个回调函数。


这其实很简单。无需思考 -- 只需按计划行事！


### 第 1 步：确定回调模组的名字

我们计划构造一个简单的支付系统。我们将把模组命名为 `my_bank`。


### 第 2 步：编写接口例程


我们将定义 5 个接口例程，全都在模组 `my_bank` 中。


- `start()`

    开设银行；

- `stop()`

    关闭银行；

- `new_account(Who)`

    创建新账户；

- `deposit(Who, Amount)`

    存钱进银行；


- `withdraw(Who, Amount)`

    把钱取出来，当账户上有钱时。


每个这些例程，都会都会引起对 `gen_server` 中例程的一个调用，如下所示：


```erlang
{{#include ../../projects/ch22-code/my_bank.erl:10:15}}
```

其中 `gen_server:start_link({local, Name}, Mod, ...)` 会启动一个 *本地* 服务器。当第一个参数是原子 `global` 时，他将启动一个可在 Erlang 节点集群上访问的全局服务器。`start_link` 的第二个参数为 `Mod`，即回调模组的名字。宏 `?MODULE` 会展开为该模组的名字 `my_bank`。我们将暂时忽略 `gen_server:start_link` 的其他参数。

`gen_server:call(?MODULE, Term)` 用于对服务器的某个远程过程调用。


### 第 3 步：编写回调例程


我们的回调模组必须导出六个回调例程：

- `init/1`
- `handle_call/3`
- `handle_cast/2`
- `handle_info/2`
- `terminate/2`
- `code_change/3`


为方便起见，我们可使用数个模板构造 `gen_server`。下面是最简单的一种：


```erlang
{{#include ../../projects/ch22-code/gen_server_template.mini}}
```

这个模板包含了一个我们可以填入其中，构造咱们服务器的简单骨架。其中关键字 `-behaviour` 会被编译器使用，以便当我们忘记定义对应的回调函数时，生成告警或错误消息。`?SERVER` 宏需要定义为与 `start_link()` 函数中服务器的相同名字，因为默认其未被定义。

*提示*：当咱们正使用 Emacs 时，那么只需敲几下键盘就能拉取到一个 `gen_server` 模板。当咱们在 Erlang 模式下编辑时，那么 `Erlang > Skeletons` 菜单会提供一个创建 `gen_server` 模板的选项卡。当咱们没有 Emacs 时，也不必惊慌。我（作者）已在本章末尾附上了这个模板。

我们将以这个模板开始，对其稍加编辑。我们必须要做的，只是让那些接口例程中的参数，与这个模板中的参数达成一致。


其中最重要的是 `handle_call/3` 这个函数。我们必须编写出与那些接口例程中定义的三个查询项匹配的代码。也就是说，我们必须填入下面的这些点：


```erlang
handle_call({new, Who}, From, State) ->
    Reply  = ...
    State1 = ...
    {reply, Reply, State1};
handle_call({add, Who, Amount}, From, State) ->
    Reply  = ...
    State1 = ...
    {reply, Reply, State1};
handle_call({remove, Who, Amount}, From, State) ->
    Reply  = ...
    State1 = ...
    {reply, Reply, State1};
```

这段代码中 `Reply` 的值，将作为远程过程调用的返回值发送回客户端。

而 `State` 只是个表示服务器全局状态，会在服务器中传递的变量。在我们的银行模组中，状态永不会改变；他只是个恒定的 ETS 数据表索引（尽管该数据表的内容会改变）。


当我们填入模板并稍作编辑后，我们会得到以下代码：


```erlang
{{#include ../../projects/ch22-code/my_bank.erl:18:54}}
```


通过调用 `gen_server:start_link(Name,CallBackMod,StartArgs,Opts)`，我们启动了服务器；然后回调模组中第一个被调用的例程为 `Mod:init(StartArgs)`，其必须返回 `{ok, State}`。`State` 的值会作为 `handle_call` 中的第三个参数，重新出现。

请注意我们停止服务器的方式。停止服务器的 `handle_call(stop, From, Tab)` 函数，会返回 `{stop, normal, stopped, Tab}`。其中第二个参数（`normal`），会被用作 `my_bank:terminate/2` 的第一个参数。第三个参数（`stoped`）会成为 `my_bank:stop()` 的返回值。


就这样，我们完成了。那么我们去一趟这家银行吧。

```erlang
1> my_bank:start().
{ok,<0.87.0>}
2> my_bank:deposit("joe", 10).
not_a_customer
3> my_bank:new_account("joe").
{welcome,"joe"}
4> my_bank:deposit("joe",10).
{thanks,"joe",your_balance_is,10}
5> my_bank:deposit("joe",30).
{thanks,"joe",your_balance_is,40}
6> my_bank:withdraw("joe",15).
{thanks,"joe",your_balance_is,25}
7> my_bank:withdraw("joe",45).
{sorry,"joe",you_only_have,25,in_the_bank}
```

## `gen_server` 回调的结构

既然我们已经掌握了这个概念，我们将更加详细地了解一下，`gen_server` 的回调结构。


### 启动服务器

`gen_server:start_link(Name,Mod,InitArgs,Opts)` 这个调用，会启动一切。他会创建一个名为 `Name` 的通用服务器。回调模组为 `Mod`。`Opts` 会控制这个通用服务器的行为。我们可在这里指定消息日志、调试函数等。通用服务器会以 `Mod:init(InitArgs)` 启动。

`init` 的模板条目，在 [图 2，*`init` 模板条目*](#fig-2) 中给出（完整模板可在 [A1.1 小节，*通用服务器模板*](../appendix/ap01-otp_templates.md#通用服务器模板) 中找到）：

在正常操作下，我们只返回 `{ok, State}`。有关其他参数的含义，请查阅 [`gen_server` 的手册页面](https://www.erlang.org/docs/24/man/gen_server)。

```erlang
{{#include ../../projects/ch22-code/fig_2.erl:1:13}}
```

<a name="fig-2"></a>
**图 2** -- **`init` 的模板条目**


当 `{ok, State}` 返回时，那么我们就成功启动了服务器，同时初始状态为 `State`。

### 调用服务器

要调用服务器，客户端程序就要调用 `gen_server:call(Name,Request)`。这导致回调模组中的 `handle_call/3` 被调用。

`handle_call/3` 有着如下的模板条目：


```erlang
{{#include ../../projects/ch22-code/fig_2.erl:16:32}}
```

`Request`（`gen_server:call/2` 的第二个参数），会作为 `handle_call/3` 的第一个参数重新出现。`From` 是请求客户端进程的 PID，`State` 是客户端的当前状态（译注：不应该是服务器的当前状态吗？）。


通常我们会返回 `{reply, Reply, NewState}`。当这种情况发生时，`Reply` 会返回客户端，成为 `gen_server:call` 的返回值。`NewState` 是服务器的下一状态。

至于别的返回值，即 `{noreply, ..}` 与 `{stop, ..}`，他们用到的频率相对较低。`noreply` 会造成服务器继续运行，但客户端将等待某个回复，因此服务器将必须把回复这个任务，委托给别的进程。调用带有适当参数的 `stop` 将停止服务器。

### 调用与播发

**Calls and Casts**

我们已经看到 `gen_server:call` 和 `handle_call` 之间的相互作用。这用于实现 *远程过程调用*。而 `gen_server:cast(Name,Msg)` 实现的则是没有返回值的调用（实际上只是条消息，但传统上其被称为播发，以区别于远程过程调用）。


相应的回调例程是 `handle_cast`；模板条目如下：

```erlang
{{#include ../../projects/ch22-code/fig_2.erl:35:46}}
```

这个处理器通常只返回改变服务器状态的 `{noreply, NewState}`，或停止服务器的 `{stop, ...}`。



### 到服务器的自发消息

**Spontaneous Messages to the Server**

回调函数 `handle_info(Info, State)` 用于处理到服务器的一些自发消息。所谓自发消息，是指到并未经由显式调用 `gen_server:call` 或 `gen_server:cast`，而到达服务器的任何消息。例如，当服务器被链接到另一进程，且正在捕获退出（信号），那么他可能会突然收到一条未预期的 `{'EXIT', Pid, What}` 消息。或者，系统中任何发现了该通用服务器 `PID` 的进程，都可以直接发送给他一条消息。像这样的任何信息，最终都会作为 `info` 的值到达服务器处。


`handle_info` 的模板条目如下：

```erlang
{{#include ../../projects/ch22-code/fig_2.erl:48:59}}
```


返回值与 `handle_cast` 的相同。


## 再见，宝贝

**Hasta la Vista, Baby**

服务器可能因多种原因终止。某个 `handle_Something` 例程可能返回 `{stop, Reason, NewState}`，或者服务器可能以 `{'EXIT', reason}` 崩溃。在所有这些情况下，无论他们如何发生，`terminate(Reason, NewState)` 都将被调用。下面是其模板：

```erlang
{{#include ../../projects/ch22-code/fig_2.erl:61:73}}
```

这段代码无法返回新状态，因为我们已经终止了。但是，在我们终止时，清楚服务器的状态是非常有用的；我们可以将该状态存储在磁盘上、在某条消息中将其发送给其他进程，或者根据应用丢弃。当咱们想要咱们的服务器在未来要重启时，咱们就将必须编写一个由 `terminate/2` 触发的 “我会回来的” 函数。


## 代码变更

咱们可在服务器运行时，动态更改其状态。当系统执行软件升级时，这个回调函数会被发布处理子系统调用。

> *知识点*：
>
> - the release handling subsystem

这个话题，在 [OTP 系统文档中的发布处理小节](https://www.erlang.org/doc/system/release_handling.html)，有详细说明。


```erlang
{{#include ../../projects/ch22-code/fig_2.erl:61:73}}
```


## 填充 `gen_server` 模板

构造某个 OTP `gen_server`，主要是以咱们自己的一些代码，弹入样板模板。下面是个示例；`gen_server` 的各个小节，均已在上一小节中列出。`gen_server` 的模板本身已内置于 Emacs 中，但若咱们未使用 Emacs，则可在 [A1.1 小节 *通用服务器模板*](../appendix/ap01-otp_templates.md#通用服务器模板) 中找到整个模板。


我（作者）已填写这个模板，来构造一个名为 `my_bank` 的银行模组。下面这段代码即派生自该模板。我（作者）已移除该模板中的全部注释，这样咱们就可以清楚地看到代码结构。


```erlang
{{#include ../../projects/ch22-code/my_bank.erl}}
```

## 深入挖掘


`gen_server` 实际上相当简单。我们还没讲完 `gen_server` 中的 *所有* 接口函数，也没有讨论所有接口函数的所有参数。一旦咱们掌握了这些基本概念，咱们就可以在 [`gen_server` 的手册页面](https://www.erlang.org/docs/24/man/gen_server) 上查找详细信息。

在这一章中，我们只介绍了使用 `gen_server` 的最简单方式，但这应足以满足大多数目的。更复杂的一些应用，通常会让 `gen_server` 回复以 `noreply` 的返回值，而将真正回复委派给另一进程。有关这种做法的信息，请阅读 [“设计原则”](https://erlang.org/documentation/doc-4.9.1/doc/design_principles/des_princ.html) 文档，以及 [`sys`](https://www.erlang.org/docs/18/man/sys) 和 [`proc_lib`](https://www.erlang.org/doc/apps/stdlib/proc_lib.html) 两个模组的手册页面。


这一章介绍了将服务器行为，抽象为两个组件的概念：一个是可用于所有服务器的 *通用* 组件，以及另一可用于对该通用组件定制的 *特定* 组件（或称处理器）。这一概念的主要好处，是代码整齐地分为了两部分。通用组件负责了并发及错误处理的许多方面，而处理器有的只是些顺序代码。


在此之后，我们介绍了 OTP 系统中的第一个大的行为，即 `gen_server`，并展示了他如何由一个相当简单易懂的服务器，在几个小的转换步骤后，建立了起来。


`gen_server` 可用于许多目的，但他并非万金油。`gen_server` 的这种客户机-服务器交互模式，有时会让人感觉别扭，而并不会自然地适应咱们的问题。当这种种情况发生时，咱们应重新考虑构造 `gen_server` 所需的那些转换步骤，并根据咱们问题的具体需求，调整这些步骤。

当我们从单个的服务器转向系统时，我们将需要多个服务器；我们会打算监视这些服务器、以一致方式重启失效服务器并记录出错。这是下一章的主题。

## 练习

在下面这些练习中，我们将以 `job_centre` 模组，构造一个服务器，该模组使用 `gen_server` 实现一项作业管理服务。作业中心会保存一个务必要完成的作业队列。这些作业都有编号。任何人都可将作业添加到这个队列。工作进程可请求该队列中的作业，并告诉作业中心某项作业已完成。这些作业以一些 fun 表示。要执行某项作业 `F`，工作进程必须执行函数 `F()`。


1. 请使用以下接口，实现这个作业中心功能：

    - `job_centre:start_link() -> true.`

        启动作业中心；

    - `job_centre:add_job(F) -> JobNumber.`

        添加一项作业 `F` 到作业队列。返回一个整数的作业编号；

    - `job_centre:work_wanted() -> {JobNumber,F} | no.`

        请求工作。当某个工作进程想要一项作业时，他会调用 `job_centre:work_wanted()`。当队列中有作业时，一个元组 `{JobNumber, F}` 即被返回。工作进程经由执行 `F()` 执行这项作业。当队列中没有作业时，`no` 即被返回。要确保同一作业在同一时间不能分配给多个工作进程。要确保系统是公平的，即作业会按他们被请求的顺序分配。

    - `job_centre:job_done(JobNumber)`

        发出某项作业已完成的信号。当某个作业进程已完成某项作业时，他必须调用 `job_centre:job_done(JobNumber)`。

2. 请添加一个报告队列中作业，与正在进行的作业及已经完成作业状态的统计调用 `job_centre:statistics()`；

3. 请添加监视工作进程的代码。当某个工作进程死掉时，要确保其正执行的作业，会返回到等待完成的作业池；

4. 请检查那些懒惰的工作进程；这是些会接受作业，但不按时交付的工作进程。请将工作请求函数，修改为返回 `{JobNumber, JobTime, F}`，其中 `JobTime` 是工作进程必须在该时间前，完成的以秒计的时间。在 `JobTime - 1` 时刻，当工作进程还未结束这项作业时，服务器应发送一条 `hurry_up` 消息到这个工作进程。在 `JobTime + 1` 时，服务器应以一个 `exit(Pid, youre_fired)`，杀死那个工作进程；

5. 可选题：请实现一个监视工作进程权利的工会服务器。检查他们是否在未收到警告下即被解雇。提示：请使用进程追踪原语，完成这一功能。
