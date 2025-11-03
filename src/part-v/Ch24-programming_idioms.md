# 编程习语

在这一章中，我们将探究一些编程习语，并探讨架构 Erlang 代码的一些不同技巧。我们将以一个展示我们应如何看待编程世界，及我们在这个世界中发现的那些对象的示例。


## 维护 Erlang 的世界观


Erlang 的世界观即是，*万事万物皆为进程*，而进程只能通过交换消息交互。有了这样一种世界观，我们的设计就有了 *概念上的完整性*，让我们的设计而更容易理解。


设想我们打算编写一个 Erlang 的 web 服务器。某名用户请求我们服务器上的一个名为 `hello.html` 的页面。最简单可行 web 服务器看起来像下面这样：


```erlang
{{#include ../../projects/ch24-code/demo.erl:1:11}}
```

而这段代码之所以简单，仅仅因为他所完成的，只是接收和发送一些 Erlang 的项。*但客户端并未发送给我们 Erlang 的项；他们发送给我们了一些复杂得多的 HTTP 请求*。HTTP 请求经由 TCP 连接到来，进而这些请求本身可能被分片了，所有这些令到服务器程序，要远比前面给出的简单代码复杂得多。

为简化起见，我们在接收来自 HTTP 客户端消息的 TCP 驱动，和咱们的 Erlang 服务器间，插入了个称为 *中间人* 的进程。这位中间人会解析 HTTP 请求，并将其转化为 Erlang 的消息。如下图所示。咱们可以看到，为何这个转译进程称为中间人；他位于 TCP 驱动和 web 服务器之间。


![中间人进程](../images/middle_man.png)


就服务器而言，外部世界的对象只会 “讲” Erlang 语言。与其完成两件事（处理 HTTP 请求和为请求提供服务）的一个进程，我们现在有两个进程，每个都有明确的职责。中间人只知道怎样在 HTTP 和 Erlang 消息间转换。服务器对 HTTP 协议的细节一无所知，而只处理纯 Erlang 的消息。将这一目的拆分为两个进程，不仅使设计更加清晰，而且有个额外好处；他可以提高并发性。两个进程可以并行执行。

满足 HTTP 请求的相关消息流，在 [图 5，* Web 服务器协议*](#fig-5) 中给出了。


中间人的具体原理与这一讨论无关。他必需完成的，就是解析传入的 HTTP 请求，将其转换为 Erlang 的项，并将传出的 Erlang 项，转换为 HTTP 响应。


在我们的示例中，我们选择了把 HTML 请求中的大量细节抽象出来。HTML 请求的头部，包含着我们并未在此展示的许多额外信息。作为中间人设计的一部分，我们必须决定，要将多少底层协议细节，向 Erlang 应用公开。


假设我们打算扩展这个示例，而要响应一些文件的 FTP 请求，或经由 IRC 频道发送文件的请求。我们可像 [图 6，*统一消息*](#fig-6) 中所示的方式，架构我们系统中的进程。


HTTP、FTP 和 IRC 三者使用了 *完全不同的协议* 在机器间传输文件。实际上，IRC 并不支持文件传输，而文件传输通常由 *直接客户端到客户端，Direct Client to Client, DCC * 协议支持，大多数 IRC 客户端都支持该协议。


![Web 服务器协议](../images/web_server_protocol.png)

<a name="fig-5"></a>
**图 5** -- **Web 服务器协议**


![统一消息](../images/unified_messages.png)

<a name="fig-6"></a>
**图 6** -- **统一消息**

在中间人将外部协议转换为 Erlang 的消息后，单个的 Erlang 服务器就可作为所有这些不同协议的后端。


使用统一的 Erlang 消息传递，切实简化了这些实现。他有着以下优点：

- 他抽象出了不同线路协议（例如 HTTP 和 FTP 协议）间的区别；
- Erlang 消息不需要解析器。在可处理消息之前，接收进程不必解析消息。与此相比，HTTP 服务器就必须解析其接收的所有消息；
- Erlang 消息可包含任意复杂度的项。与此相比，HTTP 消息在传输前，必须序列化为边平形式；
- Erlang 消息可跨处理器边界（译注：不同 Erlang 节点？）发送，或以简单通用的序列化格式，存储在数据库中。


## 多用途服务器

一旦我们打消了单独服务需要有其各自不同消息格式这一想法，我们就可以使用统一消息传递，解决一系列问题。例如，下面是某个 “多服务器”：

```erlang
{{#include ../../projects/ch24-code/multi-server.erl}}
```

这段代码模仿了数种熟知服务的 *基本行为*。


- *在第 8 至 11 行中，他像 email 客户端那样行事*；

    Email 客户端的 *基本* 工作，是接收电子邮件并将其存储在咱们电脑上，通常是在一个名为 `mbox` 的文件中。我们收到一条消息，打开名为 `mbox` 的文件，将该消息写入这个文件，然后就完成了。

- *在第 12 至 13 行中，他如即时消息客户端那样行事*；

    即时信息客户端的 *基本* 工作，是接收一条消息并告诉用户。我们通过写下一条信息到控制台，告诉用户。

- *在第 14 至 15 行中，他如 FTP/RCP/HTTP 服务器那样行事*（勘误：原文中此处存在拼写错误，“In lines 12 to 13”）。

    FTP 服务器、HTTP 服务器或任何其他文件服务器的 *基本*任务，都是将服务器上的某个文件传输到客户端。


看着这段代码，我们就会意识到，我们其实并不需要客户端-服务器请求与响应的那么多不同编码。一种通用格式就足够了；Erlang 的项，会在全部消息中使用。


由于两个 BIFs，`term_to_binary(Term)` 及恢复 Erlang 项的反义词 `binary_to_term(Bin))` 的特性，所有这些优点，在分布式的环境下均有效。


在分布式系统中，`binary_to_term(Bin)` 可以从存储在 `Bin` 中的 *项的外部表示*，重建出任何的 Eralng 项。通常，`Bin` 经由某个套接字来到机器，不过细节在这里并不重要。`binary_to_term` 只会重建出项。在诸如 HTTP 等协议下，输入请求必须要被 *解析*，这会使得整个过程本质上效率低下。


通过添加一对执行我们感兴趣部分的对称函数，我们就可以实现加密与压缩层。下面是个示例：


```erlang
{{#include ../../projects/ch24-code/demo.erl:13:15}}
```

当我们打算经由网络发送加密的、压缩后的、手机代码时，我们可这样做：

```erlang
{{#include ../../projects/ch24-code/demo.erl:18:23}}
```

这里我们结合了三种想法：

- 使用 `term_to_binary` 及其逆函数，经由网络发送 Erlang 项；
- 使用 `apply` 执行代码；
- 以及使用对称函数对，压缩/解压及加密/解密数据。


请注意，压缩/解压和加密/解密不是 Erlang 的 BIFs，而只是假定存在的一些函数。

对于 Erlang 程序员来说，这个世界是个非常美好之处。一旦相应的中间人进程已编写，所有外部进程就都讲 Erlang 语言。这切实简化了复杂系统，尤其是当大量不同外部协议用到时。

这就像一个人人都将英语（或普通话）的世界 -- 交流起来会更容易。


## 有状态模组


运用一种称为 *元组模组，tuple modules* 机制，我们可安排将状态，与模组名字封装在一起。我们可利用这种机制隐藏信息，已及创建出对用到某个接口的程序，隐藏该接口细节的适配器模组。当我们打算构造多个不同模组共用的通用接口，或模仿面向对象编程的一些特性时，这样做非常有用。

当我们调用 `X:Func(....)` 时，`X` 不必是个原子。他可以是个元组。当我们写下 `X = {Mod, P1, P2, ..., Pn}`，然后调用 `X:Func(A1, A2, ..., An)` 时，那么实际上调用的是 `Mod:Func(A1, A2, ..., An, X)`。例如，调用 `{foo,1,2,3}:bar(a,b)` 会被转换为调用 `foo:bar(a,b,{foo,1,2,3})`。


运用这种机制，我们就可以创建出 “有状态的” 模组。我们将先以一个简单的有状态计数器演示这点，然后延续到创建两个现有模组的适配器模组示例。


### 带有状态的计数器

为说明元组模组这个概念，我们将以有着表示计数器值的单个状态参数 `N` 的计数器简单示例开始。代码如下：


```erlang
-module(counter).
-export([bump/2, read/1]).

bump(N, {counter,K}) -> {counter, N + K}.

read({counter, N}) -> N.
```


我们可如下测试这段代码。首先我们要编译这个模组。


```erlang
1> c(counter).
{ok,counter}
```

然后创建一个元组模组实例。


```erlang
2> C = {counter,2}.
{counter,2}
```

并调用 `read/0`。

> **译注**：在 Erlang/OTP 28 下，这一特性已不工作，而会报出以下错误。
>
> ```erlang
> 3> C:read().
> ** exception error: bad argument
>      in function  apply/3
>         called as apply({counter,2},read,[])
>         *** argument 1: not an atom
> ```
>
> 报错显示调用 `C:read()`，并未如上述语法那样，被转换为 `counter:read()`，因此无法被 `apply/3` 执行（`counter:read(C)` 和 `apply(counter, read, [{counter,2}])` 均可得到正确的输出）。
>
> 进一步调查发现，元组模组特性，tuple module，是参数化模组特性，parameterized module，的实现机制。参数化模组在 Erlang/OTP R16 中已被移除，而元组模组也在 Erlang/OTP 21 及以后版本中正式关闭。
>
> 要为兼容旧代码启用这一历史特性，咱们需要在咱们的模组中，添加一个特定编译选项 `tuple_calls`。该选项指示 Erlang 编译器，允许在特定模组中使用元组模组的调用。
>
> 因此，将上面的 `counter.erl` 程序代码，修改成下面这样即可使用元组模组，实现有状态模组这一模式。
>
> ```erlang
> -module(counter).
> -export([bump/2, read/1, test/0]).
> -compile(tuple_calls).
>
> bump(N, {counter,K}) -> {counter, N + K}.
>
> read({counter, N}) -> N.
>
> test() ->
>     C = {counter,2},
>
>     %% 由于 C 是个元组，因此这会被转换为调用 `counter:read({counter,2})`，这会返回 `2`。
>     C:read(),
>
>     %% `C:bump(3)` 会被转换为调用 `counter:bump(3, {counter, 2})`，而因此会返回 `{counter, 5}`。
>     C1 = C:bump(3),
>     C1:read().
> ```
>
>
> 而 Erlang shell，即 Erlang 的运行时系统应用，已在 Erlang/OTP 22.0 中，移除了于 21.3 中引入的 `+ztma` 命令行开关。导致 Erlang shell 下无法运行带有元组模组的代码。
>
> 参考：
>
> - [What is a tuple module in Erlang?](https://stackoverflow.com/a/16962661/12288760)
>
> - [The Trouble with Erlang (or Erlang is a ghetto)](https://chris.improbable.org/2011/7/26/the-trouble-with-erlang-or-erlang-is-a-ghetto/)
>
> - [Parameterised Modules in Erlang](https://stackoverflow.com/a/5344182/12288760)
>
> - [SimpleBridge](https://hexdocs.pm/simple_bridge/readme.html#what-can-i-do-with-the-bridge)
>
> - [Compiler Release Notes: Improvements and New Features](http://erlang.org/documentation/doc-10.0/lib/compiler-7.2/doc/html/notes.html)
>
> - [Erlang/OTP 22: ERTS Release Notes](https://www.erlang.org/docs/22/apps/erts/notes)

请注意其中，模组名字 `counter` 及状态变量，是怎样对元组 `C` 与 `C1` 中的调用代码隐藏的。


> **译注**：以下内容参考自 Google AI 答案。

在现代 Erlang 中，“有状态模组” 通常是指将状态封装在进程中，并在进程中管理状态的模组，而不是其函数只对他们的参数与返回值操作，未在调用间保留信息的那些无状态模组。在 Erlang 中实现有状态模组的主要方法，是经由行为的运用，尤其是 `gen_server` 和 `gen_statem` 这两种行为。

+ **`gen_server` 行为**

    `gen_server` 行为是 Erlang 中实现有状态进程最常见、最广泛使用的方法。他提供了管理处理请求与维护内部状态类服务器进程的一种健壮框架。

    - **结构**：`gen_server` 模组会实现一组定义服务器如何初始化、如何处理不同类型的消息（同步调用、异步调用及其他消息），以及如何管理其生命周期的回调函数（如 `init/1`、`handle_call/3`、`handle_cast/2`、`handle_info/2`、`terminate/2`、`code_change/3` 等，译注：参见 [`gen_server` 的结构](../part-iv/Ch22-introducing_otp.md#gen_server-回调的结构)）。

    - **状态管理**：`gen_server` 的内部状态，被作为参数传递给这些回调函数，并作为他们返回值的一部分返回，从而允许状态在每次交互下被更新。

    - **示例代码**：

        ```erlang
        -module(my_stateful_server).
        -behaviour(gen_server).

        -export([start_link/0, get_state/0, increment/0]).
        -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

        start_link() ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

        get_state() ->
            gen_server:call(?MODULE, get_state).

        increment() ->
            gen_server:cast(?MODULE, increment).

        init([]) ->
            {ok, 0}. % Initial state is 0

        handle_call(get_state, _From, State) ->
            {reply, State, State}.

        handle_cast(increment, State) ->
            NewState = State + 1,
            {noreply, NewState}.

        handle_info(_Info, State) ->
            {noreply, State}.

        terminate(_Reason, _State) ->
            ok.

        code_change(_OldVsn, State, _Extra) ->
            {ok, State}.
        ```

+ **`gen_statem` 行为**

    `gen_statem` 行为设计用于实现状态机，其中进程的行为取决于其当前状态，及其接收到的事件。他提供了管理复杂的状态转换的一种更有条理的方法。


    - **结构**：与 `gen_server` 类似，`gen_statem` 也使用回调函数，但这些回调通常是围绕状态及事件组织的。`callback_mode/0` 函数定义了事件如何被分派给特定于状态的函数（如 `state_functions` 或 `handle_event_function` 等）。

    - **状态与数据**：`gen_statem` 会区分 “状态”（一个表示当前状态的原子）和 “数据”（与状态关联的内部数据）。在状态转换期间，两者都受到管理。

    - **示例代码**（简化版）：

        ```erlang
        -module(my_state_machine).
        -behaviour(gen_statem).

        -export([start_link/0, trigger_event/1]).
        -export([init/1, callback_mode/0, locked/3, open/3]).

        start_link() ->
            gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

        trigger_event(Event) ->
            gen_statem:cast(?MODULE, Event).

        init([]) ->
            process_flag(trap_exit, true),
            {ok, locked, #{}}. % Initial state: locked, empty data

        callback_mode() -> state_functions.

        locked(cast, {button, Digit}, Data) ->
            case Digit of
                1 -> {next_state, open, Data};
                _ -> {keep_state, Data}
            end;
        locked(_EventType, _EventContent, Data) ->
            {keep_state, Data}.

        open(cast, close_button, Data) ->
            {next_state, locked, Data};
        open(_EventType, _EventContent, Data) ->
            {keep_state, Data}.
        ```

+ **在 `gen_server` 与 `gen_statem` 间选择**

    - 对主要管理单个、不断变化，并响应各种请求的处理通用服务器进程，使用 `gen_server`；
    - 在进程行为是显式地由有限状态机定义，而有着清晰的状态和由事件触发的转换时，则使用 `gen_statem`。


+ **其他方法**（较少用于通用有状态模组）

    - **简单的循环进程**：对于非常基本的状态管理，进程可以实现一个将当前状态作为参数并返回新状态的简单递归循环。这种方式结构性较差，而通常用于内部的流程逻辑；
    - **元组模组**（复杂/不常用）：正如一些旧资料中提到的，使用一个用到 “元组模组” 的模组名字封装状态是可行的，但与使用行为相比，这是一种更复杂而更不常用的方法。

## 适配器模式

设想我们有两个或多个功能大致相同的库，但却无法决定使用哪个。这些库可能有着类似的功能接口，但有着不同性能特征。例如，考虑键值存储：一中存储可能会将键和值保存在内存中，另一种则可能将键保存在内存中，而值保存在磁盘上。还有一种可能会以一些较小的值，将键保留在内存中，而将较大的值存储在磁盘上。即使是像键值存储这样简单的东西，存储具体被实现的方式也有很多变化。


设想我们打算编写一些要用到键值存储的代码。在咱们编写咱们的应用时，我们必须做出设计决策，并从那些可用键值存储中，选择某种特定存储。在很久以后，我们的某些设计决策可能会证明是错误的，我们可能会打算更改后端存储。不幸的是，当用于访问新旧存储的 API 不同时，那么我们可能必须对咱们的程序，进行大量修改。

这就是 *适配器* 模式的用武之地。所谓适配器，是一些为应用提供统一接口的元组模组。

我们将通过构建一种提供到某个分别使用 `lists` 和 `dict` 模组，实现的键值存储同一接口的适配器模式。我们适配器的接口如下：


- `adapter_db1:new(Type :: dict | lists) -> Mod`

    创建一个 `Type` 类型的新键值存储。返回一个元组模组 `Mod`；

- `Mod:store(Key, Val) -> Mod1`

    将一个键、值对，存储在该存储中。`Mod` 是原有存储；`Mod1` 是新的存储；

- `Mod:lookup(Key) -> {ok, Val} | error`

    在该存储中查找 `Key`。当存储中有个值时，这会返回 `{ok, Val}`；否则返回 `error`。


要使用这个 API，我们要编写如下代码：

```erlang
M0 = adapter_db1:new(dict), ...
M1 = M0:store(Key1, Val1),
M2 = M1:store(Key2, Val2),
...
ValK = MK:lookup(KeyK),
```

当我们打算使用 `lists` 的实现时，我们只要将创建模组的代码行，改为 `Mod = adapter_db1:new(lists)`。


出于兴趣，我们可将这段代码，与咱们曾使用 `dict` 模组时的编码风格比较。使用 `dict`，我们会写下类似下面这样的代码：


```erlang
D0 = dict:new(),
D1 = dict:store(Key1, Val1, D0),
D2 = dict:store(Key2, Val2, D1),
...
ValK = dict:find(KeyK, Dk)
```

用于访问元组模组的代码要短一些，因为我们可将所有内部细节，隐藏在单个变量 `Modi` 中。使用 `dict` 需要两个参数：模组名称和字典结构本身。

现在咱们来编写这个适配器。


```erlang
{{#include ../../projects/ch24-code/adapter_db1.erl}}
```

> **译注**：这里增加了 `tuple_calls` 编译选项，以启用默认关闭的元组模组特性。原因见前面的译注。

这次我们的模组是由一个 `{adapter_db1, Type, Val}` 形式的元组表示。当 `Type` 为 `list` 时，那么 `Val` 就是个列表；当 `Type` 为 `dict` 时，那么 `Val` 就是个字典。

我们可在一个单独模组中，编写一些简单代码测试这个适配器。

```erlang
{{#include ../../projects/ch24-code/adapter_db1.erl}}
```

```erlang
1> adapter_db1_test:test().
ok
```

测试成功了。因此，我们现在已实现了咱们将两个具有不同接口的不同模组，隐藏在一个提供了到这两个模组的共用接口的适配器模组之后。

对于提供到已有代码的通用接口，适配器非常有用。接口可保持不变，而适配器后面的代码可被修改，以反映不同需求。


## 有意的编程


所谓有意编程，是给一种编程风格取的名字，其中我们可轻易看出程序员的意图。程序员的意图，应从有关函数的名字中显而易见，而不是经由分析代码结构推断出。以一个示例说明这一点最好不过。在 Erlang 早期，库模组 `dict` 导出了个有着如下接口的函数 `lookup/2`，：


```erlang
lookup(Key, Dict) -> {ok, Value} | not_found
```

在这一定义下，`lookup` 可用于三种不同上下文中。

1. 对于 *数据检索*，我们可这样写:

    ```erlang
    {ok, Value} = lookup(Key, Dict)
    ```

    这里 `lookup` 被用于以该字典中的某个已知键，提取某个条目。当这个键不在该字典中，`not_found` 将被返回，那么一个模式匹配报错就将发生，进而程序将抛出一个异常。退出原因将是 `{badmatch, not_found}`。这是个糟糕的错误消息。更翔实的错误消息，应是 `{bad_key, Key}`；


2. 对于 *搜索* 的上下文，我们会写下如下代码：

    ```erlang
    case lookup(Key, Dict) of
        {ok, Val} ->
            ... do something with Val ...
        not_found ->
            ... do something else ...
    end.
    ```

    我们可以看到，程序员并不知道这个键是否在该字典中，因为他们写下了会将 `lookup` 的返回值，与 `{ok, Val}` 及 `not_found` 同时模式匹配的代码。我们还可以看出，对 `Val` 进行了一些操作（从注释看出）。我们还可从这段代码中推断出，程序员希望搜索字典中的某个值。当我们不知道某个东西在何处时，我们就会搜索他。


3. 对 *判断某个键是否存在* 的上下文，以下代码片段：


    ```erlang
    case lookup(Key, Dict) of
        {ok, _} ->
            ... do something ...
        not_found ->
            ... do something else ...
    end.
    ```

    会判断某个特定键是否在字典中。通过注意到 `lookup` 的两个可能返回值，都进行了模式匹配，而找到的项的值从未被使用过，我们便可推断出这点。我们之所以能看出这点，因为我们模式匹配了 `{ok, _}`，而不是 `{ok, Val}`（如第一个示例中）。由于我们从未使用过与键关联的值，我们可以假定 `lookup` 被调用，来测试该键是否存在。


以上三个示例，重载了 `lookup` 这个函数的含义。他被用于了三种不同目的：数据检索、搜索及判断某个键是否存在。


与其猜测程序员的意图与分析代码，调用一个显式指明了这三个选项中，哪个是想要的库例程就要更好。`dict` 为此目的导出了三个函数。


```erlang
dict:fetch(Key, Dict)  = Val | EXIT
dict:search(Key, Dict) = {found, Val} | not_found.
dict:is_key(Key, Dict) = Boolean
```

这些函数准确地表达了程序员的意图。不涉及猜测或程序分析，函数的名字就清楚地告诉了我们程序员的以图。当某个键可能存在于字典中时，`search` 会被调用，当该键不存在时也不是错误。当某个键值必然在字典中时，`fetch` 会被调用，当该键未出现时则就是个错误。`is_key` 被调用来判断该键是否出现在某个字典中。


与使用（`fetch`、`search`、`is_key`）这些替代做法的代码相比，使用 `lookup` 编写的代码，将更难于理解和维护。


咱们应从本章中汲取的最重要思想，是中间人这种概念。外部世界中的一切，都应被建模为 Erlang 的进程这一思想极为重要，是构造可顺利组合在一起组件的核心。


在下一章中，我们将探讨如何与他人共用咱们的代码，以及把我们的工作与他人的工作集成，同时我们将介绍本书中一些示例中，用到的一些第三方工具。通过使用他人的代码，我们可更快地解决问题，而通过共享咱们自己的代码，我们也可帮助到别人。当咱们帮助别人时，别人也会帮助咱们。


## 练习

1. 请扩展 `adapter_db1` 中的适配器，以便 `adapter_db1:new(persistent)` 这个调用，会创建出带有持久化数据存储的一个元组模组；

2. 请编写一个将一些小的值存储在内存中，大的值存储在磁盘上的键值存储。制作一个使这个存储，具有与本章早先那个适配器相同接口的适配器模组；

3. 请构造一个其中一些键值对是持久的，另一些则是短暂的的键值存储。调用 `put(Key,memory,Val)` 将把一个 `Key`、`Val` 对存储在内存中。而 `put(Key,disk,Val)` 则应在磁盘上存储数据。要使用一对进程完成此目的，一个用于持久存储，另一个用于磁盘存储。请重用本章前面的代码。
