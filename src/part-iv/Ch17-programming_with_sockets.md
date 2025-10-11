# 套接字编程

我（作者）编写的大多数更为有趣的程序，都以一种或另一方式，地涉及到网络套接字。套接字下的编程很有趣，因为他允许应用，与互联网上的其他机器进行交互，这远比仅执行一些本地操作，更具潜力。

所谓套接字，是允许机器使用互联网协议（IP），经由互联网通信的某个信道。在本章中，我们将着重于互联网的两个核心协议：*传输控制协议，TCP*，与 *用户数据报协议，UDP*。

UDP 允许应用程序相互发送一些短的报文（称为 *数据报，datagrams*），但没有这些报文送达的保证。这些报文还会不按顺序到达。相反 TCP 则提供了一旦连接建立，就会按顺序送达的可靠字节流。以 TCP 发送数据，会比以 UDP 发送数据产生更大开销。咱们可选择可靠但较慢的信道（TCP），或较快但不可靠的信道（UDP）。

供套接字编程的主要库有两个：用于编写 TCP 应用的 `gen_tcp`，以及用于编写 UDP 应用的 `gen_udp`。

在本章中，我们将学习如何使用 TCP 和 UDP 套接字，编程一些客户端及服务器。我们将介绍服务器的各种可能形式（并行、顺序、阻塞与非阻塞），并了解如何编程一些可控制到应用数据流的流量整形应用。

## 使用 TCP

我们将从一个从服务器获取数据的简单 TCP 程序，开始我们的套接字编程之旅。这之后，我们将编写一个简单顺序 TCP 服务器，并展示其如何并行化，来处理多个并行会话。

### 自服务器获取数据

我们来以编写一个名为 `nano_get_url/0`，使用 TCP 套接字从 http://httpforever.com 获取 HTML 页面的小函数开始。


```erlang
{{#include ../../projects/ch17-code/socket_examples.erl:10:24}}
```

这段代码会如下工作：

1. 通过调用 `gen_tcp:connect`，我们打开一个到 http://www.httpforever.com 上 80 端口的 TCP 套接字。该连接调用中的参数 `binary`，告诉系统以 “二进制” 模式打开这个套接字，并以将全部数据作为一些二进制值，投送给该应用。`{packet,0}` 表示 TCP 数据会以未经修改的形式，直接投送到该应用；

2. 我们调用了 `gen_tcp:send`，并将消息 `GET / HTTP/1.0\r\n` 发送到这个套接字。然后我们等待回复。回复不会都在一个数据包中，而是会分片到来，每次到来一点。这些分片将作为发送到打开（或控制）该套接字进程的报文序列被接收；

3. 我们会收到一条 `{tcp,Socket,Bin}` 消息。这个元组中的第三个参数，是个二进制值。这是因为我们是以二进制模式，打开的套接字。这条信息是 web 服务器发送给我们的数据分片之一。我们要将其添加到我们已收到分片的列表，并等待下一个分片；

4. 我们收到一条 `{tcp_closed, Socket}` 消息。当服务器已完成向我们发送数据时，这就会发生；

5. 当所有分片都到来时，我们已将他们已错误顺序存储下来，因此我们要逆转顺序，并将所有分片连接起来。


重组数据分片的代码，看起来如下：

```erlang
{{#include ../../projects/ch17-code/socket_examples.erl:18:24}}
```

那么，当分片到达时，我们只需将他们添加到 `SoFar` 这个列表的头部。当所有片段都到达且套接字关闭时，我们就反转这个列表，并将所有分片连接起来。

咱们可能会认为，向下面这样编写代码累积数据分片的代码会更好：

```erlang
receive_data(Socket, SoFar) ->
    receive
        {tcp,Socket,Bin} ->
            receive_data(Socket, list_to_binary([SoFar, Bin]));
        {tcp_closed,Socket} ->
            SoFar
    end.
```

这段代码正确，但会比原始版本低效。原因是在后一版中，我们在持续将一个新的二进制值，追加到缓冲区末尾，这会涉及大量的数据拷贝。将所有分片累积到一个列表（这将以错误顺序结束），然后反转整个列表，并在一次操作中连接所有分片，会好的多。


我们来测试一下，我们的 HTTP 客户端是否工作。

```erlang
1> B = socket_examples:nano_get_url().
<<"HTTP/1.1 200 OK\r\nServer: nginx/1.18.0 (Ubuntu)\r\nDate: Tue, 07 Oct 2025 08:42:42 GMT\r\nContent-Type: text/html\r\nConten"...>>
```

*注意*：当咱们运行 `nano_get_url` 时，结果是个二进制值，所以咱们将看到一个二进制值在 Erlang shell 中，美化打印时的样子。在二进制值被美化打印时，所有控制字符都会以转义格式显示。同时这个二进制值会被截断，表现为打印输出末尾处的三个点（`...>>`）。当咱们想要查看这个二进制值的全部时，咱们可使用 `io:format` 打印，或使用 [`string:tokens`](https://www.erlang.org/docs/17/man/string#tokens-2) 将其拆分为若干片段。


> **编写 Web 服务器**
>
> 编写像是 Web 客户端或服务器之类的东西非常有趣。当然，其他人已经写过这些东西了，但当我们真的想要了解他们的原理时，深入挖掘表象，找出他们的工作原理，就非常有启发性。谁知道呢 -- 也许我们的 web 服务器，将比最好的服务器还要更好。
>
> 要构建一个 web 服务器，或者任何一个实现了标准互联网协议的软件，我们都需要用到正确的工具，并准确了解要实现哪些协议。
>
> 在我们获取 web 页面的示例代码中，我们打开了 80 端口，并发送给他一条 `GET / HTTP/1.0\r\n` 命令。我们使用了定义在 RFC 1945 中的 HTTP 协议。互联网服务的所有主要协议，都定义在 *请求评议，RFC* 中。全部 RFC 的官方网站为 [ietf.org](http://www.ietf.org/)（互联网工程任务组的主页）。
>
> 另一个宝贵的信息来源，是 *数据包嗅探器*。使用数据包嗅探器，我们可捕获并分析进出我们应用的所有 IP 数据包。大多数数据包嗅探器都包含了可解码及分析数据包中的数据，并以有意义方式呈现这些数据的软件。其中最著名也可能是最好的软件，是 Wireshark（以前称为 Ethereal），可从 [wireshark.org](http://www.wireshark.org) 获取。
>
> 有了数据包嗅探器转储和相应 RFC 的加持，我们就可以编写下咱们的一个杀手级应用了。


```erlang
2> io:format("~p~n", [B]).
<<"HTTP/1.1 200 OK\r\nServer: nginx/1.18.0 (Ubuntu)\r\nDate: Tue, 07 Oct 2025 09:00:22 GMT\r\nContent-Type: text/html\r\nContent-Length: 5
124\r\nLast-Modified: Wed, 22 Mar 2023 14:54:48
... several lines omitted ...
>>
3> string:tokens(binary_to_list(B), "\r\n").
["HTTP/1.1 200 OK","Server: nginx/1.18.0 (Ubuntu)",
 "Date: Tue, 07 Oct 2025 09:00:22 GMT",
 "Content-Type: text/html","Content-Length: 5124",
 "Last-Modified: Wed, 22 Mar 2023 14:54:48 GMT",
 "Connection: close","ETag: \"641b16b8-1404\"",
 "Referrer-Policy: strict-origin-when-cross-origin",
 "X-Content-Type-Options: nosniff",
 ... lines omitted ...
```

请注意，响应代码 302 不是个报错；其是这个命令的预期响应，即重定向到某个新地址。还请注意，这个示例展示套接字通信的原理，而并未严格遵循 HTTP 协议。

> **译注**：译者并未使用 www.google.com 作为目标服务器，而是特意使用了 httpforever.com 这个服务器，因此相应代码为 200。

这或多或少就是 web 客户端的工作方式（重点是 *或少* -- 我们将必须完成大量工作，才能在 web 浏览器中正确渲染出结果数据）。不过，前面的代码是咱们自己实验的一个好起点。咱们或许会尝试修改这段代码，获取并存储完整的某个网站，或者自动前往并阅读咱们的电子邮件。可能性是无限的。


### 简单 TCP 服务器


在上一小节中，我们编写了个简单的客户端。现在我们来编写个服务器。

这个服务器会打开 2345 端口，然后等待一条信息。该消息是包含一个 Erlang 项的二进制值。这个项是个包含一个表达式的 Erlang 字符串。服务器会计算这个表达式，并通过将结果写入套接字，将其发送给客户端。


要编写这个程序（及实际上任何透过 TCP/IP 运行的程序），我们必须回答几个简单问题。

- 数据如何组织？我们怎样知道多少数据构成了一次请求或响应？
- 某个请求或响应中的数据，是如何编码及解码的？ （编码数据有时称为 *marshaling*，解码数据有时称为 *demarshaling*。）

>   **译注**：marshaling, 又作 encoding/serialization；与之对应，demarshaling 又作 decoding/deserilization。

TCP 的套接字数据，只是无差别的字节流。在传输期间，这些数据会被分割成任意大小的分片，因此我们需要一些约定，以便清楚多少数据代表一个请求或响应。

> 参考：
>
> The terms "bit," "frame," "packet," and "fragment" represent different units of data at various layers of the network model, illustrating the process of data encapsulation and potential division for transmission.
>
> Bit: The most fundamental unit of data in computing and networking, representing a binary value of either 0 or 1. All other data units are composed of sequences of bits.
>
> Packet: At the Network Layer (Layer 3 of the OSI model), data is referred to as a packet. An IP packet, for instance, contains the source and destination IP addresses, along with the data payload.
>
> Frame: At the Data Link Layer (Layer 2 of the OSI model), a packet is encapsulated within a frame. A frame includes additional information relevant to the local network, such as MAC addresses for source and destination, and error checking mechanisms. An Ethernet frame is a common example.
>
> Fragment: When a packet is too large to fit within the Maximum Transmission Unit (MTU) of a particular network link, it can be divided into smaller pieces called fragments. Each fragment is essentially a smaller packet containing a portion of the original data, along with information (like fragment offset and identification) that allows the receiving device to reassemble the original packet. Fragmentation typically occurs at the Network Layer.

在 Erlang 情形下，我们使用一个简单约定，即每个逻辑的请求或响应之前，都会冠以一个 `N`（1、2 或 4）的字节长度计数。这就是 `gen_tcp:connect` 及 `gen_tcp:listen` 两个函数中，`{packet, N}` 这个参数的含义。这里 `packet` 一词，指一条应用请求或响应消息的长度，而不是在线路上所见到的物理数据包长度。请注意，客户端和服务器所使用的 `packet` 参数 *必须* 一致。当服务器是以参数 `{packet,2}` 打开，而客户端则以 `{packet,4}` 打开时，那么就无法工作。


当咱们以 `{packet,N}` 选项打开某个套接字后，我们就无需担心数据碎片问题。Erlang 的驱动将确保所有碎片数据消息，在传送到应用前都被重新组装成正确长度。

接下来的关注点，是数据的 *编码* 和 *解码*。我们将使用最编码与解码消息的最简单可行方式，即使用 `term_to_binary` 编码 Erlang 项，并使用其反义词 `binary_to_term` 解码数据。

请注意，客户端与服务器对话所需的打包约定和编码规则，被打包在了两行代码中。即在我们打开套接字时，使用 `{packet,4}` 这个选项；以及使用 `term_too_binary` 及其反义词，编码与解码数据。

与 HTTP 或 XML 等基于文本的方法相比，我们能打包及编码 Erlang 项的这种便利，为我们提供了显著优势。使用 Erlang 的 BIF `term_too_binary` 及其反义词 `binary_to_term`，通常要比使用 XML 项执行同等操作，快一个数量级以上，同时还涉及发送少得多的数据。现在来看程序。首先，下面是个极其简单的服务器：


```erlang
{{#include ../../projects/ch17-code/socket_examples.erl:26:46}}
```

这段代码工作如下：

1. 首先我们调用 `gen_tcp:listen`，监听 2345 端口上的连接，并设置好报文打包约定。`{packet,4}` 表示每条应用报文，都将冠以一个长度为 4 字节长的报文头。随后 `gen_tcp:listen(...)` 会返回 `{ok, Listen}` 或 `{error, Why}`，但我们只对我们能打开一个套接字的返回情形感兴趣。因此，我们写下了下面的代码：

    ```erlang
    {ok, Listen} = gen_tcp:listen(...),
    ```

    当 `gen_tcp:listen` 返回 `{error, ...}` 时，这会导致程序抛出一个模式匹配异常。而在成功情形下下，这条语句会将 `Listen`，绑定到那个新的监听套接字。对于某个监听套接字，我们只能做一件事，那就是将其用作 `gen_tcp:accept` 的参数；


2. 现在我们调用 `gen_tcp:accept(Listen)`。此刻，程序将暂停并等待某个连接。当我们获得一个连接时，该函数将返回绑定到一个套接字的变量 `Socket`，其可用于与进行连接客户端对话；

3. 在 `accept` 返回后，我们立即调用 `gen_tcp:close(Listen)`。这会关闭那个监听套接字，因此服务器将不再接受任何新的连接。这不会影响即有连接；其只阻止新的连接；

4. 我们解码输入数据（unmarshaling）；

5. 然后我们计算该字符串；

6. 然后我们编码回复数据（marshaling），并将其发送回那个套接字。

请注意，这个程序只会接受一次请求；一旦程序运行完成，随后别的连接都将不会被接受。

这是演示如何打包和编码应用数据的最简单服务器。他会接受一次请求、计算出一个回复、发送该回复并终止。

要测试该服务器，我们需要个对应的客户端。


```erlang
{{#include ../../projects/ch17-code/socket_examples.erl:48:58}}
```

为测试我们的代码，我们将在同一台机器上，运行客户端和服务器，因此 `gen_tcp:connect` 函数中的主机名，被硬连线到了 `localhost`。

请留意客户端中 `term_too_binary` 被调用来编码消息的方式，以及在服务器上 `binary_too_term` 被调用来重构出消息的方式。

要运行这个示例，我们需要打开两个终端窗口，并在各个窗口中启动 Erlang shell。

首先我们要启动服务器。

```erlang
1> socket_examples:start_nano_server().
```

在服务器窗口中，我们将看不到任何输出，因为什么都还没发生。然后移步客户端窗口，并执行以下命令：

> **译注**：执行下面的命令，观察得到的结果，就会发现在 2345 端口上已经有程序在监听。
>
> ```console
> $ ss -ntlp | grep 2345                                                                                                       ✔
> LISTEN 0      5            0.0.0.0:2345       0.0.0.0:*    users:(("beam.smp",pid=183649,fd=17))
> ```

```erlang
1> socket_examples:nano_client_eval("list_to_tuple([2+3*4,10+20])").
```

在服务器的窗口，我们就会看到如下内容：

```erlang
Server received binary = <<131,107,0,28,108,105,115,116,95,116,111,95,116,117,
                           112,108,101,40,91,50,43,51,42,52,44,49,48,43,50,48,
                           93,41>>
Server (unpacked) "list_to_tuple([2+3*4,10+20])"
Server replying = {14,30}
Server socket closed
ok
```

最后，在服务器窗口中，我们将看到下面这个输出：

```erlang
Server socket closed
```

### 顺序与并行服务器

在上一小节中，我们构造了个只接受一个连接并随后终止的服务器。稍微修改一下这些代码，我们就可以构造出两种不同类型的服务器。

- 顺序服务器 -- 一种一次只接受一个连接的服务器；
- 并行服务器 -- 一种同时接受多个并行连接的服务器。


原先的代码像下面这样启动：

```erlang
start_nano_server() ->
    {ok, Listen} = gen_tcp:listen(...),
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket).
...
```

我们将修改这种方式，构造出我们的两个服务器变种。


**顺序服务器**

要构造一个顺序服务器，我们就要将这段代码，修改为如下：


```erlang
start_seq_server() ->
    {ok, Listen} = gen_tcp:listen(...),
    seq_loop(Listen).

seq_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket),
    seq_loop(Listen).

loop(..) -> %% as before
```

这与上一示例中的工作方式基本相同，但由于我们想要服务多个请求，所以我们就让监听套接字打开，而未调用 `gen_tcp:close(Listen)`。另一区别是在 `loop(Socket)` 结束后，我们再次调用了 `seq_loop(Listen)`，其会等待下一连接。


当某个客户端在服务器忙于某个现有连接时，尝试连接到该服务器，那么这个连接将被排队，直到服务器完成那个现有连接。当排队连接数超出监听积压队列大小，那么该连接将被拒绝。

我们只给出的是启动服务器的代码。停止服务器很简单（停止并行服务器也很简单）；只要杀死启动服务器的进程。`gen_tcp` 会将自身链接到控制进程，并在控制进程死亡时，他就会关闭套接字。


**并行服务器**

构造并行服务器的诀窍在于，当每次 `gen_tcp:accept` 获取一个新连接时，都立即生成一个新的进程。

```erlang
start_parallel_server() ->
    {ok, Listen} = gen_tcp:listen(...),
    spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen) end),
    loop(Socket).

loop(..) -> %% as before
```

这段代码类似于我们前面看到的顺序服务器。最重要的区别，是增加了个 `spawn`，其确保我们会对每个新的套接字连接，创建一个并行进程。现在是比较两者的好机会。咱们应该关注两个 `spawn` 语句的位置，并看看他们如何把顺序服务器，变成了并行服务器。


> **译注**：运行这个并行服务器，并在另一窗口发起请求时，得到的响应如下：
>
> ```erlang
> 1> socket_examples:nano_client_eval("list_to_tuple([3+3*4,10+45,10*100])").
> Client received binary = <<131,104,3,97,15,97,55,98,0,0,3,232>>
> Client result = {15,55,1000}
> ok
> 2> socket_examples:nano_client_eval("list_to_tuple([3+3*4,10+45,10*1000])").
> Client received binary = <<131,104,3,97,15,97,55,98,0,0,39,16>>
> Client result = {15,55,10000}
> ok
> ```
>
> 服务器上的输出为：
>
>
> ```erlang
> 2> socket_examples:start_parallel_server().
> <0.92.0>
> Server received binary = <<131,107,0,35,108,105,115,116,95,116,111,95,116,117,
>                            112,108,101,40,91,51,43,51,42,52,44,49,48,43,52,53,
>                            44,49,48,42,49,48,48,93,41>>
> Server (unpacked) "list_to_tuple([3+3*4,10+45,10*100])"
> Server replying = {15,55,1000}
> Server socket closed
> Server received binary = <<131,107,0,36,108,105,115,116,95,116,111,95,116,117,
>                            112,108,101,40,91,51,43,51,42,52,44,49,48,43,52,53,
>                            44,49,48,42,49,48,48,48,93,41>>
> Server (unpacked) "list_to_tuple([3+3*4,10+45,10*1000])"
> Server replying = {15,55,10000}
> Server socket closed
> ```

全部三种服务器，都调用了 `gen_tcp:listen` 和 `gen_tcp:accept`；唯一的区别是，我们是在并行程序中，还是在顺序程序中，调用的这些函数。


### 注意事项

请留意以下几点：

- 创建出套接字（通过调用 `gen_tcp:accept` 或 `gen_tcp:connect`）的进程，叫做该套接字的 *控制进程*。该套接字上的所有信息，都将发送给这个控制进程；当控制进程死亡时，那么这个套接字将被关闭。通过调用 `gen_tcp:controlling_process(Socket,NewPid)`，套接字的控制进程可被更改为 `NewPid`；

- 我们的并行服务器，可潜在创建出上万连接。我们可能打算限制同时连接的最大数量。这可以通过维护一个任一时间处于活动状态的连接计数器实现。每次得到个新连接时，我们就会递增这个计数器，而在每次某个连接结束时，我们会递减这个计数器。我们可以此限制系统中，同时连接的总数；

- 在我们已接受某个连接后，显式地设置要求的套接字选项是个好主意，就像这样：

    ```erlang
    {ok, Socket} = gen_tcp:accept(Listen),
    inet:setopts(Socket, [{packet,4},binary,
                          {nodelay,true},{active,true}]),
    loop(Socket)
    ```

- 从 Erlang R11B-3 版本开始，就允许多个 Erlang 进程，在同一个监听套接字上调用 `gen_tcp:accept/1`。这简化了构造并行服务器，因为咱们可以有一个预生成进程池，其中所有进程都在 `gen_tcp:accept/1` 状态下等待。


## 主动与被动套接字


Erlang 的套接字，可以三种模式打开：

- *主动，active*；
- *主动一次, active once*；
- 或 *被动, passive*。

这是通过在 `gen_tcp:connect(Address, Port, Options)` 或 `gen_tcp:listen(Port, Options)` 的 `Options` 参数中，加入 `{active, true | false | once}` 选项完成的。

当 `{active, true}` 被指定时，那么一个主动套接字将被创建；`{active, false}` 则会指定一个被动套接字。`{active,once}` 会创建一个主动套接字，但针对一个报文的接收；在其接收完这条报文后，在可接收下一条报文前，其必须被重新启用。


在接下来的几个小节中，我们将介绍这些不同类型套接字的用法。

主动套接字和被动套接字的区别，在于在信息被套接字收到时，会发生什么。

- 在某个主动套接字被创建后，当数据（在该套接字）接收到时，会有 `{tcp, Socket, Data}` 信息发送到控制进程。控制进程无法控制这些信息的流向。某个异常客户端可能会将数千条消息发送到系统，而这些消息都会发送到控制进程。控制进程无法阻止这种消息流；

- 当套接字是在被动模式下打开时，那么控制进程必须调用 `gen_tcp:recv(Socket,N)`，从接收该套接字上的数据。然后，他将尝试精准接收该套接字上的 `N` 个字节。当 `N = 0` 时，就会返回所有可用字节。在这种情况下，服务器可通过选择何时调用 `gen_tcp:recv`，控制来自客户端的消息流。


被动套接字用于控制到服务器的数据流。为了说明这点，我们可以三种方式，编写某个服务器的消息接收循环。


- 主动的消息接收（非阻塞）
- 被动消息接收（阻塞）
- 混合的消息接收（部分阻塞）


### 主动的消息接收（非阻塞）

我们的首个示例，会以主动模式打开一个套接字，然后接收该套接字上的消息。


```erlang
{ok, Listen} = gen_tcp:(Port, [..,{active,true}...]),
{ok, Socket} = gen_tcp:accept(Listen),
loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            ... do something with the data ...
        {tcp_closed, Socket} ->
            ...
    end.
```

此进程无法控制到那个服务器循环的消息流。当客户端产生数据速度，快于服务器消费该数据速度时，那么系统就会被消息 *淹没* -- 消息缓冲区将被填满，进而系统可能崩溃，或行为异常。


这种服务器被称为 *非阻塞* 的服务器，因为他无法阻塞客户端。只有当我们确信非阻塞服务器能够满足客户端需求时，我们才应编写非阻塞服务器。


### 被动消息接收（阻塞式）


在这一小节中，我们将编写个阻塞式的服务器。该服务器会通过设置 `{active, false}` 选项，打开一个被动套接字。这个服务器不会被试图以过多数据，淹没他的过度活跃客户端崩溃掉。


服务器循环种的代码，在其每次打算接收数据时，都要调用 `gen_tcp:recv`。在服务器调用 `recv` 之前，客户端将出现阻塞。请注意，即使 `recv` 未被调用，操作系统也会完成一些允许客户端在阻塞前，发送少量数据的缓冲。


```erlang
{ok, Listen} = gen_tcp:listen(Port, [..,{active,false}...]),
{ok, Socket} = gen_tcp:accept(Listen),
loop(Socket).

loop(Socket) ->
    case gen_tcp:recv(Socket, N) of
        {ok, B} ->
            ... do something with the data ...
            loop(Socket);
        {error, closed} ->
            ...
    end.
```

### 混合方式（部分阻塞）


咱们可能会认为，将被动模式用于所有服务器，便是对的做法。不幸的是，当我们处于被动模式中时，我们只能等待一个套接字上的数据。这对于编写那些必须等待多个套接字上数据的服务器，毫无用处。

幸运的是，我们可以采取一种既非阻塞，也非非阻塞的混合方式。我们以选项 `{active, once}` 打开套接字。在这种模式下，套接字是主动的，*但只对一条消息主动*。在向控制进程发送一条信息后，他必须显式调用 `inet:setopts`，重新启用下一条消息的接收。在此之前，系统将阻塞。这是两全其美的办法。代码如下：


```erlang
{ok, Listen} = gen_tcp:listen(Port, [..,{active, once}...]),
{ok, Socket} = gen_tcp:accept(Listen),
loop(Socket).


loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            ... do something with the data ...
            %% when you're ready enable the next messsage
            inet:setopts(Socket, [{active, once}]),
            loop(Socket);
        {tcp_closed, Socket} ->
            ...
    end.
```


使用 `{active, once}` 这个选项，用户就可实现流量控制的一些高级形式（有时称为 *流量整形*），从而防止服务器被过多信息淹没。


> **找出连接来自何处**
>
> 设想我们编写了某种在线服务器，发现有人不断向我们的网站发送垃圾数据。要尝试防止这种情况，我们就需要指导连接来自何处。要发现这点，我们可调用 `inet:peername(Socket)`。
>
> - `@spec inet:peername(Socket) -> {ok, {IP_Address, Port}} | {error, Why}`
>
>     这个函数会返回连接另一端的 IP 地址和端口，从而服务器可以发现是谁发起的该连接。`IP_Address` 是个整数元组，`{N1,N2,N3,N4}` 表示 IPv4 的 IP 地址，`{K1,K2,K3,K4,K5,K6,K7,K8}` 表示 IPv6 的 IP 地址。这里，`Ni` 是范围 0 至 255 之间的整数，`Ki` 是范围在 0 至 65535 间的整数。

## 套接字下的错误处理


套接字的错误处理极其简单 -- 基本上咱们无需做什么。如前所述，每个套接字都有个控制进程（即创建出该套接字的进程）。当控制进程死掉时，那么该套接字将自动关闭。


这意味着，比如当我们有个客户端和服务器，而由于编程错误服务器死掉时，服务器所拥有的那个套接字将被自动关闭，同时客户端将被发送一条 `{tcp_closed, Socket}` 消息。


我们可以下面的小的程序，测试这种机制：

```erlang
{{#include ../../projects/ch17-code/socket_examples.erl:85:107}}
```

当我们运行他时，我们就会看到如下结果：

```erlang
1> socket_examples:error_test().
connected to: #Port<0.5>
received: <<"123">>
Any={tcp_closed,#Port<0.5>}
=ERROR REPORT==== 10-Oct-2025::10:33:26.068146 ===
Error in process <0.87.0> with exit value:
{badarg,[{erlang,atom_to_list,
                 [<<"123">>],
                 [{error_info,#{module => erl_erts_errors}}]},
         {socket_examples,error_test_server_loop,1,
                          [{file,"socket_examples.erl"},{line,105}]}]}

ok
```

我们生成了个服务器，休眠两秒钟给他启动的机会，然后发送给他一条包含二进制值 `<<"123">>` 的消息。当这条信息到达服务器时，服务器会尝试计算 `atom_to_list(Data)`，其中 `Data` 是个二进制值，因此会立即崩溃。系统监视器会打印出咱们在 shell 中看到的诊断结果。既然该套接字的服务器侧控制进程已经崩溃，那么（服务器侧）的套接字，就会被自动关闭。然后客户端即会被发送一条 `{tcp_closed, Socket}` 消息。


## UDP

现在我们来看看用户数据报协议（UDP）。使用 UDP，互联网上的机器可互相发送一些称为 *数据报* 的短信息。UDP 数据报是不可靠的。这意味着，当某个客户端将某个 UDP 数据报的序列，发送给某个服务器时，随后这些数据报可能不按顺序到达，或者根本没有到达，或甚至不止一次到达，但当单个数据报到达时，他们将是非受损的。大的数据报可被分割成较小的分片，而 IP 协议会在将这些片段传送给应用前，将重新组装这些分片。


UDP 是种 *无连接* 协议，表示在向服务器发送信息前，客户端不必建立连接。这意味着 UDP 非常适合那些有大量客户端，向服务器发送小的消息应用。


编写 Erlang 的 UDP 客户端和服务器，要比编写 TCP 情形下的容易得多，因为我们不必担心维护与服务器的连接。


### 最简单的 UDP 服务器与客户端


首先我们来谈谈服务器。UDP 服务器的一般形式如下：


```erlang
server(Port) ->
    {ok, Socket} -> gen_udp:open(Port, [binary]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp, Socket, Host, Port, Bin} ->
            BinReply = ...,
            gen_udp:send(Socket, Host, Port, BinReply),
            loop(Socket)
    end.
```

这要比 TCP 情况简单，因为我们无需担心我们的进程会收到 `"socket closed"` 消息。请注意，我们打开的是 *二进制* 模式套接字，这会告诉驱动程序，将所有消息作为二进制数据，发送给控制进程。

现在是客户端。这是个非常简单的客户端。他只是打开个 UDP 套接字、发送一条信息到服务器、等待回复（或超时），然后关闭套接字并返回由服务器返回的值。


```erlang
client(Request) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    ok = gen_udp:send(Socket, "localhost", 4000, Request),
    Value = receive
                {udp, Socket, _, _, Bin} ->
                    {ok, Bin}
            after 2000 ->
                error
            end,
    gen_udp:close(Socket),
    Value.
```

我们必须要有个超时时间，因为 UDP 是不可靠的，进而我们可能不会真正得到一个回复。

### UDP 阶乘服务器


我们可轻松构建一个计算发送给他任何数字阶乘的 UDP 服务器。代码是以上一小节中的代码为蓝本。


```erlang
{{#include ../../projects/ch17-code/udp_test.erl}}
```

请注意，我（作者）添加了一些打印语句，这样我们就能看到，在我们运行程序时发生了什么。在开发程序时，我（作者）总是会添加一些打印语句，然后在程序可工作时，编辑或注释掉他们。

现在我们来运行这个示例。首先我们启动服务器。

```erlang
1> udp_test:start_server().
<0.96.0>
server opened socket: #Port<0.7>
```

这会在后台运行，因此我们可以构造一个求 40 阶乘值的客户端请求。


```erlang
1> udp_test:client(40).
client opened socket = #Port<0.3>
client received: {udp,#Port<0.3>,
                      {127,0,0,1},
                      4000,
                      <<131,110,20,0,0,0,0,0,64,37,5,255,100,222,15,8,126,242,
                        199,132,27,232,234,142>>}
815915283247897734345611269596115894272000000000
```

> **译注**：原文是在同一个 Erlang shell 下启动服务器，并构造该客户端请求。那样客户端和服务器的打印输出混在一起，影响可读性。这里是在另一个 Erlang shell 窗口中构造客户端请求，而在服务器启动 Erlang shell 窗口中的输出如下。
>
> ```erlang
> server received: {udp,#Port<0.3>,{127,0,0,1},49950,<<131,97,10>>}
> ```
>
> 若在同一个 Erlang shell 窗口中构造客户端请求，则混合的输出如下。
>
> ```erlang
> 2> udp_test:client(20).
> client opened socket = #Port<0.4>
> server received: {udp,#Port<0.3>,{127,0,0,1},58999,<<131,97,20>>}
> client received: {udp,#Port<0.4>,
>                       {127,0,0,1},
>                       4000,
>                       <<131,110,8,0,0,0,180,130,124,103,195,33>>}
> 2432902008176640000
> ```

现在我们就有了个小小的 UDP 的阶乘服务器。为了好玩，咱们可尝试编写与此程序等价的 TCP 对等程序，并对两者进行基准测试。


### UDP 数据包陷阱

我们应该注意到，由于 UDP 是种无连接协议，服务器无法通过拒绝读取来自客户端数据来阻止客户端 -- 服务器不知道客户机是谁。

大的 UDP 数据包在通过网络时，可能会被分片。当 UDP 数据的大小，大于了数据包在网络上传输时，所经过路由器的允许最大传输单元 (MTU) 大小时，就会出现分片。优化 UDP 网络的一个常见建议，便是从一个较小的数据包大小（比如约 500 字节）开始，然后在测量吞吐量的同时，逐渐将其增大。当在某个时刻吞吐量急剧下降时，咱们就知道数据包太大了。

某个 UDP 数据包可被两次投送（这会惊讶到一些人），因此咱们必须仔细编写远程过程调用的代码。比如就可能出现对某个第二次查询的回复，实际上是对第一次查询的一个重复回复。为避免这种情况，我们可将客户端代码，修改为包含一个唯一引用，并检查该引用是否被服务器返回。为生成唯一引用，我们要调用 Erlang 的内建函数 `make_ref`，其保证会返回一个全局唯一的引用。现在的远程过程调用代码，看起来如下：


```erlang
client(Request) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    Ref = make_ref(),   %% make a unique reference
    B1 = term_to_binary({Ref, Request}),
    ok = gen_udp:send(Socket, "localhost", 4000, B1),
    wait_for_ref(Socket, Ref).

wait_for_ref(Socket, Ref) ->
    receive
        {udp, Socket, _, _, Bin} ->
            case binary_to_term(Bin) of
                {Ref, Val} ->
                    %% got the correct value
                    Val;
                {_SomeOtherRef, _} ->
                    %% some other value throw it away
                    wait_for_ref(Socket, Ref)
            end
    after 1000 ->
        ...
    end.
```

现在我们学完了 UDP。UDP 通常用于有低延迟要求，同时即使偶尔丢失数据包也无所谓的在线游戏。


## 广播到多台机器

最后，我们将看看设置广播信道的方式。下面这段代码非常简单。


```erlang
{{#include ../../projects/ch17-code/broadcast.erl}}
```


## SHOUTcast 服务器
