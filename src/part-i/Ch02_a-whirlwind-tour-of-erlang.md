# Erlang 旋风之旅


在本章中，我们将构建我们的首个并发程序。我们将构造一个文件服务器。该文件服务器有两个并发进程：一个进程代表服务器，另一个代表客户端。


我们将从一个小的 Erlang 子集开始，这样我们就可以展示一些广泛的原则，而不必纠缠于细节。至少，我们必须了解如何在 shell 中运行代码及编译模组。这就是我们入门所需的全部知识。


学习 Erlang 的最佳方法，是将示例输入到实际的 Erlang 系统中，看看能否重现书中的内容。要安装 Erlang，请参阅 [Installation Guide](https://www.erlang.org/docs/22/installation_guide/users_guide)。我们会尽量更新安装说明。这很困难，因为有许多不同平台以不同方式进行配置。如果这些安装指令失败或不是最新的，请发送邮件到 Erlang 的邮件列表，我们会尽力提供帮助。


## 关于 Erlang shell


Erlang shell 是咱们将耗费咱们大部分时间的地方。在咱们输入某个表达式后，shell 就会对该表达式求值并打印结果。


```shell
$ werl
Erlang/OTP 28 [erts-16.0.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Eshell V16.0.2 (press Ctrl+G to abort, type help(). for help)
1> 123456 * 223344.
27573156864
2>
```

那么，发生了什么？其中 `$` 是操作系统的提示符。我们输入了 `werl` 命令，他启动了 Erlang shell。Erlang shell 会以一个横幅及编号的提示 `1>` 响应。然后，我们输入了一个表达式，该表达式被求值并打印出来。注意每个表达式都 *必须* 以一个句点结束，后跟一个空格符。在这个语境下，空格是指空格、制表符或回车符。


初学者经常会忘记用这个点空白位，结束表达式。请把命令想象成英语句子。英语的句子通常以句点结束，因此很容易记住。


### `=` 运算符


使用 `=` 运算符，我们可以为变量赋值（严格来说，这叫做将该变量 *绑定* 到某个值），就像这样：


```erlang
2> X = 123.
123
3> X * 2.
246
```

若我们试图更改某个变量的值，就会发生一些奇怪的事情。

```erlang
4> X = 999.
** exception error: no match of right hand side value 999
```

这是第一个惊喜。我们无法 *重新绑定* 变量。Erlang 是门函数式语言，所以一旦我们说 `X = 123`，那么 `X` 就永远是 `123`，不能更改！


别担心，这是个好处，不是个问题。想比于同一变量在程序生命周期可获得许多不同值的程序，其中变量一旦设定就不能更改的程序要更容易理解。


当我们看到类似 `X = 123` 这样的表达式时，他的意思似乎是 “将整数 `123` 赋值给变量 `X`”，但这种解释是不正确的。`=` 不是个赋值运算符；他实际上是个 *模式匹配运算符*。这在 [变量绑定和模式匹配](../part-ii/Ch03-basic_concepts.md#变量绑定与模式匹配) 小节会详细介绍。


与在函数式编程语言中一样，Erlang 中的变量只能绑定一次。绑定某个变量意味着给了变量一个值；一旦其被绑定，以后该值就不能被更改。


如果咱们习惯了命令式语言，可能会觉得这种想法很奇怪。在命令式语言中，变量实际上是一种指向内存地址的变相方式。某个程序中的 `X`，其实就是内存中某个数据项的地址。当我们说 `X=12` 时，我们是在改变地址为 `X` 处内存位置的值，但在 Erlang 中，变量 `X` 表示了一个永远不会被改变的值。



### 变量与原子的语法

**Syntax of Variables and Atoms**


请注意，Erlang 的变量以大写字母开头。因此，`X`、`This` 和 `A_long_name` 都属于变量。以小写字母开头的名字 -- 例如，`monday` 或 `friday` -- 则不属于变量，而是一些称为 *原子* 的符号常量。


若咱们曾看到或写过某个类似 `x = 123` 的表达式（注：这里的 `x` 是用小写字母书写的，以防咱们未注意到），那么几乎可以肯定这是个错误。当咱们在 shell 中这样做时，反应是立竿见影的。


```erlang
1> abc=123.
** exception error: no match of right hand side value 123
```

但是如果这样的一行代码深埋在一些代码中，他就可能会让咱们的程序崩溃，所以请注意。诸如如 Emacs 和 Eclipse 编辑器等的大多数编辑器，都会以不同颜色对原子与变量进行颜色编码，因此很容易看出区别。


在咱们阅读下一小节之前，请尝试启动 shell 并输入几个简单的算术表达式。在此阶段，当出错时，只需通过键入 `Control+C` 然后键入 `a`（表示中止，abort），然后从操作系统的提示符重新启动 shell。

现在，咱们应已经熟悉了启动和停止 shell，以及使用 shell 计算一些简单表达式。我们还看到了函数式编程语言和命令式编程语言根本区别之一。在函数式编程语言中，变量无法改变，但在命令式编程语言中，变量则可以改变。


## 进程、模组与编译

**Process, Modules and Compilation**


Erlang 程序建立在数个并行的进程之上。进程会运行定义在模组中的函数。模组是一些扩展名为 `.erl`  的文件，且必须经过编译才能运行。编译完某个模组后，我们就可以在 shell 中，或在操作系统环境下的命令行中，运行该模组中的函数。


在接下来的小节中，我们将了解如何在 shell 及操作系统命令行下，编译模组与运行函数。


### 在 shell 下编译和并运行 `"Hello World"`


请以以下内容，构造一个名为 `hello.erl` 的文件：


[`hello.erl`](http://media.pragprog.com/titles/jaerlang2/code/hello.erl)

```erlang
-module(hello).
-export([start/0]).

start() ->
    io:format("Hello world~n").
```

要编译和运行这个程序，我们要在咱们存储 `hello.erl` 的目录下启动 Erlang shell，并执行以下操作：


```shell
$ erl
Erlang/OTP 28 [erts-16.0.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Eshell V16.0.2 (press Ctrl+G to abort, type help(). for help)
1> c(hello).
{ok,hello}
2> hello:start().
Hello world
ok
3> halt().
$ 
```


其中命令 `c(hello)` 会编译文件 `hello.erl` 中的代码。`{ok, hello}` 表示编译成功。代码现在可以运行了。在第 2 行，我们执行了函数 `hello:start()`。在第 3 行，我们停止了这个 Erlang shell。

在 shell 下工作的优势在于，这种编译和运行程序的方式，在所有支持 Erlang 的平台上已知都能运行。而在操作系统的命令行下工作，则不一定在所有平台上都能正常运行。


### 在 Erlang shell 外部编译


使用之前的同样代码，我们可以在操作系统命令行下，编译并运行我们的代码，如下所示：


```shell
$ erlc hello.erl
$ erl -noshell -s hello start -s init stop
Hello world
```


其中 `erlc` 会从命令行调用 Erlang 的编译器。该编译器会编译 `hello.erl` 中的代码，并生成一个名为 `hello.beam` 的目标代码文件。

> **译注**：经测试，`erlc hello.erl` 命令的效果，与 Erlang shell 下 `c(hello).` 命令效果一致。


而其中的 `$ erl -noshell ...` 命令，会加载这个模组 `hello` 并执行函数 `hello:start()`。然后，他会执行表达式 `init:stop()`，这会终止这个 Erlang 会话。

在 Erlang shell 之外运行 Erlang 的编译器 (即 `erlc`)，是编译 Erlang 代码的首选方式。我们可在 Erlang shell 内编译模组，但我们首先必须启动 Erlang shell。而使用 `erlc` 的优势在于自动化。我们可以在 rakefile 或 makefile 中运行 `erlc`，并自动化构建过程。

在咱们开始学习 Erlang 时，建议使用 Erlang shell 处理所有事情；这样咱们就能熟悉编译及运行代码的细节。更高级的用户会希望自动编译，而减少 Erlang shell 的使用。


## 你好，并发


我们已经了解如何编译一个简单模组。但该怎么编写一个并发的程序呢？Erlang  中并发的基本单元是 *进程*。所谓进程，是个仅靠发送和接收消息，就能与其他进程通信的轻量级虚拟机。若咱们打算让某个进程做什么事情，咱们就要向其发送一条消息，然后等待答复。


我们将编写的第一个并发程序，是个文件服务器。要在两台机器间传输文件，我们就需要两个程序：运行在一台机器上的客户端，和在第二台机器上运行的服务器。要实现这个，我们将构造两个模组，分别称为 `afile_client` 和 `afile_server`。


### 文件服务器进程


文件服务器是在名为 `afile_server` 的模组中实现的。提醒一下，进程和模组就如同对象和类。进程的代码包含在某个模组中，而要创建出进程，我们就要调用原语 `spawn(...)`， 他会实际创建出进程。

[`afile_server.erl`](http://media.pragprog.com/titles/jaerlang2/code/afile_server.erl)


```erlang
-module(afile_server).
-export([start/1, loop/1]).

start(Dir) -> spawn(afile_server, loop, [Dir]).
loop(Dir) ->
    receive
        {Client, list_dir} ->
            Client ! {self(), file:list_dir(Dir)};
        {Client, {get_file, File}} ->
            Full = filename:join(Dir, File),
            Client ! {self(), file:read_file(Full)}
    end,
    loop(Dir).
```


这段代码有着非常简单的结构。若我们省略掉大部分细节，他看起来就像这样：


```erlang
loop(Dir) ->
    %% wait for a command
    receive
        Command ->
            ... do something ...
    end,
    loop(Dir).
```


这就是我们在 Erlang 中编写无限循环的方式。其中变量 `Dir`  包含着文件服务器当前的工作目录。在这个循环中，我们等待接收一条命令；在我们收到一条命令时，我们会遵从该命令，然后再次调用咱们自己，接收下一命令。

*好奇者请注意*：请不要担心我们做的最后一件事是调用自己；我们不会耗尽堆栈空间，stack space。Erlang  会对代码进行所谓的尾调用优化，tail-call optimization，这意味着该函数将在恒定空间内运行。这是 Erlang 中编写循环的标准方法。只需将调用自己，作为咱们最后做的事情即可。


要注意的另一点是，`loop` 是没有返回值的函数。在顺序编程语言中，我们必须非常小心地避免无限循环；我们只有一个控制线程，若这个线程陷入循环，我们就麻烦了。而在 Erlang 中，就不存在这类问题。服务器只是个在无限循环中，为请求提供服务的程序，他与我们要执行的任何其他任务，一起并行运行。

> *知识点*：

- stack space
- tail-call optimization


现在，让我们仔细看看那个接收语句；为提醒大家，他看起来是这样的：




[`afile_server.erl`](http://media.pragprog.com/titles/jaerlang2/code/afile_server.erl)



```erlang
    receive
        {Client, list_dir} ->
            Client ! {self(), file:list_dir(Dir)};
        {Client, {get_file, File}} ->
            Full = filename:join(Dir, File),
            Client ! {self(), file:read_file(Full)}
    end,
```

这段代码表示，在我们接收到消息 `{Client,list_dir}` 时，应回复一个文件列表；若我们接收到消息 `{Client,{get_file,File}}`，则应回复该文件。在收到消息时，作为模式匹配过程的一部分，变量 `Client` 会被绑定。

这段代码非常紧凑，因此很容易忽略其中的细节。关于这段代码，有三个要点值得注意。


- *回复对象*

所有收到的消息，都包含着变量  `Client`；这是发送请求进程的进程标识符，回复应发送给该进程。

若咱们打算回复某条消息，那么最好说明该回复要发往的对象。这就像在信件中写上咱们的姓名和地址一样；若咱们未说明信件来自于谁，那么咱们就永远不会收到回复。

- *`self()` 的使用*


由服务器发送的回复，包含了参数 `self()`（这里的 `self()` 是服务器的进程标识符）。该标识符被添加到消息中，以便客户端可以检查其收到的消息，是否来自该服务器，而不是其他进程。


- *用于选择消息的模式匹配*


这个接收语句内有两个 *模式*。我们可以这样写：


```erlang
receive
    Pattern1 ->
      Actions1;
    Pattern2 ->
      Actions2 ->
    ...
end
```


Erlang 编译器和运行时系统会在收到消息时，正确确定出如何运行相应的代码。我们不必编写任何的 `if-then-else` 或 `switch` 语句，来确定要做什么。这正是模式匹配的乐趣之一，他将为咱们节省大量的工作。


我们可在 shell 中编译和测试这段代码，如下所示：


```console
1> c(afile_server).
{ok,afile_server}
2> FileServer = afile_server:start(".").
<0.91.0>
3> FileServer ! {self(), list_dir}.
{<0.84.0>,list_dir}
4> receive X -> X end.
{<0.91.0>,
 {ok,[".afile_server.erl.swp","afile_server.beam",
      "afile_server.erl","hello.beam","hello.erl"]}}
```


我们来看看其中细节。


```console
1> c(afile_server).
{ok,afile_server}
```


我们编译了文件 `afile_server.erl` 中的 `afile_server` 模组。编译成功了，因此 “编译” 函数 `c` 的返回值为 `{ok, afile_server}`。


```console
2> FileServer = afile_server:start(".").
<0.91.0>
```

`afile_server:start(Dir)` 会调用 `spawn(afile_server,loop,[Dir])`。这会创建一个新的、计算函数 `afile_server:loop(Dir)` 的并行进程，并返回一个可用于与该进程通信的 *进程标识符*。

其中 `<0.91.0>` 便是这个文件服务器进程的进程标识符。他被显示为三个整数，中间用句点隔开，并包含在一对尖括号中。*请注意*：每次咱们运行该程序时，这个进程标识符都会变化。因此，不同会话中的像是 `<0.91.0>` 这样的 数字会有所不同。


```console
3> FileServer ! {self(), list_dir}.
{<0.84.0>,list_dir}
```

这会向文件服务器进程，发送一条 `{self(), list_dir}` 的消息。`Pid ！Message` 的返回值 *被定义* 为了 `Message`，因此 shell 会打印出 `{self(), list_dir}` 的值，即 `{<0.84.0>, list_dir}`。其中 `<0.84.0>` 是这个 Erlang shell 本身的进程标识符；他包含在消息中，以便文件服务器知道该回复谁。


```console
4> receive X -> X end.
{<0.91.0>,
 {ok,[".afile_server.erl.swp","afile_server.beam",
      "afile_server.erl","hello.beam","hello.erl"]}}
```

`receive X -> X end` 会接收由文件服务器发送的回复。他会返回元组 `{<0.91.0>, {ok, ...}`。该元组中第一个元素是 `<0.91.0>`，即文件服务器的进程标识符。第二个参数是函数 `file:list_dir(Dir)` 的返回值，该函数是在文件服务器进程的接收循环中求值的。


### 客户端代码


文件服务器是经由一个名为 `afile_client` 客户端模组访问的。该模组的主要目的，是隐藏底层通信协议的细节。客户端代码的用户，可通过调用在这个客户端模组中导出的 `ls` 和 `get_file` 函数传输文件。这赋予了我们在不改变客户端代码 API 细节下，改变底层协议的自由。


[`afile_client.erl`](http://media.pragprog.com/titles/jaerlang2/code/afile_client.erl)

```erlang
-module(afile_client).
-export([ls/1, get_file/2]).

ls(Server) ->
    Server ! {self(), list_dir},
    receive
        {Server, FileList} ->
            FileList
    end.

get_file(Server, File) ->
    Server ! {self(), {get_file, File}},
    receive
        {Server, Content} ->
            Content
    end.
```

若咱们比较一下 `afile_client` 和 `afile_server` 的代码，就会发现两者之间有种美丽的对称。期间在客户端中有个 `Server ！...`，服务器中就会有个 `receive` 模式，反之亦然。


```erlang
receive
    {Client, Pattern} ->
        ...
end
```


现在，我们将重启 shell，重新编译一切，并展示客户端和服务器一起工作。



```console
1> c(afile_server).
{ok,afile_server}
2> c(afile_client).
{ok,afile_client}
3> FileServer = afile_server:start(".").
<0.96.0>
4> afile_client:get_file(FileServer, "missing").
{error,enoent}
5> afile_client:get_file(FileServer, "afile_server.erl").
{ok,<<"-module(afile_server).\n-export([start/1, loop/1]).\n\nstart(Dir) -> spawn(afile_server, loop, [Dir]).\nloop(Dir"...>>}
```

我们在 shell 中运行的代码，与之前代码的唯一区别是，我们抽象出了接口例程，并将其放入了一个单独模组中。我们隐藏了客户端和服务器之间消息传递的细节，因为其他程序对这些细节不感兴趣。


咱们目前看到的，是个完整文件服务器的基础，但他还没有完成。与启动和停止服务器、连接到套接字等相关的细节还有很多，这里就不一一介绍了。


在 Erlang 的视角，咱们如何启动与停止服务器、连接到套接字、从错误中恢复等等，都是无关紧要的细节。问题的 *本质* 在于创建并行进程，以及发送及接收消息。


> *知识点*：


- the *essence* of the problem, 问题的本质



在 Erlang 中，我们使用进程,架构出问题的解决方案。对进程结构的思考（换句话说，哪些进程相互了解），与对进程间所发送消息及消息所包含信息的思考，是我们思维方式及编程方式的核心。


### 改进这个文件服务器


我们开发的这个文件服务器，涉及运行在同一台机器上的两个通信进程，并展示了编写并发程序所需的几个构件。在真正的服务器中，客户端和服务器将运行在不同机器上，因此我们必须以某种方式，安排好进程间消息传递不仅可在同一 Erlang 节点中的进程间进行，还可以在物理上分离的机器上 Erlang 进程间进行。

在 [第 17 章 “使用套接字编程”](../part-iv/Ch17-programming_with_sockets.md) 中，我们将了解如何将 TCP 传输层用于进程通信，而 [“文件服务器回顾”](../part-iii/Ch14-distributed_programming.md#文件服务器回顾) 中，我们将了解如何在分布式 Erlang 中，直接实现这个文件服务器。


在本章中，我们了解了如何在 shell 中执行一些简单的操作、编译某个模组，以及使用三个原语：`spawn`、`send` 和 `receive`，创建出有着两个进程的一个简单并发程序。


本书第一部分到此结束。在第二部分中，我们将详细介绍顺序编程，并在 [第 12 章 “并发编程”](../part-iii/Ch12-concurrent_programming.md)  中，我们将回到并发编程。在下一章中，我们将通过详细关注 shell、模式匹配及原生 Erlang 数据类型，开始学习顺序编程。



## 练习


现在也许是检查咱们对迄今为止学习内容理解的好时机。


1. 请启动并停止 Erlang shell;
2. 请在 Erlang shell 中键入几条命令。切记要以句点空格，结束这些命令；
3. 请对 `hello.erl` 做一些小修改。在 shell 中编译并运行他们。在出错时，请从 Erlang shell 中止并重新启动 shell；
4. 请运行哪个文件客户端及服务器代码。添加一个名为 `put_file` 的命令。需要添加哪些消息呢？请了解如何查阅手册页面。请查阅 `file` 模组的手册页面；
