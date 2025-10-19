# 性能分析、调试与栈追踪

在这一章中，我们将介绍一些咱们可用来优化咱们程序、找出 bug 及避免错误的技术。

- *性能分析*

    我们会使用性能分析，进行性能调优，找出咱们程序中的热点所在。我（作者）认为要猜测出程序的瓶颈所在几乎是不可能的。最好的方法是先编写出咱们的程序，然后确认他们正确无误，并最后 *测量*，以找出时间的用在了哪里。当然，当程序足够快时，我们省去最后一步；

- *覆盖率分析*

    我们会运用覆盖率分析，计算咱们程序中每行代码被执行的次数。被执行次数为零的代码行，可能表示某个错误，咱们可将其删除的死代码。找出那些被执行很多次的行，可能会帮助咱们优化咱们的程序；

- *交叉引用*

    我们可使用交叉引用，找出我们是否有任何缺失代码，并找出谁调用了什么。当我们试图调用某个不存在的函数时，那么交叉引用分析就将发现这点。这主要对那些有着数十个模组的大型程序，非常有用；

- *编译器诊断*

    这个小节会介绍编译器诊断；

- *运行时错误消息*

    运行时系统会产生许多不同的错误消息。我们将解释一些最常见错误消息表示什么；

- *调试技术*

    调试被分作了两个小节。首先，我们将学习一些简单技术；其次，我们将学习 Erlang 的调试器；

- *追踪*

    使用进程追踪，我们可检查运行系统的行为。Erlang 有着我们可以用来远程观察任何进程行为的先进追踪设施。我们可以观察进程间的消息传递，以及找出某个进程中正在执行哪些函数，以及进程何时被调度等等；

- *测试框架*

    有数种用于 Erlang 程序自动测试的测试框架。我们将看到这些框架的一个快速概览，并了解获取他们的方式。


## Erlang 代码的性能分析工具

标准 Erlang 发行版附带了三个性能分析工具。

- `cprof` 会计算每个函数被调用的次数。这是个轻量级的性能分析器。在某个现场系统上运行该工具，会增加 5% 到 10% 的系统负载；

- `fprof` 会显示调用及被调用函数的时间。输出是到某个文件。这适合实验室或模拟系统中的大型系统性能。他会给系统增加显著负载；

- `eprof` 会测量 Erlang 程序中时间的使用情况。他是 `fprof` 的前身，适合小规模的性能分析。


下面是咱们运行 `cprof` 的方式；我们将用他对 [17.6 小节 *SHOUTcast 服务器*](Ch17-programming_with_sockets.md#shoutcast-服务器) 中，咱们编写的代码进行性能分析：


```erlang
1> cprof:start().           %% start the profiler
9655
2> shout:start().           %% run the application
<0.88.0>
3> cprof:pause().           %% pause the profiler
9940
4> cprof:analyse(shout).    %% analyse function calls
{shout,8,
       [{{shout,start_parallel_server,1},1},
        {{shout,start,0},1},
        {{shout,songs_loop,1},1},
        {{shout,songs,0},1},
        {{shout,par_connect,2},1},
        {{shout,'-start_parallel_server/1-fun-1-',2},1},
        {{shout,'-start_parallel_server/1-fun-0-',0},1},
        {{shout,'-start/0-fun-0-',0},1}]}
5> cprof:stop().            %% stop the profiler
9940
```


此外，`cprof:analyse()` 会分析已收集统计信息的所有模组。

`cprof` 的更多详细信息，[可在线上找到](https://www.erlang.org/doc/apps/tools/cprof.html)。

[`fprof`](https://www.erlang.org/doc/apps/tools/fprof.html) 和 [`eprof`](https://www.erlang.org/doc/apps/tools/eprof.html) 与 `cprof` 大致相似。详情请查阅线上文档。


## 测试代码覆盖率

当我们在测试咱们的代码时，除要看出哪些代码行执行较多外，看出哪些代码行从未执行通常也很不错。从未执行的代码行，是错误的潜在来源，因此找出这些代码行的位置，就相当棒。要完成这点，我们就要用到程序覆盖率分析器。


下面是个示例：


```erlang
1> cover:start().           %% start the coverage analyser
{ok,<0.87.0>}
2> cover:compile(shout).    %% compile shout.erl for coverage
{ok,shout}
3> shout:start().           %% run the program
<0.96.0>
Playing:<<"title: track018 performer: .. ">>
4> %% let the program run for a bit
4> cover:analyse_to_file(shout).    %% analyse the results
{ok,"shout.COVER.out"}              %% this is the results file
```

此操作的结果，被打印到一个文件。


```console
...
        |  send_file(S, Header, OffSet, Stop, Socket, SoFar) ->
        |      %% OffSet = first byte to play
        |      %% Stop   = The last byte we can play
     0..|      Need = ?CHUNKSIZE - byte_size(SoFar),
     0..|      Last = OffSet + Need,
     0..|      if
        |  	Last >= Stop ->
        |  	    %% not enough data so read as much as possible and return
     0..|  	    Max = Stop - OffSet,
     0..|  	    {ok, Bin} = file:pread(S, OffSet, Max),
     0..|  	    list_to_binary([SoFar, Bin]);
        |  	true ->
     0..|  	    {ok, Bin} = file:pread(S, OffSet, Need),
     0..|  	    write_data(Socket, SoFar, Bin, Header),
     0..|  	    send_file(S, bump(Header),
        |  		      OffSet + Need,  Stop, Socket, <<>>)
        |      end.
...
```

在该文件的左侧，我们会看到每条语句已被执行的次数。标有零的那些行尤其有趣。因为这些代码从未被执行过，所以我们不能说我们的程序是正确的。


> **所有测试方法中最好的是？**
>
> 对咱们代码进行覆盖率分析，回答了这个问题：哪些代码行从未被执行？在我们清楚了哪些代码行从未被执行后，我们就可以设计强制这些代码行执行的测试用例。
>
> 这样做是找出咱们程序中一些意想不到及隐蔽 bugs 的可靠方法。每个从未执行过的代码行，都可能包含着一个错误。强制这些代码行执行，是我（作者）所知测试程序的最佳方法。
>
> 我（作者）就曾对最初的 Erlang JAM <sup>a</sup> 编译器，完成过这种测试。我想我们在两年中，获得了三份 bug 报告。在此之后，就再也没有报告过 bug 了。
>
> <sup>a</sup>：Joe's Abstract Machine, 首个 Erlang 编译器。

设计一些造成全部覆盖率计数都大于零的测试用例，是一种系统地找出咱们程序中隐藏错误的宝贵方法。


## 生成交叉引用


在我们开发某个程序时，对咱们的代码运行偶尔的交叉引用检查是个好主意。当有遗漏的函数时，咱们就将在运行咱们的程序前，而不是之后发现他们。


使用 `xref` 模组，我们便可生成交叉引用。只有当咱们的代码在 `debug_info` 开关设置下编译时，`xref` 才工作。

我（作者）无法向咱们展示 `xref` 对本书附带代码的输出，因为这些代码的开发已经完成，且没有任何遗漏函数。


作为代替，我（作者）将向咱们展示，当我（作者）对我的一个业余项目中的代码，运行交叉引用检查时发生的情况。


`vsg` 是个我（作者）或许有一天会发布的简单图形程序。我们将对我（作者）正在其下开发这个程序的 `vsg` 目录下的代码进行分析。


```console
$ cd /home/joe/2007/vsg-1.6
$ rm *.beam
$ erlc +debug_info *.erl
$ erl
1> xref:d('.').
[{deprecated,[]},
{undefined,[{{new,win1,0},{wish_manager,on_destroy,2}},
{{vsg,alpha_tag,0},{wish_manager,new_index,0}},
{{vsg,call,1},{wish,cmd,1}},
{{vsg,cast,1},{wish,cast,1}},
{{vsg,mkWindow,7},{wish,start,0}},
{{vsg,new_tag,0},{wish_manager,new_index,0}},
{{vsg,new_win_name,0},{wish_manager,new_index,0}},
{{vsg,on_click,2},{wish_manager,bind_event,2}},
{{vsg,on_move,2},{wish_manager,bind_event,2}},
{{vsg,on_move,2},{wish_manager,bind_tag,2}},
{{vsg,on_move,2},{wish_manager,new_index,0}}]},
{unused,[{vsg,new_tag,0},
{vsg_indicator_box,theValue,1},
{vsg_indicator_box,theValue,1}]}]
```

其中 `xref:d('.')` 对当前目录下，所有以那个调试标记（`+debug_info`）编译的代码，执行了一次交叉引用分析。他生成了废弃、未定义及未使用函数的一些列表。

与大多数工具一样，`xref` 有着大量选项，因此，当咱们打算使用这个程序所拥有的一些更为强大特性时，阅读 [手册](https://www.erlang.org/doc/apps/tools/xref_chapter.html) 是必要的。


## 编译器诊断

在我们编译某个程序时，当我们的源代码语法不正确时，编译器会提供给我们有用的错误消息。大多数错误消息是不言自明的：当我们漏掉了个括号、逗号或关键字时，编译器将给出一条带有问题语句文件名及行号的错误消息。下面是我们可能看到的一些错误。


### 头部不匹配

当组成某个函数定义的子句，没有同样名字与元数时，我们将得到以下报错：


```erlang
foo(1,2) ->
    a;
foo(2,3,1) ->
    b.
```

```erlang
1> c(bad).
bad.erl:5:1: head mismatch: function foo with arities 2 and 3 is regarded as two distinct functions. Is the number of arguments incorrect or is the semicolon in foo/2 unwanted?
%    5| foo(2,3,1) ->
%     | ^

error
```


### 未绑定的变量


下面是一些包含着非绑定变量的代码：


```erlang
foo(A, B) ->
    bar(A, dothis(X), B),
    baz(Y, X).
```

```erlang
1> c(bad).
bad.erl:4:5: function bar/3 undefined
%    4|     bar(A, dothis(X), B),
%     |     ^

bad.erl:4:12: function dothis/1 undefined
%    4|     bar(A, dothis(X), B),
%     |            ^

bad.erl:4:19: variable 'X' is unbound
%    4|     bar(A, dothis(X), B),
%     |                   ^

bad.erl:5:5: function baz/2 undefined
%    5|     baz(Y, X).
%     |     ^

bad.erl:5:9: variable 'Y' is unbound
%    5|     baz(Y, X).
%     |         ^

bad.erl:3:1: Warning: function foo/2 is unused
%    3| foo(A, B) ->
%     | ^

error
```

这意味着在第 2 行中，变量 `X` 没有值。实际上，错误并不在第 2 行上，但是在第 2 行处检测到的，该行正是那个未绑定变量 `X` 的第一次出现（在第 3 行上 `X` 也被使用了，但编译器只报告了错误出现的第一行）。


### 未终止字符串


当我们忘记了某个字符串或原子中的最后引号时，我们将得到以下错误消息：


```erlang
unterminated string starting with "..."
```


有时，找到缺失引号可能非常棘手。当咱们收到这条消息，又确实无法看到缺失引号在何处时，那么较要尝试把一个引号，放在咱们认为问题可能所在的地方，然后重新编译该程序。这样做可能产生出将帮助咱们找出该错误的一个更精确诊断。


### 转储到文件
