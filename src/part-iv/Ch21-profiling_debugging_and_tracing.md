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


### 转储到文件
