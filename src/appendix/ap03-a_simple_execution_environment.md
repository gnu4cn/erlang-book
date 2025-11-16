# 一种简单执行环境

在这一附录中，我们将构建一种运行 Erlang 程序的简单执行环境，a simple execution environment, SEE。这些代码放在一个附录中，而不是本书正文中，是因为其在性质上有很大不同。本书中的所有代码，都是意图在标准 Erlang/OTP 发行版中运行，而这些代码则有意最小化使用 Erlang 的库代码，而尝试仅使用 Erlang 的那些元语。

当第一次接触 Erlang 时，往往会分不清对于哪些东西属于这门语言，哪些属于运行环境。OTP 提供了一个丰富环境，类似于为运行一些长寿命分布式 Erlang 应用的一个操作系统。而由 Erlang（语言）和 OTP（环境）究竟分别提供了哪些功能，则并不清楚。


SEE 提供了 “更接近裸机” 的环境，给到了 Erlang 所提供功能，与 OTP 所提供功能间更明显的区分。SEE 提供的所有功能，都包含在一个模组中。OTP 会以加载 60 多个模组启动，这些东西的工作原理虽然并不立即可见，但当咱们知道从哪里入手时，这些都不是特别复杂。起点是那个引导文件。从查看引导文件开始，然后阅读 `init.erl` 中的代码，一切就会水落石出。


SEE 环境可用于编写脚本，因为他启动非常快，或者用于嵌入式编程，因为他非常小。要达成此目标，咱们将了解 Erlang 如何启动，以及代码自动加载系统工作原理。


当咱们启动一个 “标准” Erlang 系统时（使用 shell 命令 `erl`），67 个模组会被加载，25 个进程会被启动，然后咱们的程序才得以运行。这一过程耗时大约一秒钟。当我们打算执行的程序，不需要由标准系统提供的全部这些时，我们便可将这个时间，缩短到数十毫秒。


弄清这 67 个模组与流程的到底做了些什么，可能让人望而生畏。不过，有一条不同的捷径，可让咱们豁然开朗。SEE 将系统简化为只需研究一个模组，就能了解代码如何被加载到系统中，以及 I/O 服务怎样被提供。SEE 在一个模组中，提供了自动加载、通用服务器进程及错误处理。


在开始介绍 SEE 前，我们将收集一些我们稍后用于引用的，有关 OTP 系统的统计数据。

```erlang
$ erl
1> length([I || {I,X} <- code:all_loaded(), X =/= preloaded]).
93
2> length(processes()).
45
3> length(registered()).
28
```

> **译注**：以下是在加上一些常用命令行参数后的结果。
>
> ```erlang
> $
> $ erl -boot start_sasl -config elog4 -smp +S 12 -pa _build/default/lib/math_server/ebin
> 1> length([I || {I, X} <- code:all_loaded(), X =/= preloaded]).
> 98
> 2> length(processes()).
> 52
> 3> length(registered()).
> 33
> ```


`code:all_loaded()` 会返回当前加载到系统中所有模组的列表，`processions()` 是个会返回系统已知的所有进程列表，而 `registered()` 则会返回所有注册进程的列表。

因此，仅启动系统就会加载 67 个模组，启动 25 个进程，其中 16 个是注册进程。我们来看看咱们能否降低这个数字。


## Erlang 如何启动

在 Erlang 启动时，他会读取一个引导文件，并执行他在这个引导文件找到的那些命令。8 个 Erlang 模组会被预加载。这些属于已被编译为 C，且已被链接到 Erlang 虚拟机中的 Erlang 模组。这 8 个模组负责引导系统。他们包括了读取并执行引导文件中命令的 `init`，以及知道如何将代码加载到系统中的 `erl_prim_loader`。


引导文件包含着一个经由调用 `term_to_binary(Script)` 创建的二进制值，其中 `Script` 是包含着引导脚本的一个元组。


我们将构造一个名为 `see.boot` 的新引导文件，和一个名为 `see`，会使用这个新引导文件启动某个程序的脚本。这个引导文件将加载少量模组，就包括包含我们的定制执行环境的 `see.erl`。引导文件和脚本，是经由执行 `make_scripts/0` 创建的。


```erlang
{{#include ../../projects/appendix/src/see.erl:285:314}}
```

> **译注**：为适应 Rebar3 构建工具，译者做了一些修改。
>
> - 修改了 `path` 元组中的 `Cwd`；
>
> - 将 `kernel` 与 `stdlib` 两个路径，添加到了 `path` 元组的列表中。否则在运行 `./see see_test1` 时，将报出如下错误。
>
> ```console
> ./see see_test1
> Runtime terminating during boot ({load_failed,[error_handler,lists]})
>
> Crash dump is being written to: erl_crash.dump...done
> ```



其中第 3 行包含了一个标识该脚本的字符串。随后是个会被 `init.erl` 执行的命令列表。第一条命令在第 4 行处，其中的元组 `{preloaded,[Mods]}` 告诉系统，哪些模组已被预加载。下一条是个 `{progress, Atom}` 命令。处于调试目的，一些 `progress` 元组被包含。当于 Erlang 启动时，在命令行上 `-init_debug` 命令行开关被包含时，这些元组就会被打印出来。


元组 `{path, [Dirs]}` （第 6 行处）建立了一个代码加载器路径，而 `{primLoad, [Mods]}` 表示要加载这个模组列表中的那些模组。因此，第 7 行至第 11 行，告诉系统，要使用给定的代码路径，加载三个模组（`lists`、`error_handler` 及 `see`）。


当我们加载完所有代码后，我们就到了 `{kernel_load_completed}` 这个命令。这表示 “我们可以开始工作了”，我们会将控制权，移交给用户代码。在内核加载完成命令后（而不是前），我们就能用户函数了。这是经由调用 `apply` 完成的。最后，在第 15 行处，我们写下了 `{apply,{see,main,[]}}`，这会调用 `apply(see,main,[])`。


`make_scripts/0` 必须在 Erlang 的开发环境中执行，因为他调用了 `code`、`filename` 及 `file` 三个模组中的函数，这三个模组对 SEE 程序不可用。


现在我们将构建这个引导文件与启动脚本。


```console
$ make
rebar3 compile
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling math_server
===> "/home/hector/erlang-book/projects/appendix/_build/default/lib/math_server/ebin/math_server.app" is missing description entry
$ erl -boot start_sasl -config elog4 -smp +S 12 -pa _build/default/lib/math_server/ebin -s see make_scripts
Erlang/OTP 28 [erts-16.0.3] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [jit:ns]

Script:{script,{"see","1.0"},
               [{preLoaded,[zlib,prim_file,prim_zip,prim_inet,erlang,
                            otp_ring0,init,erl_prim_loader]},
                {progress,preloaded},
                {path,["/home/hector/erlang-book/projects/appendix/_build/default/lib/math_server/ebin",
                       "/usr/lib/erlang/lib/stdlib-7.0.3/ebin",
                       "/usr/lib/erlang/lib/kernel-10.3.2/ebin"]},
                {primLoad,[lists,error_handler,see]},
                {kernel_load_completed},
                {progress,kernel_load_completed},
                {progress,started},
                {apply,{see,main,[]}}]}
```

## 在 SEE 下运行一些测试程序


一旦我们已构建这个引导脚本，我们就可以将我们的注意力，转向那些我们将要在 SEE 下运行的程序。我们的所有示例，都是一些必须导出函数 `main()` 的普通 Erlang 模组。


全部程序中最简单，便是 `see_test1`。


```erlang
{{#include ../../projects/appendix/src/see_test1.erl}}
```


要运行这个程序，我们首先要使用 Erlang 开发环境下的标准编译器，编译 `see_test1`。一旦我们编译了这个程序，我们就可以运行咱们的程序。


```erlang
$ ./see see_test1
HELLO WORLD
4 modules loaded
```

> **译注**：经测试，我们可把这个 `see` 可执行二进制文件放在任何地方，都可以此方式运行命令。因为其中已经包含了预先设置的那些路径信息。


我们可如下对这个程序计时：


```console
$ time ./see see_test1
HELLO WORLD
4 modules loaded
./see see_test1  0.04s user 0.03s system 180% cpu 0.039 total
```
