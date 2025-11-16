# Erlang 如何启动

在 Erlang 启动时，他会读取一个引导文件，并执行他在这个引导文件找到的那些命令。8 个 Erlang 模组会被预加载。这些属于已被编译为 C，且已被链接到 Erlang 虚拟机中的 Erlang 模组。这 8 个模组负责引导系统。他们包括了读取并执行引导文件中命令的 `init`，以及知道如何将代码加载到系统中的 `erl_prim_loader`。


引导文件包含着一个经由调用 `term_to_binary(Script)` 创建的二进制值，其中 `Script` 是包含着引导脚本的一个元组。


我们将构造一个名为 `see.boot` 的新引导文件，和一个名为 `see`，会使用这个新引导文件启动某个程序的脚本。这个引导文件将加载少量模组，就包括包含我们的定制执行环境的 `see.erl`。引导文件和脚本，是经由执行 `make_scripts/0` 创建的。


```erlang
{{#include ../../../projects/appendix/src/see.erl:285:314}}
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


