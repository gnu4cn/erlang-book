# Erlang 中代码如何被加载的


加载代码到 Erlang 的默认机制，是使用一种 “按需” 代码加载的形式。当代码首次被调用，并发现该代码找不到时，那么该代码就会被加载。

这便是所发生的情况。假设函数 `my_mod:myfunc(Arg1,Arg2,...ArgN)` 被调用了，而这个模组的代码尚未被加载。系统会自动将这个调用，转换为下面的调用：

```erlang
error_module:undefined_function(mymod, myfunc, [Arg1, Arg2, ..., ArgN])
```


`undefined_function` 是像下面这样的某种代码：

```erlang
undefined_function(Mod, Func, ArgList) ->
    case code:load_module(Mod) of
        {ok, Bin} ->
            erlang:load_module(Mod, Bin),
            apply(Mod, Func, ArgList);
        {error, _} ->
            ...
    end.
```

这个未定义函数处理器，会将找到模组代码的任务，委托给代码处理器。当代码处理其能找到该代码时，他会加载该模组，然后调用 `apply(Mod, Func, ArgList)`，而系统会继续运行，就像这个陷阱没发生一样。

SEE 中完成这一目的的代码，就遵循了这种模式：


```erlang
{{#include ../../../projects/appendix/src/error_handler.erl:}}
```


*注意*：Erlang 系统中的错误处理程序，明显不同于这里的代码。相比这里的简单代码处理器，他完成了多得多的事情；他必须跟踪模组版本，和其他一些使生活变得复杂的事情。

一旦这个错误处理器（定制 `error_handler` 模组）已由引导脚本加载，自动加载随后将使用 SEE 的代码处理机制，而不是 OTP 系统工作（`/usr/lib/erlang/lib/kernel-10.3.2` 下的 `error_handler` 模组）。


# 练习

1. SEE 提供了自动加载。但我们可编写一个 *更简单的* 不带自动加载的执行环境。请移除自动加载的代码；

2. 对于那些完全独立的应用，咱们甚至不需要 SEE。请编写一个在标准输出上写下 "Hello world" 并终止的最小程序；

3. 请编写一个将标准输入，拷贝到标准输出（类似于 `see_test3.erl`），但启动时不使用 SEE 的最小化 `cat` 程序。测量拷贝几个大文件所用时间。请将这个程序，与一些流行的脚本语言进行比较。



