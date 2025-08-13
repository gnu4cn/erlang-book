# 入门

## 学习 Erlang 需要多长时间？

这要看情况。(你还指望别的吗？）

如果有命令式语言, an imperative language <sup>1</sup>, 背景（如 C、Python、Java、C++、Pascal、PERL 等），大多数人需要大约一周时间，才能写出一些较复杂的程序，大约一个月才能真正感到自如，几个月后才会觉得自己已准备好独自承担一些大型任务。如果身边有人知道如何使用 Erlang，并能手把手地教，会有很大帮助。

如果你的背景包括另一种声明式语言（Lisp、Prolog、Haskell、Scheme 等），咱们就可以直接编写 Erlang 代码，不过掌握如何利用容错与并发，则还需要一段时间。

> **译注**：
>
> <sup>1</sup>，
>
> 参考：[What is the difference between declarative and imperative paradigm in programming?](https://stackoverflow.com/questions/1784664/what-is-the-difference-between-declarative-and-imperative-paradigm-in-programmin#:~:text=Declarative%20programming%20is%20when%20you%20say%20what%20you,i%20in%20range%2820%29%3A%20if%20i%20%3C%205%3A%20small_nums.append%28i%29)


## 如何学习 Erlang？

许多人通过书籍或在线教程自学 Erlang，通常还要在 [erlang-questions 邮件列表](http://www.erlang.org/static/doc/mailinglist.html) 或 irc.freenode.net 上的 #erlang IRC 频道上发帖。

[《Learn You Some Erlang》](http://learnyousomeerlang.com/) 是本简单易懂的教程，只需一两天就能读完。另外，还有一个 [简易的在线教程](http://www.erlang.org/course/course.html)。

任何一本 [Erlang 书籍](https://www.erlang.org/faq/obtaining#books) 也可用作教程。

Erlang 发行版包括一个循序渐进的入门指南。这也是 [在线的](http://www.erlang.org/doc/getting_started/users_guide.html)。


Ericsson 为 Ericsson 员工提供了培训课程。


[erlang-solutions.com](http://www.erlang-solutions.com/) 针对企业开设了培训课程（主要在伦敦和斯德哥尔摩，也在美国、亚洲和澳大利亚）。这些课程通常与一年一度的 Erlang 用户大会同时举办。

许多 [大学](https://www.erlang.org/faq/introduction#universities) 都开设了部分或全部有关 Erlang 的课程。关于函数式编程的课程，对于打下坚实基础也很有用，可以让咱们轻松地自学 Erlang。


## "hello world" 看起来是什么样的？

下面是 "hello world "的一种写法：


```erl
	-module(hello).
	-export([hello_world/0]).

	hello_world() -> io:fwrite("hello, world\n").
```

要编译这段代码，请将其保存到名为 `hello.erl` 的文件中，然后在 erlang shell 中编译。不要忘记在每条命令的末尾加上句号（美式英语中的 "句号"），如下所示：


```shell
$ werl
Erlang/OTP 28 [erts-16.0.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Eshell V16.0.2 (press Ctrl+G to abort, type help(). for help)
1> c(hello).
{ok,hello}
	
```


(在 unix 系统上，通过在命令行输入 `erl` 即可启动 Erlang shell。在 Windows 系统中，打开命令提示符窗口键入 `werl`，或在程序菜单中找到 Erlang 图标）。在 Erlang shell 中运行这个程序：


```shell
2> hello:hello_world().
hello, world
ok
```


## 如何退出 Erlang shell？


要干净利落地关闭某个系统，请使用 `init:stop().`。

一些快速方法包括执行 `halt()`，或 `Control+\`。

`Control+C` 与 `Control+G`，则可以让咱们访问到菜单。


## 为什么 Erlang 会在我的程序输出后打印 "ok"？

Erlang shell 的工作原理是读取 Erlang 表达式，对其进行求值，打印结果并循环到另一表达式，即他是个 REPL shell<sup>2</sup>。

`io:fwrite()` 函数做了两件事。他打印出 "hello world"，并返回值 `ok`。

因此，当咱们在 shell 中执行 `io:fwrite("hello, world\n")` 时，`fwrite` 函数会打印第一行，同时 shell 会打印返回值 `ok`。[不使用 shell 运行 Erlang ](https://www.erlang.org/faq/how_do_i#noshell) 可避免这种情况。
