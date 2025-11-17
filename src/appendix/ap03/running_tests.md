# 在 SEE 下运行一些测试程序


一旦我们已构建这个引导脚本，我们就可以将我们的注意力，转向那些我们将要在 SEE 下运行的程序。我们的所有示例，都是一些必须导出函数 `main()` 的普通 Erlang 模组。


全部程序中最简单，便是 `see_test1`。


```erlang
{{#include ../../../projects/appendix/src/see_test1.erl}}
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

因此，加载四个模组（`lists`、`error_handler`、`see` 及 `see_test1`）并运行这个程序，花了 0.039 秒。


为了比较，我们可对使用 OTP 系统的对等 Erlang 程序计时。


```erlang
{{#include ../../../projects/appendix/src/otp_test1.erl}}
```


```console
$ make
$ time erl -boot start_sasl -config elog4 -smp +S 12 -pa _build/default/lib/math_server/ebin -s otp_test1 main -s init stop
...
HELLO WORLD
erl -boot start_sasl -config elog4 -smp +S 12 -pa  -s otp_test1 main -s init   0.32s user 0.12s system 38% cpu 1.145 total
```


在启动和运行我们的小程序方面，SEE 比 OTP 快 29 倍（1.145/0.039 ~= 29.36）。要知道，快速启动 OTP，从来不是个设计目标。一旦启动，OTP 应用预期就会运行 *永久*，因此，当应用随后会永远运行时，那么在启动时间上节省几毫秒，就无关紧要了。


下面是另外一些简单程序。`see_test2` 会测试自动加载是否工作。


```erlang
{{#include ../../../projects/appendix/src/see_test2.erl}}
```


其中：

```erlang
{{#include ../../../projects/appendix/src/my_code.erl}}
```


因此：


```console
$ ./see see_test2
{about_to_call,my_code}
{new_error_handler,undefined_function,my_code,double,[1000]}
{error_handler,calling,my_code,double,[1000]}
see_test2 worked
```


> **译注**：译者在运行此命令时，并未得到原著的结果。而是得到以下结果。
>
> ```console
> $ ./see see_test2
> {about_to_call,my_code}
> ({stopping_system,{undef,[{my_code,double,[1000],[]},{see_test2,main,0,[{fniole ,l"o/ghgoemre /phreecsteonr/te)r luannegx-pbeocotwe/dp rloojgegcetrs /maepspseangdei:x /{srlco/gs,eeer_rtoers,t"2E.rerrolr" }i,{nl ipnreo,c6e}s]s} ]~}p}
> ith exit value:~n~p~n",[<0.21.0>,{undef,[{my_code,double,[1000],[]},{see_test2,main,0,[{file,"/home/hector/erlang-book/projects/appendix/src/see_test2.erl"},{line,6}]}]}],#{error_logger=>#{emulator=>true,tag=>error},pid=><0.21.0>,time=>1763342965855668,gl=><0.0.0>}}
> ```
>
> 并每次运行会有不同输出。这显示在 `see_test2.erl` 中对 `my_code.erl` 模组的引用失败。


~~我们可以看到，`my_code` 模组已正确自动加载~~。其他输出为来自自定义代码加载器的调试打印输出。这是由 `error_handler` 模组生成的，本章稍后将讨论。


下面的 `see_test3` 是个将在标准输入中看到的所有内容，都拷贝到标准输出中的 Erlang 程序。(这就是如何编写 Unix 管道进程。）


```erlang
{{#include ../../../projects/appendix/src/see_test3.erl}}
```


下面是个示例：


```console
$ cat src/see.erl | cksum
4160209670 8424
$ cat src/see.erl | ./see see_test3 see.erl | cksum
4160209670 8424
```

`see_test4`  会测试错误处理。


```erlang
{{#include ../../../projects/appendix/src/see_test4.erl}}
```


下面是个示例：


```console
$ see see_test4
I will crash now
{stopping_system,{{badmatch,2},
[{see_test4,main,0,[{file,"see_test4.erl"},{line,6}]}]}}
```

> **译注**：译者运行此命令的输出为：
>
> ```console
> $ ./see see_test4
> I will crash now
> {stopping_system,{{badmatch,2},[{see(_ntoe slto4g,gmeari np,r0e,s[e{nfti)l eu,n"e/xhpoemcet/ehde cltoogrg/eerrl amnegs-sbaogoek:/~p{rloojge,cetrsr/oarp,p"eEnrdrioxr/ sirnc /psereo_cteessst 4~.pe rwli"t}h, {elxiinte ,v6a}l]u}e]:}~}n
> p~n",[<0.21.0>,{{badmatch,2},[{see_test4,main,0,[{file,"/home/hector/erlang-book/projects/appendix/src/see_test4.erl"},{line,6}]}]}],#{error_logger=>#{emulator=>true,tag=>error},pid=><0.21.0>,time=>1763344098538686,gl=><0.0.0>}}
> ```
>
> 结合上面的异常输出，初步断定是因为 `see.erl` 中引入的 `error_hander` 模组未能正常工作，从而导致译者得到的结果与原著中的结果不同。随后将继续排查此问题。



