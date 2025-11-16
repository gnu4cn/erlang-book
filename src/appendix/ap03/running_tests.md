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
