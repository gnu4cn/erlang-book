# 顺序 Erlang 的其余部分


对于顺序 Erlang，其余的就是一些咱们必须知道，但又不适合其他主题的零碎知识。这些主题没有特定逻辑顺序，所以他们只按字母顺序呈现，以便参考。涵盖的主题如下：


- [*内建函数 `apply`*](#apply)
- [*算术表达式*](#算术表达式)
- [*元数*](#元数)
- [*属性*](#属性)
- [*块表达式*](#块表达式)
- [*布尔值*](#布尔值)
- [*布尔表达式*](#布尔表达式)
- [*字符集*](#字符集)
- [*注释*](#注释)
- [*动态代码加载*](#动态代码加载)
- [*Erlang 的预处理器*](#erlang-的预处理器)
- [*转义序列*](#转义序列)
- [*表达式与表达式序列*](#表达式与表达式序列)
- [*函数的引用*](#函数的引用)
- [*包含文件*](#包含文件)
- [*列表的加法与减法运算符*](#列表的加法与减法运算符)
- [*宏*](#宏)
- [*模式中的匹配运算符*](#模式中的匹配运算符)
- [*数字*](#数字)
- [*运算符优级*](#运算符优先级)
- [*线程字典*](#线程字典)
- [*引用*](#引用)
- [*短路的布尔表达式*](#短路的布尔表达式)
- [*项的比较*](#项的比较)
- [*元组的模组*](#元组的模组)
- [*下划线变量*](#下划线变量)


## 内建函数 `apply`


`apply(Mod, Func, [Arg1, Arg2, ..., ArgN])` 这个 BIF，会将将模组 `Mod` 中的函数 `Func`，应用于参数 `Arg1, Arg2, ... ArgN`。他等同于调用如下函数：


```erlang
Mod:Func(Arg1, Arg2, ..., ArgN)
```

`apply` 可让咱们调用某个模组中的一个函数，传递给他参数。令其与直接调用该函数不同的是，其中的模组名和/或函数名，可被动态地计算。


在假定所有 Erlang BIFs 都属于 `erlang` 模组下，那么他们也都可以 `apply` 调用。因此，要构建对某个 BIF 的动态调用，我们可写出如下代码：


```erlang
1> apply(erlang, atom_to_list, [hello]).
"hello"
```


*警告*：`apply` 的使用应尽可能避免。当某个函数的参数事先知道时，使用 `M:F(Arg1,Arg2,...ArgN)` 形式的调用，就要比使用 `apply` 好得多。当对函数的调用，是以 `apply` 构建的时，许多分析工具无法计算出发生了什么，进而一些确切的编译器优化就无法进行。因此，请尽量少使用 `apply`，而只在绝对需要时才使用。


要应用的 `Mod` 参数不必是个原子；他还可以是个元组。当我们调用以下这个语句时：

```erlang
{Mod, P1, P2, ..., Pn}:Func(A1, A2, ..., An)
```


那么实际调用的是以下函数：


```erlang
Mod:Func(A1, A2, ..., An, {Mod, P1, P2, ..., Pn})
```


[24.3 节 “有状态模组”](../part-v/Ch24-programming_idioms.md#有状态模组) 将详细讨论这种技术。



## 算术表达式


所有可能的算术表达式，都显示在下面的表格中。每种算术运算，都有一或两个参数 -- 这些参数在表中显示为整数或数值（*数值* 表示该参数可以是整数或浮点数）。


| *运算* | *描述* | *参数类型* | *优先级* |
| :-- | :-- | :-- | :-- |
| `+ X` | `+ X` | 数字 | 1 |
| `- X` | `- X` | 数字 | 1 |
| `X * Y` | `X * Y` | 数字 | 2 |
| `X / Y` | `X / Y` （浮点数除法） | 数字 | 2 |
| `bnot X` | `X` 的比特非运算 | 整数 | 2 |
| `X div Y` | `X` 与 `Y` 的整数除法 | 整数 | 2 |
| `X rem Y` | `X` 除以 `Y` 的整数余数 | 整数 | 2 |
| `X band ` | `X` 和 `Y` 的比特与运算 | 整数 | 2 |
| `X + Y` | `X + Y` | 数字 | 3 |
| `X - Y` | `X - Y` | 数字 | 3 |
| `X bor Y` | `X` 和 `Y` 的比特或运算 | 整数 | 3 |
| `X bxor Y` | `X` 和 `Y` 的比特异或运算 | 整数 | 3 |
| `X bsl N` | 算术的 `X` 向左移 `N` 位运算 | 整数 | 3 |
| `X bsr N` | 算术的 `X` 向右移 `N` 位运算 | 整数 | 3 |

<a name="table-3"></a>
***表 3*** - *算术表达式*

与每种运算符相关的，是个 *优先级*。复算术表达式的运算顺序，取决于运算符的优先级：优先级 1 的所有运算符，会被先求值，然后是优先级为 2 的所有运算符，依此类推。


咱们可使用括号，改变求值的默认顺序 -- 任何括号括起来的表达式，都会被优先求值。同等优先级的运算符，会被视为左关联的，而会被从左到右计算。


## 元数


某个函数的 *元数*，是指该函数的参数个数。在 Erlang 下，同一模组中具有同样名字与不同元数的两个函数，表示 *完全* 不同的函数。除了凑巧使用了同一个名字外，他们之间 *没有任何关系*。


依惯例，Erlang 程序员通常会将同名不同元数函数，用作辅助函数。下面是个示例：

[`lib_misc.erl`](http://media.pragprog.com/titles/jaerlang2/code/lib_misc.erl)

```erlang
sum(L) -> sum(L, 0).

sum([], N)	    -> N;
sum([H|T], N)	-> sum(T, H+N).
```


咱们在这里看到的是两个不同函数，一个的元数为 1，另一个元数为 2。



函数 `sum(L)` 会对列表 `L` 中的元素求和。他利用了个叫做 `sum/2` 的辅助例程，但这个例程可以叫做任何名字。咱们可以把这个例程叫做 `hedgehog/2`，而该程序的意义还是一样。不过，`sum/2` 是个更好的名字，因为他给了咱们程序的读者一个发生了什么事的线索，同时咱们也不必发明一个新名字（这总是很困难）。


通常，我们会通过不导出那些辅助函数，而 “隐藏” 他们。因此，定义了 `sum(L)` 的模组，就只会导出 `sum/1`，而不会导出 `sum/2`。


## 属性


模组属性的语法是 `-AtomTag(...)`，并被用于定义某个文件的一些属性。(*注意*：`-record(...)` 及 `-include(...)` 有着类似语法，但他们不属于模组属性）。模组属性有两种类型：预定义的和用户定义的。


### 预定义的模组属性


以下模组属性有着预定义的含义，而必须放在任何的函数定义前：



- `-module(modname).`

    模组的声明。`modname` 必须是个原子。该属性必须是文件中的首个属性。通常，`modname` 的代码，应存储在一个名为 `modname.erl` 的文件中。若咱们不这样做，那么自动的代码加载，就将无法正常工作；详情请参见 [8.10 小节，*动态代码加载*](#动态代码加载)。

- `-import(Mod, [Name1/Arity1, Name2/Arity2,...]).`

    `import` 声明指定了要导入某个模组的函数。上面的声明表示有着 `Arity1` 个参数的函数 `Name1`、有着 `Arity2` 个参数的函数 `Name2` 等，将从模组 `Mod` 导入。

    在某个函数已从某个模组导入后，那么在 *无需* 指定模组名字下，调用该函数即可达成。下面是个示例：

    ```erlang
    -module(abc).
    -export([f/1]).
    -import(lists, [map/2]).

    f(L) ->
        L1 = map(fun(X) -> 2*X end, L),
        lists:sum(L1).
    ```

    到 `map/2` 的调用不需要限定的模组名，而要调用 `sum/1`，我们需要在该函数调用中，包含模组的名字。

- `-export([Name1/Arity1, Name2/Arity2,...]).`

    导出当前模组中的 `Name1/Arity1`、`Name2/Arity2` 等函数。只有导出的函数，才能从模组外部调用。下面是个示例：

    ```erlang
    -module(abc).
    -export([f/1, a/2, b/1]).
    -import(lists, [map/2]).

    f(L) ->
        L1 = map(fun(X) -> 2*X end, L),
        lists:sum(L1).


    a(X, Y) -> c(X) + a(Y).
    a(X) -> 2 * X.
    b(X) -> X * X.
    c(X) -> 3 * X.
    ```

    这个导出声明意味着只有 `a/2` 和 `b/1` 可从 `abc` 这个模组外部调用。因此，比如从 shell（属于模组外部）调用 `abc:a(5)`，就将导致错误，因为 `a/1` 未从模组导出。

    ```erlang
    1> abc:a(1,2).
    7
    2> abc:b(12).
    144
    3> abc:a(5).
    ** exception error: undefined function abc:a/1
    ```

    这里的错误消息可能引起混淆。因相关函数未定义，这个到 `abc:a(5)` 的调用失败。其实际上在这个模组被定义了，只是他未被导出。

- `-compile(Options).`

    将 `Options` 添加到编译器选项的列表。`Options` 可以是单个编译器选项，也可以是个编译器选项的列表（这些选项在 `compile` 模组手册页中有说明）。

    *注意*：在调试程序时，编译器选项 `-compile(export_all).` 会经常被用到。这会在无需显式使用 `-export` 注解下，导出模组中的全部函数。


- `-vsn(Version).`

    指定模组版本。`Version` 是个任意的字面值项。`Version` 的值没有特定的语法或含义，但可用于分析程序，或文档目的。


### 用户定义的属性

用户定义属性的语法如下：


```erlang
-SomeTag(Value).
```


`SomeTag` 必须是个原子，而 `Value` 必须是个字面值项。这些模组属性的值，会被编译到模组中，并可在运行时被提取到。下面是个包含了一些用户定义属性的模组示例：


```erlang
-module(attrs).
-vsn("0.0.1").
-author({joe,armstrong}).
-purpose("example of attributes").
-export([fac/1]).


fac(1) -> 1;
fac(N) -> N * fac(N-1).
```


我们可如下提取到这些属性：


```erlang
1> attrs:module_info().
[{module,attrs},
 {exports,[{fac,1},{module_info,1},{module_info,0}]},
 {attributes,[{vsn,"0.0.1"},
              {author,[{joe,armstrong}]},
              {purpose,"example of attributes"}]},
 {compile,[{version,"9.0.1"},
           {options,[]},
           {source,"c:/Users/Hector/erlang-book/projects/ch08-code/attrs.erl"}]},
 {md5,<<133,23,25,113,143,224,222,86,254,91,190,122,9,27,
        43,91>>}]
```

包含在源码文件中的用户定义属性，会作为 `{attributes, ...}` 的子项重现。元组 `{compile, ...}` 包含由编译器添加的信息。`值 {version, "9.0.1"}` 是编译器的版本，而不应与模组属性中定义的 `vsn` 标记混淆。

在前面的示例中，`attrs:module_info()` 返回了个与某个已编译模组相关的所有元数据的属性列表。`attrs:module_info(X)`，其中 `X` 是 `exports`、`imports`、`attributes` 或 `compile` 之一，会返回了与该模组相关的各个属性。

> **译注**：分别对 `module_info/1` 运行上述原子参数的输出如下。
>
>```erlang
>2> attrs:module_info(attributes).
>[{vsn,"0.0.1"},
> {author,[{joe,armstrong}]},
> {purpose,"example of attributes"}]
>3> attrs:module_info(compile).
>[{version,"9.0.1"},
> {options,[]},
> {source,"c:/Users/hector.peng/erlang-book/projects/ch08-code/attrs.erl"}]
>4> attrs:module_info(exports).
>[{fac,1},{module_info,1},{module_info,0}]
>5> attrs:module_info(imports).
>** exception error: bad argument
>     in function  erlang:get_module_info/2
>        called as erlang:get_module_info(attrs,imports)
>     in call from attrs:module_info/1
>```
>
> 可以看出，在 Erlang/OTP 28 下 `moduel_info/1` 函数已不支持原子参数 `imports`。
>
> 参考：[`module_info/0` and `module_info/1` functions](https://www.erlang.org/docs/28/system/modules.html#module_info-0-and-module_info-1-functions)


请注意，每次某个模组被编译时，`module_info/0` 和 `module_info/1` 两个函数都会被自动创建出来。


要运行 `attrs:module_info`，我们必须把 `attrs` 这个模组的 beam 代码，加载到 Erlang 的虚拟机中。通过使用 `beam_lib` 模组，我们可在 *无需* 加载该模组下，提取到同样的信息。


```erlang
3> beam_lib:chunks("attrs.beam", [attributes]).
{ok,{attrs,[{attributes,[{author,[{joe,armstrong}]},
                         {purpose,"example of attributes"},
                         {vsn,"0.0.1"}]}]}}
```


> **译注**：`beam_lib:chunks/2` 只支持 `attributes` 这一个属性，而不支持 `compile`、`exports` 等其他属性。


`beam_lib:chunks` 在无需加载模组代码下，提取提取到某个模组中的属性数据。


## 块表达式


当 Erlang 语法要求使用单一表达式，但我们希望代码中的这一点处，使用一个表达式序列时，块表达式就会被用到。例如，在一个形式为 `[E || ...]` 的列表综合中，语法就要求 `E` 是个单一表达式，但我们可能打算在 `E` 中，完成好几件事。



```erlang
begin
    Expr1,
    ...,
    ExprN
end
```

咱们可使用块表达式，分组表达式序列，这类似于子句体。某个 `begin ... end` 块的值，为该块中最后一个表达式的值。


## 布尔值


Erlang 中并无明确的布尔类型；相反，原子 `true` 和 `false` 被赋予了特殊解释，而被用于表示布尔的两个字面值。


有时，我们会编写返回两个可能的原子值中一个的函数。在这种情况下，好的做法是确保他们要返回一个布尔值。此外，将咱们的函数，命名成明确反映其返回布尔值，也是个好主意。


例如，设想我们要编写个表示某文件状态的程序。我们可能发现咱们自己写了个返回 `open` 或 `closed` 的函数 `file_state(File)`。当我们编写这个函数时，我们可以考虑重新命名该函数，并让他返回布尔值。只要稍加思考，我们就可以把我们的程序，重写为使用一个名为 `is_file_open(File)`、返回 `true` 或 `false` 的函数。


使用布尔值而不是选择两个不同原子，表示状态的原因很简单。在标准库中，有大量工作于函数上的函数，都返回了布尔值。因此，当我们确保我们的所有函数都返回布尔值时，那么我们就可以将他们与标准库函数一起使用。


例如，设想我们有个文件列表 `L`，同时我们打算将其划分为一个打开文件列表，和一个关闭文件列表。在使用标准库时，我们可以写下如下代码：


```erlang
lists:partition(fun is_file_open/1, L)
```

而在使用咱们的 `file_state/1` 函数时，我们就不得不在调用这个库例程前，编写一个转换函数。


```erlang
lists:partition(fun(X) ->
                    case file_state(X) of
                        open   -> true;
                        closed -> false
                    end, L)
```


## 布尔值表达式


有四种可能的布尔表达式。

- `not B1`：逻辑非；
- `B1 and B2`：逻辑与；
- `B1 or B2`：逻辑或；
- `B1 xor B2`：逻辑异或。


在所有布尔表达式中，`B1` 和 `B2` 必须是布尔的字面量，或求值到布尔值的表达式。下面是一些示例：


```erlang
1> not true.
false
2> true and false.
false
3> true or false.
true
4> (2 > 1) or (3 > 4).
true
```

> **译注**：重置 shell 终端，按下 `Ctrl+g`，再按下 `s`、`c`。
>
> 参考：[How do I reset/clear erlang terminal](https://stackoverflow.com/a/37834853/12288760)


## 字符集


自 Erlang R16B 版本起，Erlang 源码文件被假定为是以 UTF-8 字符集编码的。在此之前，使用的是 ISO-8859-1 (Latin-1) 字符集。这意味着在无需使用任何转义序列下，源代码文件中即可使用所有 UTF-8 的可打印字符。


Erlang 内部并无字符数据类型。字符串实际上并不存在，而是以整数的列表表示。Unicode 的字符串可毫无问题地以整数列表表示。


## 注释


Erlang 中的注释以百分号字符 (`%`) 开始，一直延伸到行尾。没有块的注释。


*注意*：咱们经常会在代码示例中，看到双百分号字符 (`%%`)。Emacs 的 erlang 模式下双百分号可被识别到，而开启自动的注释行缩进。


```erlang
% This is a comment
my_function(Arg1, Arg2) ->
    case f(Arg1) of
        {yes, X} -> % it worked
            ..
```


## 动态代码加载


动态代码加载，是构建于 Erlang 核心中最令人惊讶的特性之一。最棒的是，他会在咱们无需关心后台发生了什么下，发挥作用。


其思路很简单：每次我们调用 `someModule:someFunction(...)` 时，我们将始终调用最新版本的这个模组中，该函数的最新版本，*即使在这个模组中的代码正在运行时，我们重新编译了该模组*。


当 `a` 在某个循环中调用了 `b`，而我们重新编译了 `b`，那么在下次调用 `b` 时，`a` 将自动调用 `b` 的新版本。当有许多不同正在运行，且所有进程都调用了 `b` 时，那么在 `b` 被重新编译了时，所有这些进程都将调用 `b` 的新版本。为了解其工作原理，我们将编写两个小模组：`a` 和 `b`。


[`b.erl`](http://media.pragprog.com/titles/jaerlang2/code/b.erl)



```erlang
-module(b).
-export([x/0]).


x() -> 1.
```


现在我们将编写 `a`。


[`a.erl`](http://media.pragprog.com/titles/jaerlang2/code/a.erl)


```erlang
-module(a).
-compile(export_all).

start(Tag) ->
    spawn(fun() -> loop(Tag) end).

loop(Tag) ->
    sleep(),
    Val = b:x(),
    io:format("Vsn1 (~p) b:x() = ~p~n", [Tag, Val]),
    loop(Tag).


sleep() ->
    receive
        after 3000 -> true
    end.
```

现在我们可以编译 `a` 和 `b`，并启动数个 `a` 的进程。


```erlang
1> c(b).
{ok,b}
2> c(a).
a.erl:2:2: Warning: export_all flag enabled - all functions will be exported
%    2| -compile(export_all).
%     |  ^

{ok,a}
3> a:start(one).
<0.96.0>
4> a:start(two).
<0.98.0>
Vsn1 (one) b:x() = 1
Vsn1 (two) b:x() = 1
Vsn1 (one) b:x() = 1
Vsn1 (two) b:x() = 1
```

`a` 的进程会休眠三秒钟，醒来并调用 `b:x()`，然后打印结果。现在我们将进入编辑器，将模组 `b` 改为如下内容：


```erlang
-module(b).
-export([x/0]).


x() -> 2.
```

然后在 shell 中重新编译 `b`。这就是发生的事情：


```erlang
5> c(b).
{ok,b}
Vsn1 (one) b:x() = 2
Vsn1 (two) b:x() = 2
Vsn1 (one) b:x() = 2
Vsn1 (two) b:x() = 2
...
```


两个原始版本的 `a` 仍在运行，但现在他们会调用 *新* 版本的 `b`。因此，当我们模组 `a` 中调用 `b:x()` 时，我们真正调用的是 “最新版本的 `b`”。我们可随意更改并重新编译 `b`，所有调用他的模组，都将自动调用新版本的 `b`，无需做任何特殊处理。


现在我们已重新编译过 `b`，但若我们修改并重新编译 `a`，会发生什么呢？我们将做个实验，并把 `a` 改成下面这样：


```erlang
-module(a).
-compile(export_all).

start(Tag) ->
    spawn(fun() -> loop(Tag) end).

loop(Tag) ->
    sleep(),
    Val = b:x(),
    io:format("Vsn2 (~p) b:x() = ~p~n", [Tag, Val]),
    loop(Tag).


sleep() ->
    receive
        after 3000 -> true
    end.
```


现在我们编译并启动 `a`。


```erl
6> c(a).
a.erl:2:2: Warning: export_all flag enabled - all functions will be exported
%    2| -compile(export_all).
%     |  ^

{ok,a}
Vsn1 (one) b:x() = 2
Vsn1 (two) b:x() = 2
...
7> a:start(three).
<0.153.0>
Vsn1 (one) b:x() = 2
Vsn2 (three) b:x() = 2
Vsn1 (two) b:x() = 2
Vsn1 (one) b:x() = 2
Vsn2 (three) b:x() = 2
...
```


这里发生了些有趣的事情。当我们启动新版本的 `a` 时，我们看到新版本在运行。但是，运行第一个版本 `a` 的现有进程，仍在没有任何问题的运行旧版本 `a`。


现在，我们可尝试再次修改 `b`。


```erlang
-module(b).
-export([x/0]).


x() -> 3.
```


我们将在 shell 中重新编译 `b`。请观察会发生什么。


```erlang
8> c(b).
{ok,b}
Vsn1 (two) b:x() = 3
Vsn1 (one) b:x() = 3
Vsn2 (three) b:x() = 3
...
```


现在，新旧两个版本的 `a` 都会调用最新版本的 `b`。

最后，我们将再次修改 `a`（这是第三次对 `a` 的修改）。



```erlang
-module(a).
-compile(export_all).

start(Tag) ->
    spawn(fun() -> loop(Tag) end).

loop(Tag) ->
    sleep(),
    Val = b:x(),
    io:format("Vsn2 (~p) b:x() = ~p~n", [Tag, Val]),
    loop(Tag).


sleep() ->
    receive
        after 3000 -> true
    end.
```


现在当我们重新编译 `a` 并启动一个新版本的 `a` 时，我们会看到如下内容：

```erlang
9> c(a).
a.erl:2:2: Warning: export_all flag enabled - all functions will be exported
%    2| -compile(export_all).
%     |  ^

{ok,a}
Vsn2 (three) b:x() = 3
Vsn2 (three) b:x() = 3
10> a:start(four).
<0.230.0>
Vsn2 (three) b:x() = 3
Vsn3 (four) b:x() = 3
Vsn2 (three) b:x() = 3
Vsn3 (four) b:x() = 3
...
```


输出结果包含由最后两个版本 `a`（版本 2 和 3）生成的字符串；运行版本 1 `a` 代码的那个进程，已经死亡。



Erlang 可同时运行某个模组的两个版本，即当前版本与原有版本。当咱们重新编译某个模组时，运行旧版本代码的任何进程都会被杀死，当前版本会变成原有版本，而新近编译的那个模组，则变成当前版本。请把这想象成有两个版本代码的某种移位寄存器。随着我们添加新代码，最早版本的代码就会被删除。一些进程可以运行该代码的原有版本，而另一些进程则可以同时运行该代码的新版本。


请阅读 [`purge_module` 文档](https://www.erlang.org/doc/apps/erts/erlang.html#purge_module/1) 了解更多详情。


## Erlang 的预处理器


在某个 Erlang 模组被编译前，其会被 Erlang 预处理器自动处理。预处理器会展开源文件中可能的任何宏，并插入任何必要的包含文件。


通常情况下，咱们将无需查看预处理器的输出，但在特殊情况下（例如，在调试某个问题宏时），咱们可能会要保存预处理器的输出。要查看模组 `some_module.erl` 的预处理结果，就要操作系统的 shell 命令。


```erlang
$ erlc -P some_module.erl
```

这会产生一个名为 `some_module.P` 的清单文件。


> **译注**：`abc.erl` 源文件内容如下。
>
>```erlang
>-module(abc).
>-export([f/1, a/2, b/1]).
>-import(lists, [map/2]).
>
>f(L) ->
>    L1 = map(fun(X) -> 2*X end, L),
>    lists:sum(L1).
>
>
>a(X, Y) -> c(X) + a(Y).
>a(X) -> 2 * X.
>b(X) -> X * X.
>c(X) -> 3 * X.
>```
>
> 运行 `erlc -P abc.erl` 后得到的 `abc.P` 文件内容如下。
>
>```erlang
>-file("abc.erl", 1).
>
>-module(abc).
>
>-export([f/1,a/2,b/1]).
>
>-import(lists, [map/2]).
>
>f(L) ->
>    L1 =
>        map(fun(X) ->
>                   2 * X
>            end,
>            L),
>    lists:sum(L1).
>
>a(X, Y) ->
>    c(X) + a(Y).
>
>a(X) ->
>    2 * X.
>
>b(X) ->
>    X * X.
>
>c(X) ->
>    3 * X.
>
>
>
>```


## 转义序列


咱们可在字符串和带引号原子内，使用转义序列输入任何的不可打印字符。所有可能的转义序列如 [表 4，*转义序列*](#table-4) 所示。


我们来在 shell 下给出几个示例，说明这些约定是如何起作用的。(注意：格式字符串中的 `~w` 会在不带任何美化打印结果的尝试下，打印出列表。）


```erlang
%% Control characters
1> io:format("~w~n", ["\b\d\e\f\n\r\s\t\v"]).
[8,127,27,12,10,13,32,9,11]
ok
%% Octal characters in a string
2> io:format("~w~n", ["\123\12\1"]).
[83,10,1]
ok
%% Quotes and escapes in a string
3> io:format("~w~n", ["\'\"\\"]).
[39,34,92]
ok
%% Character codes
4> io:format("~w~n", ["\a\z\A\Z"]).
[97,122,65,90]
ok
```


| *转义序列* | *意义* | *整数代码* |
| :-- | :-- | :-- |
| `\b` | 退格 | 8 |
| `\d` | 删除 | 127 |
| `\e` | 转义，escape | 27 |
| `\f` | 换页，form feed | 12 |
| `\n` | 新行，new line | 10 |
| `\r` | 回车，catriage return | 13 |
| `\s` | 空格 | 32 |
| `\t` | 制表符，tab | 9 |
| `\v` | 竖向制表符，vertical tab | 11 |
| `\x{...}` | 十六进制字符（`...` 为十六进制字符） |  |
| `\^a..\^z` 或 `\^A..\^Z` | `Ctrl+A` 到 `Ctrl+Z` | 1 到 26 |
| `\'` | 单引号 | 39 |
| `\"` | 双引号 | 34 |
| `\\` | 反斜杠 | 92 |
| `\C` | `C` 的 ASCII 代码（`C` 是个字符） | （某个整数） |


<a name="table-4"></a>
***表 4*** -- *转义序列*


## 表达式与表达式序列

在 Erlang 中，任何可求值的东西，都称为 *表达式*。这意味着诸如 `catch`、`if` 及 `try...catch` 等，都属于表达式。记录声明和模组属性等物件，无法被求值，因此他们不属于表达式。


*表达式序列* 是由逗号分隔的表达式的序列。他们在紧随 `->` 箭头处随处可见。表达式序列 `E1, E2, ..., En` 的值，被定义为该序列中最后一个表达式的值。这个值是使用在计算 `E1`、`E2` 等的值时，所创建的全部绑定值计算得出的。这等同于 LISP 中的 `progn`。


## 函数引用


我们经常会打算引用某个定义在当前模组，或某个外部模组中的函数。为此，咱们可使用下面的写法：


- `fun LocalFunc/Arity`

    这用于引用当前模组中，名为 `LocalFunc` 并有着 `Arity` 个参数的本地函数。


- `fun Mod:RemoteFunc/Arity`


    这用于引用模组 `Mod` 中，有着 `Arity` 个参数、名为 `RemoteFunc` 的某个外部函数。


下面是当前模组中某个函数引用的示例：

```erlang
-module(x1).
-export([square/1, ...]).


square(X) -> X * X.
...
double(L) -> lists:map(fun square/1, L).
```

当我们打算调用某个远端模组中的一个函数时，我们可像下面示例中那样，引用该函数：


```erlang
-module(x2).
...
double(L) -> lists:map(fun x1:square/1, L).
```

其中 `fun x1:square/1` 表示模组 `x1` 中的函数 `square/1`。

请注意，包含模组名称的函数引用，为动态代码升级提供了切换点。详情请参阅 [8.10 节，动态代码加载](#动态代码加载)。

> *知识点*：
>
>- switch-over pointer for dynamic code upgrade


## 包含文件


以如下语法，文件可被包含：

```erlang
-include(Filename).
```


在 Erlang 中，该约定是包含文件要有 `.hrl` 的扩展名。其中 `FileName` 应包含绝对路径或相对路径，以便预处理器可定位到相应文件。使用以下语法，库的头文件即可被包含：


```erlang
-include_lib(Name).
```

下面是个示例：

```erlang
-include_lib("kernel/include/file.hrl").
```


在这种情况下，Erlang 的编译器将找到相应的包含文件。(在上面的示例中，`kernel` 指的是定义该头文件的应用。）


包含文件通常会包含一些记录定义。当许多模组需要共享共同的一些记录定义时，那么这些共同的记录定义，就会被放入由所有需要这些定义的模组，所包含的文件中。



<a name="列表的加法与减法运算符"></a>
## 列表运算 `++` 与 `--`


`++` 和 `--` 是列表加法与减法的下位运算符。


`A ++ B` 会将 `A` 与 `B` 相加（即追加）。


而 `A -- B` 则会从列表 `A` 中减去列表 `B`。减法表示 `B` 中的每个元素，都会被从 `A` 中移除。注意，当某个符号 `X` 在 `B` 中只出现 `K` 次时，那么只有 `X` 在 `A` 中的前 `K` 次出现，才会被移除。


下面是一些示例：


```erlang
1> [1,2,3] ++ [4,5,6].
[1,2,3,4,5,6]
2> [a,b,c,1,d,e,1,x,y,1] -- [1].
[a,b,c,d,e,1,x,y,1]
3> [a,b,c,1,d,e,1,x,y,1] -- [1,1].
[a,b,c,d,e,x,y,1]
4> [a,b,c,1,d,e,1,x,y,1] -- [1,1,1].
[a,b,c,d,e,x,y]
5> [a,b,c,1,d,e,1,x,y,1] -- [1,1,1,1].
[a,b,c,d,e,x,y]
```


`++` 也可以用于模式中。在匹配字符串时，我们可写出如下的模式：


```erlang
f("begin" ++ T) -> ...
f("end" ++ T)   -> ...
```
第一个子句中的模式，会被展开为 `[$b,$e,$g,$i,$n|T]`。


## 宏

Erlang 的宏，被写作下面这样：


```erlang
-define(Constant, Replacement).
-define(Func(Var1, Var2,.., Var), Replacement).
```


当遇到 `?MacroName` 形式的某个表达式时，宏就会被 Erlang 的预处理器 `epp` 展开。宏定义中出现的变量，会在该宏的调用相应位置，匹配到完整形式。


```erlang
-define(macro1(X, Y), {a, X, Y}).

foo(A) ->
    ?macro1(A+10, b).
```

会展开为如下：


```erlang
foo(A) ->
    {a, A + 10, b}.
```


此外，还有一些提供当前模组信息的预定义宏。他们如下：

- `?FILE` 会展开为当前文件名；
- `?MODULE` 会展开为当前模组名字；
- `?LINE` 会展开为当前行号。


### 宏内的控制流

在模组内部，以下指令受支持；咱们可使用他们，控制宏的展开：


- `-undef(Macro).`

    解除该宏定义；在此之后咱们就无法调用该宏。

- `-ifdef(Macro).`

    只有在 `Macro` 已被定义时，才计算随后的代码行。

- `-ifndef(Macro).`

    只有在 `Macro` 未被定义时，才计算随后的代码行。


- `-else.`

    允许在 `ifdef` 或 `ifndef` 语句后使用。当条件为假时，`else` 后的语句就会被求值。

- `-endif.`


    标记某个 `ifdef` 或 `ifndef` 语句的结束。



条件的宏必须要正确嵌套。依常规他们会如下分组：


```erlang
-ifdef(<FlagName>).
-define(...).
-else.
-define(...).
-endif.
```


我们可使用这些宏，定义一个 DEBUG 宏。下面是个示例：



[`m1.erl`](http://media.pragprog.com/titles/jaerlang2/code/m1.erl)

```erlang
-module(m1).
-export([loop/1]).


-ifdef(debug_flag).
-define(DEBUG(X), io:format("DEBUG ~p:~p ~p~n", [?MODULE, ?LINE, X])).
-else.
-define(DEBUG(X), void).
-endif.


loop(0) -> done;
loop(N) ->
    ?DEBUG(N),
    loop(N-1).
```


*注意*：`io:format(String, [Args])` 会根据 `String` 中的格式化信息，将 `[Args]` 中的变量打印在 Erlang shell 中。格式化代码前有个 `~ `符号。`~p` 是 *美化打印，pretty print* 的缩写，而 `~n` 会产生一个换行。`io:format` 理解大量的格式化选项；更多信息，请参阅第 [将项的列表写到某个文件](../part-iv/Ch16-programming_with_files.md#将项的列表写到某个文件)。


要启用这个 DEBUG 宏，我们就要在编译这段代码时，设置 `debug_flag` 。这是以一个 `c/2` 的额外参数完成，如下所示：



```erlang
1> c(m1, {d, debug_flag}).
{ok,m1}
2> m1:loop(4).
DEBUG m1:14 4
DEBUG m1:14 3
DEBUG m1:14 2
DEBUG m1:14 1
done
```


当 `debug_flag` 未被设置时，该宏就会展开为 `void` 这个原子。这种名字的选取，没有任何意义；他只是提醒咱们，没人会对该宏的值感兴趣。


## 模式中的匹配运算符


咱们假设我们有这样一段代码：


```erlang
func1([{tag1, A, B}|T]) ->
    ...
    f(..., {tag1, A, B}, ...)
    ...
```

在第 1 行，我们对 `{tag1, A, B}` 模式匹配，而在第 3 行，我们以一个为 `{tag1, A, B}` 的参数，调用了 `f`。当我们这样做时，系统会重建 `{tag1, A, B}` 这个项。完成这点的一种更高效、更少出错方法，是将这个模式，赋值给一个临时变量 `Z`，并将其传递给 `f`，如下所示：


```erlang
func1([{tag1, A, B}=Z|T]) ->
    ...
    f(..., Z, ...)
    ...
```


匹配运算符可用在模式的任何位置，因此，当我们有两个需要重建的项，如下面这段代码中时：


```erlang
func1([{tag, {one, A}, B}|T]) ->
    ...
    ... f(..., {tag, {one, A}, B}, ...),
    ... g(..., {one, A}, ...)
    ...
```

此时我们可引入两个新变量 `Z1` 和 `Z2`，并写下以下代码：


```erlang
func1([{tag, {one, A}=Z1, B}=Z2|T]) ->
    ...
    ... f(..., Z2, ...),
    ... g(..., Z1, ...)
    ...
```


## 数字


Erlang 种的数字，可以是整数或浮点数。


### 整数


整数算术是精确的，同时可被表示为某个整数的位数，只受可用内存的限制。


整数以三种不同语法之一写出。


- *常规语法*

    这里整数如咱们预期那样写下。例如，`12`、`12375` 及 `-23427` 都是整数。

- *底数为 K 的整数*

    十以外基数的整数，会以 `K#Digits` 语法书写；因此，我们可以将某个二进制数，写作 `2#00101010`，或将某个十六进制数，写作 `16#af6bfa23`。对于大于 10 的底数，字符 `abc...`（或 `ABC...`）表示 `10`、`11`、`12` 等数字。最大的底数是 36。

- *`$` 语法*


    `$C` 这种语法表示 ASCII 字符 `C` 的整数代码。因此，`$a` 是 `97` 的简称，`$1` 是 `49` 的简称，依此类推。

    紧接着 `$` 后，我们还可使用 [表 4 “转义序列”](#table-4) 中描述的任何转义序列。因此，`$\n` 表示 `10`，`$\^c` 表示 `3`，以此类推。


下面是一些整数的示例：


```erlang
5> X = [0, 65, 2#010001110, -8#377, 16#fe34, 16#FE34, 36#wow].
[0,65,142,-255,65076,65076,42368]
```


### 浮点数


浮点数有五个部分：

- 可选的符号；
- 整数部分；
- 小数点；
- 小数部分；
- 以及可选的指数部分。


以下是一些浮点数的示例：


```erlang
8> F1 = [1.0, 3.14159, -2.3e+6, 23.56E-27].
[1.0,3.14159,-2.3e6,2.356e-26]
```


解析后，浮点数在内部会以 IEEE 754 的 64 位格式表示。绝对值范围在 10<sup>-323</sup> 到 10<sup>308</sup> 之间的实数，可以 Erlang 的浮点数表示。


## 运算符优先级


[表 5，*运算符优先级*](#table-5) 以降序的优先级，展示了所有 Erlang 运算符及其关联性。运算符的优先级和关联性，用于确定无父表达式中的求值顺序。


| 运算符 | 关联性 |
| :-- | :-- |
| `:` |  |
| `#` |  |
| （一元）`+`、（一元）`-`、`bnot`、`not` |  |
| `/`、`*`、`div`、`rem`、`band`、`and` | 左关联 |
| `+`、`-`、`bor`、`bxor`、`bsl`、`bsr`、`or`、`xor` | 左关联 |
| `++`、`--` | 右关联 |
| `andaslo` |  |
| `orelse` |  |
| `=!` | 右关联 |
| `catch` |  |


<a name='table-5'></a>

***表 5*** -- *运算符优先级*


优先级较高的表达式（在表格中较高处），会先被求值，然后优先级较低的表达式再被求值。因此，例如要求值 `3+4*5+6`，我们会先求值其中的子表达式 `4*5`，因为在表中 （`*`）高于 (`+`) 。现在我们要求值 `3+20+6`。由于 (`+`) 是个左关联运算符，我们将其解释为 `(3+20)+6`，因此我们要先计算 `3+20`，得到 `23`；最后我们计算 `23+6`。


在其完整括符形式下，`3+4*5+6` 表示 `((3+(4*5))+6)` 。与所有程序语言一样，使用括号表示范围，比依赖优先级规则会更好。



## 进程字典


Erlang 中的每个进程，都有称为 *进程字典* 的自己私有数据存储。所谓进程字典，是个由键值集合组成的关联数组（在别的语言中可能称为 *映射*、*哈希图* 或 *哈希表*）。其中每个键都只有一个值。


该字典可使用以下 BIFs 操作：

- `put(Key, Value) -> OldValue.`

    添加一个键值关联到进程字典。`put` 的值为 `OldValue`，即与 `Key` 关联的早先值。在没有早先值时，原子 `undefined` 会被返回。

- `get(Key) -> Value.`

    查找 `Key` 的值。当该字典中存在 `Key, Value` 关联时，就返回 `Value`；否则返回原子 `undefined`。

- `get() -> [{Key, Value}].`

    以 `{Key,Value}` 元组列表形式，返回整个字典。

- `get_keys(Value) -> [Key].`


    返回该字典中有着 `Value` 值的键列表。

- `erase(Key) -> Value.`


    返回与 `Key` 关联的值，或在没有与 `Key` 关联值时，则返回原子 `undefined`。最后，删除与 `Key` 关联的值。

- `erase() -> [{Key, Value}].`


    擦除整个进程字典。返回值是个表示该字典被擦除前状态的 `{Key,Value}` 元组列表。


下面是个示例：


```erlang
1> erase().
[{'$ancestors',[<0.72.0>,<0.70.0>,user_drv,<0.69.0>,
                <0.65.0>,kernel_sup,<0.47.0>]}]
2> put(x, 20).                                                                                                                          undefined
3> get(x).
20
4> put(y, 40).
undefined
5> get(y).                                                                                                                              40
6> get().
[{y,40},{x,20}]
7> erase(x).
20
8> get().
[{y,40}]
9> put(x, 50).
undefined
10> put(x, 60).
50
```


正如咱们所看到的，进程字典中变量的行为，与命令式编程语言中传统可变变量非常相似。当咱们使用进程字典时，咱们的代码将不再是无副作用的，我们在 [“Erlang 变量不会变”](Ch03-basic_concepts.md#Erlang-变量不会变) 中，曾讨论过的使用非破坏性变量的所有好处，也不适用。因此，咱们应尽量少用进程字典。


*注意*：我（作者）很少使用进程字典。使用进程字典会将一些微妙错误，引入咱们的程序，而使其难于调试。我赞成的一种用法，是使用进程字典存储一些 “只写一次” 的变量。当某个键确实只获取一次值，而不会改变该值时，那么将其存储在进程字典中，有时是可接受的。


## 引用


所谓 *引用*，是全局唯一的一些 Erlang 项。他们是以 BIF `erlang:make_ref()` 创建的。对于创建一些可包含在数据中，然后在稍后阶段用于相等比较的标签，引用很有用。例如，某个错误跟踪系统，就可能会将引用添加到每个新的错误报告，以赋予其唯一标识。


## 短路的布尔表达式


短路布尔表达式，是一种其参数只在必要时才求值的布尔表达式。


有两种 “短路” 布尔表达式。


- `Epr1 orelse Epr2`


    这会先对 `Expr1` 求值。当 `Expr1` 求值为 `true` 时，`Expr2` 就不会求值。当 `Expr1` 求值为 `false` 时，则 `Expr2` 会被求值。

- `Epr1 andalso Epr2`


    这会先对 `Expr1` 求值。当 `Expr1` 求值为 `true` 时，`Expr2` 会被求值。当 `Expr1` 求值为 `false` 时，则 `Expr2` 不会被求值。



*注意*：在对应布尔表达式（`A or B`；`A and B`）中，即使表达式的真值，可通过只求值第一个表达式确定，其中两个参数也总是会被求值。


## 项的比较


有八种可能的项比较运算，如 [表 6 “项的比较”](#table-6) 所示。


为比较目的，对所有项的总体排序被定义了出来。如此定义，以便以下情况成立：


```erlang
number < atom < reference < fun < port < pid < tuple(and record) < map < list < binary
```


这意味着，例如，某个数字（任何数字）被定义为小于某个原子（任何原子），某个元组要大于某个原子，依此类推。(请注意，为排序目的，端口和 PID 也包含在此清单中。我们稍后将讨论这些内容。）


对所有项有个总的排序，意味着我们可对任何类型的列表排序，并根据键的排序顺序，构建出高效的数据访问例程。


除 `=:=` 和 `=/=` 外的全部项比较运算符，在他们的参数是数字时，都会以如下方式行事：


- 当一个参数是整数，另一个是浮点数时，那么在进行比较前，会将整数转换为浮点数；
- 当两个参数都是整数，或都是浮点数时，那么两个参数将 “按原样” 使用，即不会转换。


咱们应对 `==` 的使用非常小心（尤其在咱们是个 C 或 Java 程序员时）。99% 的情况下，咱们都应使用 `=:=` 。*只有* 在比较浮点数和整数时，`==` 才有用。而 `=:=` 则用于测试两个项是否 *一致*。


一致标识有着同样的值（就像 Common Lisp 的 EQUAL 一样）。由于值是不可变的，故这并未意指任何指针一致的概念。当存疑时，就要使用 `=:=`，并在咱们看到 `==` 有所防备。请注意，类似批注也适用于 `/=` 和 `=/=` 的使用，其中 `/=` 表示 “不等于”，而 `=/=` 则表示 “不相同”。


*注意*：在许多库及公开代码中，咱们会看到在运算符应是 `=:=` 处，使用了 `==`。 幸运的是，这种错误并不经常造成错误程序，因为在 `==` 的参数，未包含任何浮点数时，那么这两个运算符的行为是一样的。


咱们还应注意，函数子句匹配，总是意味着精确的模式匹配，因此当咱们定义了个 `fun F = fun(12) -> ... end`，那么尝试计算 `F(12.0)` 将失败。


| *运算符* | 意义 |
| :-- | :-- |
| `X > Y` | `X` 大于 `Y`。 |
| `X < Y` | `X` 小于 `Y`。 |
| `X =< Y` | `X` 小于等于 `Y`。 |
| `X >= Y` | `X` 大于等于 `Y`。 |
| `X == Y` | `X` 等于 `Y`。 |
| `X /= Y` | `X` 不等于 `Y`。 |
| `X =:= Y` | `X` 与 `Y` 完全一致。 |
| `X =/= Y` | `X` 不同于 `Y`。 |

<a name="table-6"></a>
***表 6*** -- *项的比较*



## 元组的模组


当我们调用 `M:f(Arg1,Arg2,...,ArgN)` 时，我们已假定 `M` 是个模组名。但 `M` 也可以是个形式为 `{Mod1, X1, X2, ... Xn}` 的元组。在这种情况下，函数 `Mod1:f(Arg1, Arg2, ..., Arg3, M)` 会被调用。


这种机制可用于创建 [24.3 节，有状态模组](../part-v/Ch24-programming_idioms.md#有状态模组) 中讨论的 “有状态模组”，以及创建 [24.4 节，适配器模式](../part-v/Ch24-programming_idioms.md#适配器模式) 中讨论的 “适配器模式”。


## 下划线变量


关于变量，还有一点要说明。特殊语法 `_VarName` 用于普通变量，而非匿名变量。通常情况下，当某个子句中的一个变量只使用了一次时，编译器将生成一条警告消息，因为这通常是个错误迹象。而当某个变量只使用了一次，但以下划线开头时，该警告消息将不会产生。

由于 `_Var` 是个普通变量，当忘记这一点并将其用作 “无所谓” 模式时，一些非常微妙错误就会引发。在复杂的模式匹配中，就很难发现这个问题，比如当 `_Int` 在他不应出现的地方重复出现时，就会造成模式匹配失败。


下划线变量主要有两种用途。


- 给某个我们不打算使用的变量命名。也就是说，写下 `open(File, _Mode)` 要比写下 `open(File, _)`，会让程序更具可读性；
- 用于调试目的。例如，设想我们写下这段代码：


```erlang
some_func(X) ->
    {P, Q} = some_other_func(X),
    io:format("Q = ~p~n", [Q]),
    P.
```

这段代号会在没有错误消息下编译。

现在注释掉以下格式化语句：


```erlang
some_func(X) ->
    {P, Q} = some_other_func(X),
    io:format("Q = ~p~n", [Q]),
    P.
```


当我们编译这段代码时，编译器将发出变量 `Q` 未使用的警告消息。

> **译注**：告警消息如下。
>
>```erlang
>underscore_var.erl:5:9: Warning: variable 'Q' is unused
>%    5|     {P, Q} = some_other_func(X),
>%     |         ^
>
>{ok,underscore_var}
>```


而当我们重写该函数如下：


```erlang
some_func(X) ->
    {P, _Q} = some_other_func(X),
    io:format("_Q = ~p~n", [_Q]),
    P.
```

那么我们可以注释掉那个格式语句，同时编译器不会抱怨。



现在，我们实际上已经完成了顺序 Erlang 的学习。


在接下来的两章中，我们将完成本书的第二部分。我们将从用于描述 Erlang 函数类型的类型注解，及讨论一些可用于对 Erlang 代码进行类型检查的工具开始。在第二部分的最后一章，我们将探讨编译和运行咱们程序的不同方法。


## 练习


1. 请重读本章有关 `Mod:module_info()` 的小节。给出 `dict:module_info().` 命令，那么该模组返回多少个函数？

2. 命令 `code:all_loaded()` 会返回已加载到 Erlang 系统的所有模组的 `{Mod,File}` 对列表。请使用 `Mod:module_info()` 这个 BIF，找出这些模组。请编写确定哪个模组导出函数最多，以及哪个函数名最常用的一些函数。请编写一个查找所有不明确的（单一的）函数名，即只在一个模组中使用的那些函数名的函数。
