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
**表格 3** -- **算术表达式**

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


```erlang
2> attrs:module_info(attributes).
[{vsn,"0.0.1"},
 {author,[{joe,armstrong}]},
 {purpose,"example of attributes"}]
3> attrs:module_info(compile).
[{version,"9.0.1"},
 {options,[]},
 {source,"c:/Users/ZBT7RX/erlang-book/projects/ch08-code/attrs.erl"}]
4> attrs:module_info(exports).
[{fac,1},{module_info,1},{module_info,0}]
5> attrs:module_info(imports).
** exception error: bad argument
     in function  erlang:get_module_info/2
        called as erlang:get_module_info(attrs,imports)
     in call from attrs:module_info/1
```

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





### 整数


## 表达式与表达式序列

## 列表操作 `++` 与 `--`

## 包含文件
