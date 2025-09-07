# 类型


Erlang 有我们可以用于定义新的数据类型，及将类型注解添加到咱们代码的类型记法。类型注解使代码更易于理解和维护，以及可用于在编译时检测错误。


在本章中，我们将介绍类型记法，并讨论两个可用于查找咱们代码中错误的程序。


我们将要讨论的这两个程序叫做 `dialyzer` 和 `typer`，且他们都包含在标准 Erlang 发行版中。`dialyzer` 是 “DIscrepancy AnaLYZer for ERlang programs”，ERlang 程序差异分析器的缩写，他所做的正如其名称所暗示的那样：他会发现 Erlang 代码中的差异。`typer` 提供有关咱们程序中用到的类型信息。在完全没有类型注释下，`dialyzer` 和 `typer` 都会很好地工作，但若咱们将类型注解，添加到咱们的程序，这两个工具的分析质量将得到提升。


这是相当复杂的一章，因此我们将从一个简单示例开始，然后我们将深入研究类型语法；之后，我们将与 `dialyzer` 进行会话。我们将讨论用于 `dialyzer` 的工作流程，以及 `dialyzer` 无法发现的错误类别。最后，我们将以 `dialyzer` 工作原理相关理论结束本章，这将有助于我们理解 `dialyzer` 发现的那些错误。


## 指定数据与函数的类型


我们将去徒步旅行，很幸运有个可以用来规划我们徒步旅行的模组。该模组这样开始：


[`walks.erl`](http://media.pragprog.com/titles/jaerlang2/code/walks.erl)

```erlang
-module(walks).
-export([plan_route/2]).


-spec plan_route(point(), point()) -> route().

-type direction() :: north | south | east | west.
-type point()	  :: {integer(), integer()}.
-type route()	  :: [{go,direction(),integer()}].


...
```


这个模组导出了个名为 `plan_route/2` 的函数。该函数的输入和返回类型，指定于一个 *类型规范* 中，同时三种新的类型在使用 *类型声明* 下被定义出来。他们会如下解释：


- `-spec plan_route(point(), point()) -> route().`


表示当函数 `plan_route/2` 以两个参数调用，且两个都是 `point()` 类型时，那么他将返回一个类型为 `route()` 的对象。


- `-type direction() :: north | south | east | west.`


引入一个名为 `direction()` 的新类型，其值为原子 `north`、`south`、`east` 或 `west` 之一。


- `-type point() :: {integer(), integer()}.`


表示 `point()` 这个类型，是两个整数的元组（`integer()` 是个预定义的类型）。

- `-type route() :: [{go, direction(), integer()}].`


将类型 `route()` 定义为一个 3 元素元组的列表，其中每个元组包含原子 `go` 、一个 `direction` 类型的对象和一个整数。`[X]` 这种记法，表示一个类型 `X` 的列表。



仅从这些类型注解，我们就可以设想运行 `plan_route` 就会看到这样的结果：

```erlang
> walks:plan_route({1,10}, {25, 57}).
[{go, east, 24},
 {go, north, 47},
 ...
]
```


当然，我们不知道函数 `plan_route` 是否会返回任何值；他可能会崩溃而不返回值。但当他真的返回了值时，那么在输入参数是  `point()` 时，返回值的类型就应是 `route()`。我们也不知道上一表达式中的数字表示什么。他们是英里、公里、厘米等等吗？我们只知道类型声明告诉我们的，即他们是些整数。


要将表达能力添加到这些类型，我们可以一些描述性变量注解他们。例如，我们可将 `plan_route` 的规格，修改为以下内容：


```erlang
-spec plan_route(From:: point(), To:: point()) -> ...
```


类型注解中的名字 `From` 和 `To`，给到用户有关这些参数在函数中所扮演角色的一些想法。他们还用于将文档中的名称，链接到类型注解中的变量。Erlang 的官方文档，使用了编写类型注解的严格规则，因此类型注解中的名称，与相应文档中使用的名称相对应。


表明我们的路径从 `From` 处开始，同时 `From` 是个整数对，对于该函数的文档可能足够了，也可能还不够；这取决于语境。通过添加更多信息，我们可以轻松完善这些类型定义。例如，通过写下这段代码：


```erlang
-type angle()	    :: {Degrees::0..360, Minutes::0..60, Seconds::0..60}.
-type position()	:: {latitude | longtitue, angle()}.
-type plan_route1(From::position(), To::position()) -> ...
```


这种新的形式，给到我们更多信息，但同样需要猜测。我们可能会猜测角度的单位是度，因为允许的数值范围是 `0` 到 `360`，但他们可能只是弧度，这样我们就猜错了。


随着类型注解变长，我们可能会以增加冗长为代价，最终提高精确度。注解篇幅的增加，可能使代码更加难以阅读。写出良好的类型注解，与编写清晰的代码一样，都是一门艺术 -- 这非常困难，需要多年的实践。这是一种禅宗冥想的形式：咱们做得越多，就会变得越容易，咱们也就越来越好！


我们已经看了一个如何定义类型的简单示例，下一节会正式介绍类型记法。一旦我们对类型符号感到满意，我们将与 `dialyzer` 会话。



## Erlang 的类型记法


到目前为止，我们已经经由一些非正式的描述，介绍了类型。要充分利用类型系统，我们需要理解类型语法，这样我们才能读写更精确的类型描述。


### 类型的语法


类型是使用如下语法，被非正式地定义的：


```erlang
T1 :: A | B | C ...
```


这表示 `T1` 被定义未 `A`、`B` 或 `C` 之一。


运用这种记法，我们可将 Erlang 的类型子集定义如下：


```erlang
Type :: any() | none() | pid() | port() | reference() | []
      | Atom | binary() | float() | Fun | Integer | [Type]
      | Tuple | Union | UserDefined

Union :: Type1 | Type2 | ...

Atom :: atom() | Erlang_Atom

Integer :: integer() | Min .. Max

Fun :: fun() | fun((...) -> Type)

Tuple :: tuple() | {T1, T2, ... Tn}
```


在上一示例中，`any()` 表示任何的 Erlang 项，`X()` 表示类型 `X` 的某个 Erlang 对象，而 `none()` 这个符号，则用于表示某个不会返回的函数类型。


`[X]` 写法表示 `X` 类型的一个列表，而 `{T1, T2, ..., Tn}` 表示参数为 `T1, T2, ... Tn` 类型，大小为 `n` 的一个元组。


新类型可以下面的语法定义：


```erlang
-type NewTypeName(TVar1, TVar2, ... TVarN) :: Type.
```


下面是一些示例：


```erlang
-type onOff()	    :: on | off.
-type person()	    :: {person, name(), age()}.
-type people()	    :: [person()].
-type name()	    :: {firstname, string()}.
-type age()	        :: integer().
-type dict(Key,Val) :: [{Key,Val}].
```


这些规则表明，比如，`{firstname, "dave"}` 属于 `name()` 类型，而 `[{person, {firstname, "john"}, 35}, {person, {firstname, "mary"}, 26}]` 属于 `people()` 类型，以此类推。类型 `dict(Key,Val)` 展示了类型变量的使用，并将字典类型定义为 `{Key, Val}` 元组的一个列表。



### 预定义的类型


除类型语法外，以下类型别名还被预定义了出来：


```erlang
-type term() :: any().
-type boolean() :: true | false.
-type byte() :: 0..255.
-type char() :: 0..16#10ffff.
-type number() :: integer() | float().
-type list() :: [any()].
-type maybe_improper_list() :: maybe_improper_list(any(), any()).
-type maybe_improper_list(T) :: maybe_improper_list(T, any()).
-type string() :: [char()].
-type nonempty_string() :: [char(),...].
-type iolist() :: maybe_improper_list(byte() | binary() | iolist(),
                                      binary() | []).
-type module() :: atom().
-type mfa() :: {atom(), atom(), atom()}.
-type node() :: atom().
-type timeout() :: infinity | non_neg_integer().
-type no_return() :: none().
```


其中 `maybe_improper_list` 用于指定那些最终尾部为非空，non-nil，的列表类型。这类列表很少使用，但要指定他们的类型！


还有少数其他预定义的类型。其中 `non_neg_integer()` 为非负整数，`pos_integer()` 为正整数，`neg_integer()` 为负整数。最后，`[X,...]` 这种写法表示类型 `X` 的一个非空列表。

现在我们可以定义类型了，接下来是函数规范。


### 指定函数的输入与输出类型


函数规范指出了某个函数的参数类型为何，以及该函数的返回值类型为何。函数规范被写如下：


```erlang
-spec functionName(T1, T2, ..., Tn) -> Tret when
    Ti :: Typei,
    Tj :: Typej,
    ...
```


这里 `T1, T2, ..., Tn` 描述某个函数参数的类型，`Tret` 描述该函数返回值的类型。必要时可在可选的 `when` 关键字后，引入其他类型变量。


我们将以一个示例开始。下面的类型规范：


```erlang
-spec file:open(FileName, Modes) -> {ok, Handle} | {error, Why} When
    FileName	:: string(),
    Modes	    :: [Mode],
    Mode	    :: read | write | ...
    Handle	    :: file_handle(),
    Why	        :: error_term().
```


是说，当我们打开文件 `FileName` 时，我们将得到一个要么是 `{ok，Handle}` 或 `{error，Why}` 的返回值。`FileName` 是个字符串，`Modes` 是个 `Mode` 的列表，而 `Mode` 则是 `read`、`write` 等之一。


上面的函数规范，可以有多种等价写法，例如，我们可能写成下面这样，而不使用 `when` 的限定符：


```erlang
-spec file:open(string(), [read|write|...]) -> {ok, Handle} | {error, Why}
```



这种写法的问题是，首先，我们失去了 `FileName` 和 `Modes` 等描述性变量等；其次，类型规范会变得更长，而因此在印刷文档中更难阅读和格式化。当函数参数没有命名时，我们就无法在理想情况下程序后的文档中，引用这些参数。


在第一种规范编写方式中，我们写下了以下内容：


```erlang
-spec file:open(FileName, Modes) -> {ok, Handle} | {error, Why} When
    FileName	:: string(),
    ...
```


因此，这个函数的全部文档，都可以 `FileName` 这个名称，明确指代正在打开的文件。而当我们写下这个：


```erlang
-spec file:open(string(), [read|write|...]) -> {ok, Handle} | {error, Why}
```

而丢弃 `when` 这个限定符时，那么文档就不得不将正在打开的文件，称为 "`open` 函数的第一个参数"，这是第一种函数规范写法中，不必要的省略。


类型变量可用于参数，如下面的示例：


```erlang
-spec lists:map(fun(A) -> B, [A]) -> [B].
-spec lists:filters(fun(X) -> bool(), [X]) -> [X].
```


这表示 `map` 会取一个从类型 `A` 到类型 `B` 函数，以及一个类型 `A` 对象的列表，并返回一个类型 `B` 对象的列表，以此类推。



### 导出与本地类型


有时我们希望某个类型的定义，属于定义他的模组本地；而在别的情况下，我们则打算导出该类型到另一模组。设想有两个模组 `a` 和 `b`，模组 `a` 生成了一些 `rich_text` 类型的对象，模组 `b` 会操作这些对象。在模组 `a` 中，我们构造了以下一些注解：


```erlang
-module(a).
-type rich_text() :: [{font(), char()}].
-type font()	  :: integer().
-export_type([rich_text/0, font/0]).
```


我们不仅要声明 `rich_text` 和 `font` 类型，我们还要使用一个 `-export_type(...)` 注解，导出他们。


设想模组 `b` 会操作一些 `rich_text` 的实例；其中可能存在某个计算 `rich_text` 对象长度的函数 `rich_text_length`。我们可将这个函数的类型规范，写作如下：


```erlang
-module(b).
...
-spec rich_text_length(a:rich_text()) -> integer().
...
```

`rich_text_length` 的输入参数，使用了完全限定的类型名称 `a:rich_text()`，即导出自模组 `a` 的类型 `rich_text()`。


### 不透明类型


在上一小节中，两个模组 `a` 和 `b` 以操作表示富文本的对象内部结构，相互合作。不过，我们可能希望隐藏这个富文本数据结构的内部细节，这样只有创建这个数据结构的模组，才知道该类型的细节。这最好一个示例，加以解释。


假定模组 `a` 像下面这样开头：


```erlang
-module(a).
-opaque rich_text() :: [{font(), char()}].
-export_type([rich_text/0]).

-export([make_text/1, bounding_box/1]).
-spec make_text(string()) -> rich_text().
-spec bounding_box(rich_text()) -> {Height::integer(), Width::integer()}.
...
```

下面这个语句：


```erlang
-opaque rich_text() :: [{font(), char()}].
```


创建了个名为 `rich_text()` 的不透明类型。现在我们来看看一些尝试操作 `rich_text()` 对象的代码：


```erlang
-module(b).
...

do_this() ->
    X = a:make_text("hello world"),
    {W, H} = a:bounding_box(X)
```


模组 `b` 永远不需要知道变量 `X` 的内部结构。`X` 是在模组 `a` 内创建的，并在我们调用 `bounding_box(X)` 时，`X` 又被传回 `a`。


现在，设想我们要编写会用到一些有关 `rich_text` 对象形状知识的代码。例如，设想我们要创建一个 `rich_text()` 对象，然后询问渲染该对象需要哪些字体。我们可能写下这段代码：


```erlang
-module(c).
...

fonts_in(Str) ->
    X = a:make_text(Str),
    [F || {F,_} <- X].
```


在列表综合下，我们 “知道” `X` 是个 2 元元组的列表。在模组 `a` 中，我们曾将 `make_text` 的返回类型,声明为一个不透明类型，这意味着我们不应知道该类型内部结构的任何情况。当我们在有关函数中，正确地声明了类型的可见性时，利用类型的内部结构，就被称为一次 *抽象背离*<sup>1</sup>，同时这会被 `dialyzer` 检测到。


> **译注**：
>
> <sup>1</sup>：an *abstraction violation*，一次抽象背离
>
> 参考：[Violating Data Abstraction](https://berkeley-cs61as.github.io/textbook/violating-data-abstraction.html)


## 与 `dialyzer` 的一次交谈

首次运行 `dialyzer` 时，咱们需要构建出标准库中，咱们打算用到的所有类型的缓存。这是个一次性操作。当咱们启动 `dialyzer` 时，他会告诉咱们该做什么。


```console
$ dialyzer
  Checking whether the PLT c:/Users/ZBT7RX/AppData/Local/erlang/Cache/.dialyzer_plt is up-to-date...
dialyzer: Could not find the PLT: c:/Users/ZBT7RX/AppData/Local/erlang/Cache/.dialyzer_plt
Use the options:
   --build_plt   to build a new PLT; or
   --add_to_plt  to add to an existing PLT

For example, use a command like the following:
   dialyzer --build_plt --apps erts kernel stdlib mnesia
Note that building a PLT such as the above may take 20 mins or so

If you later need information about other applications, say crypto,
you can extend the PLT by the command:
  dialyzer --add_to_plt --apps crypto
For applications that are not in Erlang/OTP use an absolute file name.

```


PLT 是 *持久查找表，persistent lookup table* 的简称。PLT 应包含标准系统中所有类型的缓存。构建 PLT 需要几分钟时间。我们下达的第一条命令，会构建出 `erts`、`stdlib` 及 `kernel` 的 PLT。


```console
$ dialyzer --build_plt --apps erts kernel stdlib compiler crypto syntax_tools parsetools
  Creating PLT /home/hector/.cache/erlang/.dialyzer_plt ... done in 0m25.10s
done (passed successfully)
```


> **译注** 其中 `parsetools` 对应 YECC -- Erlang 的解析器生成器，是 `parsetools` 应用的一部分；`syntax_tools` 选项对应 `erlang_syntax`； `crypto` 对应 `crypto`；`compiler` 对应 `compile`。若没有后面 4 个选项，输出如下所示。



```console
$ dialyzer --build_plt --apps erts kernel stdlib
  Creating PLT /home/hector/.cache/erlang/.dialyzer_plt ...
Unknown functions:
  compile:file/2 (c.erl:509:10)
  compile:forms/2 (escript.erl:803:12)
  compile:noenv_forms/2 (erl_abstract_code.erl:34:9)
  compile:noenv_forms/2 (qlc_pt.erl:455:14)
  compile:output_generated/1 (c.erl:568:10)
  crypto:crypto_one_time/5 (beam_lib.erl:1411:11)
  crypto:hash_info/1 (inet_dns_tsig.erl:184:31)
  crypto:mac_finalN/2 (inet_dns_tsig.erl:317:5)
  crypto:mac_init/3 (inet_dns_tsig.erl:276:16)
  crypto:mac_update/2 (inet_dns_tsig.erl:303:16)
  crypto:strong_rand_bytes/1 (net_kernel.erl:2632:37)
  erl_syntax:map_field_assoc_name/1 (shell_docs_test.erl:386:24)
  erl_syntax:map_field_assoc_value/1 (shell_docs_test.erl:387:25)
  erl_syntax:map_field_exact/2 (shell_docs_test.erl:388:17)
  erl_syntax:revert/1 (shell_docs_test.erl:382:5)
  erl_syntax:type/1 (shell_docs_test.erl:384:14)
  erl_syntax_lib:map/2 (shell_docs_test.erl:383:5)
Unknown types:
  compile:option/0 (c.erl:149:19)
  compile:option/0 (erl_expand_records.erl:56:26)
  compile:option/0 (erl_lint.erl:100:47)
  compile:option/0 (qlc.erl:746:32)
  compile:option/0 (qlc_pt.erl:78:32)
  crypto:mac_state/0 (inet_dns_tsig.erl:71:48)
  yecc:option/0 (c.erl:1411:23)
  yecc:yecc_ret/0 (c.erl:1411:57)
 done in 0m14.62s
done (warnings were emitted)
```


现在我们已构建好 PLT，那么就可以运行 `dialyzer` 了。之所以会出现未知函数的告警，是因为提及的那些函数，不在我们选择分析的三个应用中。


`dialyzer` 是 *保守的*。当他抱怨时，那么程序中必然有不合理之处。构造 `dialyzer` 的项目目标之一，就是要消除虚假警告消息，即那些并非属于真正错误的警告消息。


在后面的小节中，我们给出一些不当程序的示例；我们将对这些程序运行 `dialyzer`，并说明我们可依赖 `dialyzer` 报告哪些错误。



### BIF 返回值的不当使用

[`dialyzer/test1.erl`](http://media.pragprog.com/titles/jaerlang2/code/dialyzer/test1.erl)



```erlang
-module(test1).
-export([f1/0]).


f1() ->
    X = erlang:time(),
    seconds(X).


seconds({_Year, _Month, _Day, Hour, Min, Sec}) ->
    (Hour * 60 + Min)*60 + Sec.
```

```console
$ dialyzer test1.erl
  Checking whether the PLT /home/hector/.cache/erlang/.dialyzer_plt is up-to-date... yes
  Proceeding with analysis...
test1.erl:5:1: Function f1/0 has no local return
test1.erl:7:13: The call test1:seconds
         (X :: {byte(), byte(), byte()}) will never return since it differs in the 1st argument from the success typing arguments:
         ({_, _, _, number(), number(), number()})
test1.erl:10:1: Function seconds/1 has no local return
test1.erl:10:1: The pattern
          {_Year, _Month, _Day, Hour, Min, Sec} can never match the type
          {byte(), byte(), byte()}
 done in 0m0.16s
done (warnings were emitted)
```


这个相当可怕的错误消息，是由于 `erlang:time()` 返回的是个名为 `{Hour, Min, Sec}` 的 3 元组，而不是我们所期望的 6 元组。“函数 `f1/0` 不会有本地返回值” 这个消息，意味着 `f1/0` 将崩溃。`dialyzer` 知道 `erlang:time()` 的返回值是 `{non_neg_integer(), non_neg_integer(), non_neg_integer()}` 类型的实例，因此绝不会与其中的 6 元组模式，也就是 `seconds/1` 的参数匹配。


### 某个 BIF 的不当参数


当我们以不当参数，调用某个 BIF 时，我们可使用 `dialyzer` 告诉我们 。下面是这方面的一个示例：


[`dialyzer/test2.erl`](http://media.pragprog.com/titles/jaerlang2/code/dialyzer/test2.erl)

```erlang
-module(test2).
-export([f1/0]).


f1() ->
    tuple_size(list_to_tuple({a, b, c})).
```

```console
$ dialyzer test2.erl
  Checking whether the PLT /home/hector/.cache/erlang/.dialyzer_plt is up-to-date... yes
  Proceeding with analysis...
test2.erl:5:1: Function f1/0 has no local return
test2.erl:6:30: The call erlang:list_to_tuple
         ({'a', 'b', 'c'}) breaks the contract
          (List) -> tuple() when List :: [term()]
 done in 0m0.16s
done (warnings were emitted)
```



