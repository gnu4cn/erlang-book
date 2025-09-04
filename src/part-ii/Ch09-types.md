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



