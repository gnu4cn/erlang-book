# 顺序程序中的错误处理


Erlang 最初是为编程容错系统设计的，这些系统原则上应永远不会停止运行。这意味着处理运行时错误至关重要。我们非常重视 Erlang 下的错误处理。当错误发生时，我们需要检测、纠正他们，并继续运行。


典型的 Erlang 应用，是由数十到数百万个并发进程组成。有着大量进程，改变了我们对错误处理的看法。在只有一个进程的顺序语言中，这个进程不会崩溃至关重要。而当我们有着大量进程时，那么某个进程崩溃就不那么重要了，只要某个其他进程能检测到这次崩溃，并接管该崩溃进程本应做的事情。

要构建出真正容错的系统，我们需要不止一台计算机；毕竟，整个计算机都可能崩溃。因此，检测故障并在其他地方恢复计算的想法，必须扩展到联网计算机。


要完全理解错误处理，我们首先需要了解顺序程序中的错误处理，然后在理解了这点后，再了解在并行进程集下的错误处理。本章讨论了前一个问题。并行进程下的错误处理，会在 [第 13 章 “并发程序中的错误”](../part-iii/Ch13-errors_in_concurrent_programs.md) 中讨论，而构建协作纠错的进程集则是 [23.5 节 “监督树”](../src/part-iv/Ch23-making_a_system_with_OTP.md#监督树) 的主题。


## 顺序代码中的错误处理


每次我们在 Erlang 下调用某个函数时，都会发生两种情况中的一种：要么该函数返回一个值，要么出错。在上一章中我们曾看到这方面的示例。还记得那个 `cost` 函数吗？


[`shop.erl`](http://media.pragprog.com/titles/jaerlang2/code/shop.erl)


```erlang
cost(oranges) -> 5;
cost(newspaper) -> 8;
cost(apples) -> 2;
cost(pears) -> 9;
cost(milk) -> 7.
```


下面时当我们之前运行他时，发生的事情：


```erlang
1> shop:cost(apples).
2
2> shop:cost(socks).
** exception error: no function clause matching shop:cost(socks) (shop.erl:4)
```


当我们调用 `cost(socks)` 时，这个函数崩溃了。出现这种情况的原因是，定义该函数的子句中，没有都与调用参数匹配的子句。


调用 `cost(socks)` 纯粹是无稽之谈。由于袜子价格未被定义，因此该函数无法返回合理值。在这种情况下，系统非但不会返回值，反而会 *抛出异常* -- 这就是 “崩溃” 的专业术语。


我们并未尝试修复这个错误，因为这是不可能的。我们不知道袜子要用多少钱，因此就无法返回值。在该函数崩溃时，应该由 `cost(socks)` 的 *调用者*，决定如何处理。


异常应由系统在发生内部错误时抛出，或通过调用 `throw(Exception)`、`exit(Exception)` 或 `error(Exception)` 在代码中显式抛出。当我们计算 `cost(socks)` 时，就出现了个模式匹配的错误。因为没有定义袜子费用的子句，所以系统自动生成了个错误。


抛出异常的典型内部错误，是一些模式匹配错误（函数中没有匹配的子句），或以不正确的参数类型调用 BIFs（例如，以一个整数的参数调用 `atom_to_list`），或以不正确的参数值调用某个 BIF（例如，试图将某个数字除以零）。

*注意*：许多语言都声称，咱们应使用 *防御性编程* 并检查所有函数的参数。在 Erlang 中，防御性编程是内建的。咱们应仅针对有效输入参数，描述函数行为；所有其他参数，都会导致会被自动检测到的内部错误。在以无效参数调某个用函数时，咱们绝不应返回值。咱们应始终抛出一个异常。这条规则就叫 “让其崩溃” 原则。

通过调用以下的 BIFs，我们可显式生成一个错误：


- `exit(Why)`

    当咱们真的打算终止当前进程时，就要用到这个 BIF。在本次异常未被捕获时，那么信号 `{'EXIT',Pid,Why}` 就将广播到所有与当前进程有链接的那些进程。我们还没有接触过信号，但在 [13.3 小节 “创建链接”](../src/part-iii/Ch13-errors_in_concurrent_programs.md#创建链接) 中，我们将详细介绍这点。信号和错误信息差不多，但我（作者）不会在此赘述。


- `throw(Why)`

    这用于抛出一个调用者可能希望捕获的异常。在这种情况下，我们会 *写明* 我们的函数可能会抛出这个异常。该函数的用户有两种选择：咱们可对常见情况编程，并完全忽略异常；或者咱们可将调用包含在一个 `try...catch` 表达式中，然后处理错误。

- `error(Why)`


    这用于表示 “崩溃的错误”。也就是说，发生了一些调用者无法处理的令人讨厌的错误。这与内部产生的错误类似。

Erlang 有两种捕获异常的方式。一种是将到函数的调用包含在一个 `try...catch` 表达式中。另一种是将调用包含在一个 `catch` 表达式中。



## 使用 `try...catch` 捕获异常


若咱们熟悉 Java，那么理解这个 `try...catch` 表达式不会有任何困难。Java 可使用以下语法捕获异常：


```java
try {
    block
} catch (exception type identifier) {
    block
} catch (exception type identifier) {
    block
} ...
finally {
    block
}
```


Erlang 有着非常相似的结构，如下所示：


```erlang
try FuncOrExpressionSeq of
    Pattern1 [when Guard1] -> Expressions1;
    Pattern2 [when Guard2] -> Expressions2;
    ...
catch
    ExceptionType1: ExPattern1 [when ExGuard1] -> ExExpressions1;
    ExceptionType2: ExPattern2 [when ExGuard2] -> ExExpressions2;
    ...
after
    AfterExpressions
end
```


### `try...catch` 有个值


请记住，Erlang 下的一切都是表达式，而所有表达式都有值。早先在 [`if` 表达式](Ch04-modules_and_functions.md#if-表达式) 处，讨论为何 `if` 表达式没有 `else` 部分时，我们就谈到过这个问题。这意味着表达式 `try...end` 也有一个值。因此，我们可以写出如下的代码：


```erlang
f(...) ->
...
X = try ... end,
Y = g(X),
...
```

而更常见的情况是，我们并不需要 `try...catch` 表达式的值。因此，我们只要写出下面的：


```erlang
f(...) ->
    ...
    try ... end,
    ...
    ...
```


请注意 `try...catch` 表达式与 `case` 表达式之间的相似性。


```erlang
case Expression of
    Pattern1 [when Guard1] -> Expressions1;
    Pattern2 [when Guard2] -> Expressions2;
    ...
end
```


`try...catch` 就像是打了类固醇的 `case` 表达式。他基本上是个末尾带有 `catch` 和 `after` 块的 `case` 表达式。

`try...catch` 的工作原理如下：首先 `FuncOrExpessionSeq` 会被求值。在这步以没有引发异常结束时，那么该函数的返回值将与模式 `Pattern1`（在可选条件 `Guard1` 下）、`Pattern2` 等模式匹配，直到某个匹配找到为止。在匹配找到时，则通过计算这个匹配模式后的表达式序列，找到整个 `try...catch` 表达式的值。


当 `FuncOrExpressionSeq` 内部某个异常被抛出时，捕获模式 `ExPattern1` 等就会被匹配，以找到应求值的那个表达式序列。`ExceptionType` 是个原子（`throw`、`exit` 或 `error` 之一），他会告诉我们异常是如何产生的。在 `ExceptionType` 被省略时，则默认值为 `throw`。


*注意*：由 Erlang 运行时系统检测到的内部错误，总是有着 `error` 这个标签。


`after` 关键字后面的代码，被用于 `FuncOrExpressionSeq` 后的清理工作。即使某个异常被抛出，这些代码也保证会被执行。`after` 小节中的代码，会该表达式的 `try` 或 `catch` 小节下 `Expressions` 中代码之后立即运行。`AfterExpressions` 的返回值将丢失。


若咱们是从 Ruby 而来，那么所有这写都应该非常熟悉。在 Ruby 下，我们会写出类似模式。


```ruby
begin
    ...
rescue
    ...
ensure
    ...
end
```


关键字不同，但行为相似。


### 简写


我们可以省略 `try...catch` 表达式的几个部分。下面这段代码：


```erlang
try F
catch
    ...
end
```

意思与下面这段代码一样：


```erlang
try F of
    Val -> Val
catch
    ...
end
```

同样，`after` 小节也可被省略。


### `try...catch` 下的编程习语


在设计应用时，我们通常会确保捕捉某个错误的代码，能够捕捉到某个函数可能产生的所有错误。


下面是演示这点的两个函数。第一个函数会产生某个异常的三种不同类型，并有两个普通的返回值。


```erlang
generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> error(a).
```


现在，我们将编写一个在 `try...catch` 表达式中调用 `generate_exception` 的封装函数。


```erlang
demo1() ->
    [catcher(I) || I <- [1,2,3,4,5]].

catcher(N) ->
    try generate_exception(N) of
        Val -> {N, normal, Val}
    catch
        throw:X -> {N, caught, thrown, X};
        exit:X -> {N, caught, exited, X};
        error:X -> {N, caught, error, X}
    end.
```


运行这个程序我们就会得到以下内容：


```erlang
5> try_test:demo1().
[{1,normal,a},
 {2,caught,thrown,a},
 {3,caught,exited,a},
 {4,normal,{'EXIT',a}},
 {5,caught,error,a}]
```


这表明我们可以捕获并区分出，某个函数可抛出的所有异常形式。

## 使用 `catch` 捕获异常


捕获异常的另一种方法，是使用原语 `catch`。`catch` 这个原语，不同于 `try...catch` 语句中的 `catch` 块（这是早在 try...catch 引入前，`catch` 语句就曾是这门语言的一部分）。


当 `catch` 语句内部某个异常发生时，他会被转换为一个描述该错误的 `{`EXIT`, ...}` 元组。为演示这点，我们可在一个 `catch` 表达式中，调用 `generate_exception`。


[`try_test.erl`](http://media.pragprog.com/titles/jaerlang2/code/try_test.erl)

运行这段代码，我们会得到以下内容：

```erlang
7> try_test:demo2().
[{1,a},
 {2,a},
 {3,{'EXIT',a}},
 {4,{'EXIT',a}},
 {5,
  {'EXIT',{a,[{try_test,generate_exception,1,
                        [{file,"try_test.erl"},{line,11}]},
              {try_test,'-demo2/0-lc$^0/1-0-',1,
                        [{file,"try_test.erl"},{line,18}]},
              {try_test,'-demo2/0-lc$^0/1-0-',1,
                        [{file,"try_test.erl"},{line,18}]},
              {erl_eval,do_apply,7,[{file,"erl_eval.erl"},{line,924}]},
              {shell,exprs,7,[{file,"shell.erl"},{line,937}]},
              {shell,eval_exprs,7,[{file,"shell.erl"},{line,893}]},
              {shell,eval_loop,4,[{file,"shell.erl"},{line,878}]}]}}}]
```


若咱们将其与 `try...catch` 小节的输出进行比较，就会发现两种方式提供不同数量的调试信息。第一种方式总结了信息。而第二种方式则提供了详细的栈跟踪信息。



## 异常下的编程风格


异常处理并不难（不是一门火箭科学）；后面的章节包含了一些我们可以在程序中重复使用的，经常出现的代码模式。


### 改进错误消息

`error/1` 这个BIF 的一个用途，就是改进错误消息质量。当我们以负的参数调用 `math:sqrt(X)` 时，我们会看到如下输出：


```erlang
1> math:sqrt(-1).
** exception error: an error occurred when evaluating an arithmetic expression
     in function  math:sqrt/1
        called as math:sqrt(-1)
        *** argument 1: is outside the domain for this function
```


我们可为此编写个改进错误消息的封装函数。


[`lib_misc.erl`](http://media.pragprog.com/titles/jaerlang2/code/lib_misc.erl)


```erlang
sqrt(X) when X < 0 ->
    error({squareRootNegativeArgument, X});
sqrt(X) ->
    math:sqrt(X).
```


```erlang
2> lib_misc:sqrt(-1).
** exception error: {squareRootNegativeArgument,-1}
     in function  lib_misc:sqrt/1 (lib_misc.erl:83)
```


### 其中错误返回属于常见的代码

当咱们的函数真的缺少某种 “常见情况” 时，咱们可能应返回类似 `{ok, Value}` 或 `{error, Reason}` 的值，但请记住，这样做会迫使所有调用者，对返回值完成 *某种操作*。这样，咱们就必须在两种情况中做出选择；要么写成这样：


```erlang
...
case f(X) of
    {ok, Val} ->
        do_some_thing_with(Val);

    {error, Why} ->
        %% ... do something with the error ...
end,
...
```


这兼顾到了两种返回值，或者写成这样：


```erlang
...
{ok, Val} = f(X),
do_some_thing_with(Val);
...
```

这会在 `f(X)` 返回 `{error, ...}` 时抛出一个异常。



### 其中错误有可能但很少发生的代码


通常情况下，咱们应编写出预期要处理错误的代码，如本例所示：

```erlang
try my_func(X)
catch
    throw:{thisError, X} -> ...
    throw:{someOtherError, X} -> ...
end
```


同时侦测错误的代码应如有着如下一些匹配的 `throws`：


```erlang
my_func(X) ->
    case ... of
        ...
        ... ->
                ... throw({thisError, ...})
        ... ->
                ... throw({someOtherError, ...})
```


### 捕获所有可能的异常


当我们打算捕捉所有可能的错误时，我们可使用下面的习语（他运用了 `_` 可匹配任何东西这一事实）：


```erlang
try Expr
catch
    _:_ -> ... Code to handle all exceptions ...
end
```


而当我们省略了标签，并写出这段代码时：


```erlang
try Expr
catch
    _ -> ... Code to handle all exceptions ...
end
```


那么我们就 *不会* 捕捉到所有错误，因为在这种情况下，我们假定了默认标记 `throw`。


## 栈追踪


当某个异常被捕获到时，通过调用 `erlang:get_stacktrace()`，我们可发现最新的堆栈跟踪。下面是个示例：


```erlang
demo3() ->
    try generate_exception(5)
    catch
        error:X ->
            {X, Class::Error:Stacktrace}
    end.
```

> **译注**：`erlang:get_stacktrace/0` 这个函数已在 Erlang OTP/21 种启用。因此在编写这段代码时，Vim 的 Erlang 插件会给出 `erlang:get_stacktrace/0 is remove; use the new try/catch syntax for retrieving  the stack backtrace` 警告。新的写法如下：


```erlang
demo3() ->
    try generate_exception(5)
    catch
        C:E:S -> {C, E, S}
    end.
```

> 参考：[在 OTP 21 編譯 Zotonic](https://medium.com/@yauhsienhuang/%E5%9C%A8-otp-21-%E7%B7%A8%E8%AD%AF-zotonic-fc247236ea78)



```erlang
1> try_test:demo3().
{error,a,
       [{try_test,generate_exception,1,
                  [{file,"try_test.erl"},{line,12}]},
        {try_test,demo3,0,[{file,"try_test.erl"},{line,32}]},
        {erl_eval,do_apply,7,[{file,"erl_eval.erl"},{line,924}]},
        {shell,exprs,7,[{file,"shell.erl"},{line,937}]},
        {shell,eval_exprs,7,[{file,"shell.erl"},{line,893}]},
        {shell,eval_loop,4,[{file,"shell.erl"},{line,878}]}]}
```

上面的跟踪，显示了在我们尝试计算 `try_test:demo3()` 时发生的情况。他显示我们的程序在函数 `generate_exception/1` 中崩溃了，该函数定义在文件 `try_test.erl` 的第 12 行处。


该堆栈跟踪包含了当前函数（崩溃的那个）成功后，会返回到何处的信息。栈跟踪中单个元组的格式为 `{Mod,Func,Arity,Info}`。其中 `Mod`、`Func` 和 `Arity` 表示某个函数，而 `Info` 包含了该栈跟踪中，项目的文件名和行号。


因此，`try_test:generate_exception/1` 将返回到 `try_test:demo3()`，而 `try_test:demo3()` 将返回到 `erl_eval:do_apply/6`，以此类推。当某个函数是在某个表达式序列中间被调用时，那么调用处与函数将返回的位置，会几乎相同。而当被调用函数是表达式序列中的最后那个函数时，那么栈上就不会保留该函数于何处被调用的信息。Erlang 对此类代码应用了一种最后调用的优化，因此堆栈跟踪不会记录该函数被调用的位置，而只会记录其将返回的位置。


检查栈跟踪会给到我们在错误发生时，程序执行位置的良好指标。通常情况下，栈跟踪的前两个条目，会给到咱们定位错误发生于何处的足够信息。


现在我们了解了顺序程序中的错误处理。要记住的重要一点，是 *让其崩溃*。当某个函数被以某个不正确参数调用时，千万不要返回值；而要抛出一个异常。假设调用者会修复这个错误。


> *知识点*：
>
>- a last-call optimization
>- let it crash


## 快速喧闹地失败，抑或礼貌地失败


在为出错编码时，我们需要考虑两个关键原则。首先，在错误发生时，我们应立即宣告失败，同时大声地宣告失败。有数种编程语言采用了悄无声息失败原则，而试图修复错误并继续运行；这会导致代码成为调试的噩梦。在 Erlang 中，当某个错误被系统于内部检测到，或由程序逻辑检测到时，正确方式是立即崩溃并生成有意义的错误消息。我们立即崩溃，是为了避免事情变得更糟。错误消息应写入某种永久的错误日志，并且要足够详细，以便我们在稍后可找出什么东西出错了。

其次，“礼貌地失败” 是指只有程序员才应看到在程序崩溃时，所产生的详细错误消息。程序的用户永远不应该看到这些消息。另一方面，用户应被通知某个错误已发生的事实，并被告知他们可以采取什么措施纠正这个错误。


对程序员来说，错误消息是难能可贵、不可多得的。他们绝不应在屏幕上滚动一下然后永远消失。他们应去到可在随后阅读的某个永久日志文件。


> **译注**，原文，error messages are gold dust for programmers.


到此为止，我们只介绍了顺序程序中的错误。在 [第 13 章 “并发程序中的错误”](../part-iii/Ch13-errors_in_concurrent_programs.md) 中，我们将了解在并发程序中的如何管理错误，并在 [23.2 小节 “错误记录器”](../part-iv/Ch23-making_a_system_with_otp.md#错误记录器) 中，我们将了解如何永久记录错误，以免丢失。


下一章种，我们将学习二进制值与位语法。位语法是 Erlang 独有的，将模式匹配扩展到位的字段，其简化了操作二进制数据程序的编写。


## 练习

1. `file:read_file(File)` 会返回 `{ok, Bin}` 或 `{error, Why}`，其中 `File` 是文件名，`Bin` 包含了该文件的内容。请编写一个在可以读取文件时返回 `Bin`，在无法读取文件时抛出一个异常的函数 `myfile:read(File)`；


```erlang
read(File) ->
    try file:read_file(File) of
        Val -> Val
    catch
        E:X -> { E, X }
    end.
```

2. 请重写 `try_test.erl` 中的代码，使其产生两条错误消息：给用户的礼貌消息，和给开发者的详细消息。
