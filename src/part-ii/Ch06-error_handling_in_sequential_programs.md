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
