# 模组与函数


模组与函数，是构建顺序程序及并行程序的基本单元。模组包含了函数，而函数可以顺序运行，也可以并行运行。


本章建立在上一章中模式匹配思想基础上，介了我们编写代码所需的所有控制语句。我们将讨论高阶函数（称为 `funs`），以及如何使用他们，创建咱们自己的控制抽象。此外，我们还将讨论


- 列表综合
- 守护
- 记录
- 及 `case` 表达式

并展示如何在一些小的代码片段中使用他们。


让我们开始把。


*知识点*：

- higher-order functions, `funs`
- list comprehensions
- guards
- records
- `case` expressions


## 模组乃我们存储代码之处

模组是 Erlang 代码的基本单元。模组包含在扩展名 `.erl` 的文件中，必须编译后模组中的代码才能运行。编译后的模组扩展名为 `.beam`。


在我们编写咱们的首个模组前，我们先来了解一下模式匹配。我们要做的就是，创建两个分别表示矩形和正方形的数据结构。然后，我们将解包这两个数据结构，并提取出矩形和正方形中的边。具体方法如下：


```erlang
1> Rectangle = {rectangle, 10, 5}.
{rectangle,10,5}
2> Square = {square, 3}.
{square,3}
3> {rectangle, Width, Height} = Rectangle.
{rectangle,10,5}
4> Width.
10
5> Height.
5
6> {square, Side} = Square.
{square,3}
7> Side.
3
```

在第 1 和第 2 行中，我们创建了一个矩形和一个正方形。在第 3 和第 6 行中，我们使用模式匹配，解包了这个矩形和正方形的字段。在第 4、5 和 7 行中，我们打印了通过模式匹配表达式，创建出的变量绑定。第 7 行后，shell 中的变量绑定为 `Width = 10`、`Height = 5` 及 `Side = 3`。


从 shell 中的模式匹配，到函数中的模式匹配，是极小的一步。我们来从一个名为 `area`，计算矩形和正方形面积的函数开始。我们将把这个函数，放在一个名为 `geometry` 的模组中，并将该模组存储在名为 `geometry.erl` 的文件中。整个模组看起来是这样的:


[`geometry.erl`](http://media.pragprog.com/titles/jaerlang2/code/geometry.erl)



```erlang
-module(geometry).
-export([area/1]).

area({rectangle, Width, Height}) -> Width * Height;
area({square, Side})			 ->	Side * Side.
```

该文件的第一行，是个 *模组声明*。声明中的模组名字，必须与模组所在文件的基本名称相同。

第二行是个 *导出声明*。`Name/N` 这种写法，表示一个名为 `Name`，带有 `N` 个参数的函数；`N` 称为该函数的 *元数*。`export` 的参数，是个 `Name/N` 项的列表。因此，`-export([area/1])` 就表示，可以从此模组外部调用有着一个参数的函数 `area`。


*知识点*：

- module declaration
- export declaration
- the arity of the function, [Wikipedia: Arity](https://en.wikipedia.org/wiki/Arity)


模组中导出的函数，只能在模组内调用。导出的函数等同于面向对象编程语言（OOPL）中的公共方法；未导出的函数则相当于 OOPL 中的私有方法。


上面的函数 `area` 由两个 *子句* 组成。子句间以分号隔开，最后子句以点空白结束。每个子句都有个 *头* 和一个 *躯干*，由箭头 (`->`) 分隔。头部由函数名称，及零个或多个模式组成，躯干由一个表达式序列组成（表达式在 [8.13 小节 表达式与表达式序列](Ch08-the_rest_of_sequential_erlang.md#表达式与表达式序列)），在头部中模式与调用参数成功匹配时，该表达式序列即会被计算求值。各子句会以其在函数定义中出现顺序，逐一尝试。



请注意，我们曾在 shell 示例中用到的模式，已成为这个 `area` 函数定义的一部分。每个模式准确对应了一个子句。`area` 函数的第一个子句：


```erlang
area({rectangle, Width, Height}) -> Width * Height;
```

告诉我们如何计算某个矩形的面积。当我们计算函数 `geometry:area({rectangle,10,5})` 时，`area/1` 下的第一个子句，就会以 `Width = 10` 及 `Height = 5` 匹配。在该次匹配后，箭头 `->` 后面的代码就会被求值。这就是 `Width * Height`，即 `10*5` 或 `50`。请注意，这个函数没有显式返回语句；该函数的返回值，就是子句躯干中，最后那个表达式的值。


现在我们将编译这个模组并运行他。


```erlang
1> c(geometry).
{ok,geometry}
2> geometry:area({rectangle,10,5}).
50
3> geometry:area({square,3}).
9
```


在第 1 行，我们下达了 `c(geometry)` 这个命令，其会编译文件 `geometry.erl` 中的代码。编译器返回了表示编译成功，以及这个 `geometry` 模组已被编译并加载的 `{ok,geometry}`。编译器将在当前目录下，创建出一个名为 `geometry.beam` 的目标代码模组。在第 2 和第 3 行中，我们调用了 `geometry` 这个模组中的函数。请注意，我们需要将模组名字与函数名字放在一起，以便准确识别到我们打算调用的函数。


### 常见错误


需要提醒的是：`c(geometry).`（前面用到）这样的命令，只在 shell 中有效，而不能放入模组中。有些读者将源码清单中的代码片段，错误地输入到 shell 中。这些都不是有效的 shell 命令，当咱们尝试这样做时，就将得到一些非常奇怪的错误消息。所以，请不要这样做。


当咱们不小心选择了与某个系统模组相冲突的模组名字时，那么在咱们编译该模组时，咱们将收到一条提示咱们无法加载某个位于粘滞目录中模组的奇怪消息。只要重命名这个模组，并删除咱们编译模组时，可能生成的 `.beam` 文件即可。


### 目录与代码路径


若咱们下载了本书中的代码示例，或想编写咱们自己的示例，咱们必须确保在咱们于 shell 中运行编译器时，处于正确目录中，这样系统才能找到咱们的文件。


Erlang shell 有许多查看和更改当前工作目录的内建命令。


- `pwd()` 会打印当前工作目录；
- `ls()` 会列出当前工作目录下的文件名字；
- `cd(Dir)` 会将当前工作目录，改变到 `Dir`。


### 将测试添加到咱们的代码


在此阶段，我们可将一些简单测试，添加到咱们的模组。我们来将该模组重命名为 `geometry1.erl`，并添加一些测试代码。


[`geometry1.erl`](http://media.pragprog.com/titles/jaerlang2/code/geometry1.erl)


```erlang
-module(geometry1).
-export([test/0, area/1]).


test() ->
    12 = area({rectangle, 3, 4}),
    144 = area({square, 12}),
    test_worked.

area({rectangle, Width, Height}) -> Width * Height;
area({square, Side})			 ->	Side * Side.
```


```erlang
1> c(geometry1).
{ok,geometry1}
2> geometry1:test().
test_worked
```


`12 = area({rectangle, 3, 4})` 这行代码，就是个测试。当 `area({rectangle, 3, 4})` 没有返回 `12` 时，这个模式匹配就会失败，而我们就会得到一条错误消息。在我们运行 `geometry1:test()`，并看到结果 `tests_worked` 时，我们就可以得出结论：`test/0` 函数主体中的所有测试都成功了。


> **译注**：若我们修改一下 `geometry1.erl` 中的代码如下：

```erlang
-module(geometry1).
-export([test/0, area/1]).


test() ->
    12 = area({rectangle, 3, 4}),
    144 = area({square, 12}),
    test_worked.

area({rectangle, Width, Height}) -> Width * Height * 2;
area({square, Side})			 ->	Side * Side.
```

> 测试就不会通过，报出如下错误：

```erlang
4> geometry1:test().
** exception error: no match of right hand side value 24
     in function  geometry1:test/0 (geometry1.erl:6)
```


在无需任何额外工具下，我们就能轻松添加测试，并进行测试驱动的开发。我们需要的只是模式匹配与 `=`。虽然这对于快速测试来说已经足够，但对生产代码来说，最好使用功能齐全的测试框架，比如通用测试框架或单元测试框架；详情请阅读 [Erlang 文档](http://www.erlang.org/doc) 中的测试部分。


*知识点*：

- clause
- the head of a function
- the body of a function
- the calling arguments
- the system modules
- a sticky directory
- test-driven development
- quick-and-dirty testing
- the common test framework
- the unit test framework


### 扩展这个程序


现在，设想我们打算通过把圆添加到我们的几何对象，扩展我们的程序。我们可以这样写：


```erlang
area({rectangle, Width, Height}) -> Width * Height;
area({square, Side})			 ->	Side * Side;
area({circle, Radius})	         -> 3.14159 * Radius * Radius.
```

或这样：


```erlang
area({rectangle, Width, Height}) -> Width * Height;
area({circle, Radius})	         -> 3.14159 * Radius * Radius;
area({square, Side})			 ->	Side * Side.
```


请注意在这个示例中，子句的顺序并不重要；无论子句如何排序，该程序的含义都是同样的。这是因为子句中的模式是互斥的。这使得编写及扩展程序变得非常容易 -- 我们只要添加更多的模式。不过，一般来说，子句顺序确实重要。当某个函数被输入时，子句将按照调用参数在文件中出现的顺序，进行模式匹配。

在继续后面内容前，咱们应该注意以下有关这个 `area` 函数书写方式的内容：


- 这个函数 `area` 由几个不同子句组成。在我们调用这个函数时，执行将从与调用参数匹配的首个子句开始；
- 我们的函数不会处理没有模式匹配的情形 -- 我们的程序会以一个运行时错误失败。这是故意的。这正是我们在 Erlang 下编程的方式。


> **译注**：在没有没有与调用参数匹配的模式时，报错如下所示：


```erlang
1> geometry:area({diamond, 12, 15}).
** exception error: no function clause matching geometry:area({diamond,12,15}) (geometry.erl:4)
```


许多编程语言，比如 C，的每个函数，都只有一个入口点。若我们用 C 编写这个程序，代码可能如下：


```c
enum ShapeType { Rectangle, Circle, Square };

struct Shape {
    enum ShapeType kind;

    union {
        struct { int width, height; } rectangleData;
        struct { int radius; }	      circleData;
        struct { int side;}	          squareData;
    } shapeData;
};


double area(struct Shape* s) {
    if( s->kind == Rectangle ) {
        int width, ht;
        width = s->shapeData.rectangleData.width;
        ht	  = s->shapeData.rectangleData.height;
        return width * ht;
    } else if ( s->kind == Circle ) {
      ...
```

这段 C 代码本质上对这个函数参数，执行了一次模式匹配操作，但程序员必须编写出模式匹配代码，并确保其正确无误。


在 Erlang 的等价代码中，我们只需编写出模式，Erlang 的编译器会生成为程序选取正确入口点的最佳模式匹配代码。


下面显示的是 Java 下的等价代码 <sup>1</sup>：


```java
abstract class Shape {
    abstract double area();
}

class Circle extends Shape {
    final double radius;
    Circle(double radius) { this.radius = radius; }
    double area() { return Math.PI * radius*radius; }
}

class Rectangle extends Shape {
    final double ht;
    final double width;
    Rectangle(double width, double height) {
        this.ht = height;
        this.width = width;
    }
    double area() { return width * ht; }
}

class Square extends Shape {
    final double side;
    Square(double side) {
        this.side = side;
    }
    double area() { return side * side; }
}
```

> 注 <sup>1<sup>：[http://java.sun.com/developer/Books/shiftintojava/page1.html](https://web.archive.org/web/*/http://java.sun.com/developer/Books/shiftintojava/page1.html)

若咱们将 Erlang 的代码与 Java 代码进行比较，就会发现在 Java 程序中，`area` 的代码位于三个位置。而在 Erlang 程序中，`area` 的所有代码都在同一个地方。


### 分号的位置


在离开这个 `geometry` 示例前，我们再看一下这段代码，这次要看的是标点符号。这次我们要仔细观察代码，找一下逗号、分号和句点的位置。


[`geometry.erl`](http://media.pragprog.com/titles/jaerlang2/code/geometry.erl)


```erlang
-module(geometry).
-export([area/1]).

area({rectangle, Width, Height}) -> Width * Height;
area({circle, Radius})	         -> 3.14159 * Radius * Radius;
area({square, Side})			 ->	Side * Side.
```


咱们将看到下面这些：

- *逗号*（`,`）分隔了函数调用、数据构造器及模式中的那些参数；
- *分号*（`;`）分隔了 *子句*。我们可以在多种上下文中找到子句，即函数定义与 `case`、`if`、`try...catch` 及 `receive` 表达式等中的子句；
- *句点*（`.`）（后跟空白符）分隔了 shell 中的整个函数与表达式。


有种简单的记忆方法 -- *想想英语*。句号分隔句子，分号分隔子句，而逗号则分隔了从句。逗号是个短距符号，分号是个中距符号，句号是个长距符号。


*知识点*：

- sentence
- clause
- subordinate clause
- short-range symbol
- medium-range symbol
- long-range symbol


每当我们看到表达式后有一组模式时，我们就会看到作为分隔符的分号。下面就是个示例：


```erlang
case f(...) of
    Pattern1 ->
        Expressions1;
    Pattern2 ->
        Expressions2;

    ...

    LastPattern ->
        LastExpression
end
```

请注意，最后的表达式（紧接 `end` 关键字前的那个）没有分号。


理论到此为止。我们来继续学习一些代码；稍后我们再讨论控制结构。


## 回到购物的示例


在 [“定义列表” 小节](Ch03-basic_concepts.md#定义列表) 中，我们有过这样一个购物清单：


```erlang
[{oranges,4},{newspaper,1},{apples,6},{pears,6},{milk,3}]
```


现在假设我们想要知道咱们购物的花费。为此，我们需要知道咱们购物清单中，每件物品要用多少钱。我们假设此信息是在一个名为 `shop` 的模组中计算出来的，其定义如下：


[`shop.erl`](http://media.pragprog.com/titles/jaerlang2/code/shop.erl)


```erlang
-module(shop).
-export([cost/1]).

cost(oranges) -> 5;
cost(newspaper) -> 8;
cost(apples) -> 2;
cost(pears) -> 9;
cost(milk) -> 7.
```


其中函数 `cost/1` 由五个子句组成。每个子句的头部，都包含一种模式（在此情形下是种非常简单的模式，只是个原子）。当我们计算 `shop:cost(X)` 时，系统就会尝试将 `X` 与这些子句中的每种模式匹配。在找到某个匹配时，`->` 箭头右边的代码就会被求值。


我们来测试一下这段代码。我们将在 Erlang shell 中编译并运行这个程序。


```erlang
1> c(shop).
{ok,shop}
2> shop:cost(apples).
2
3> shop:cost(oranges).
5
4> shop:cost(socks).
** exception error: no function clause matching shop:cost(socks) (shop.erl:4)
```
在第 1 行种，我们编译了 `shop.erl` 文件中的模组。在第 2 和第 3 行中，我们询问了 `apples` 和 `oranges` 的开支（结果 `2` 和 `5` 是以开支单位表示）。在第 4 行中，我们询问了 `socks` 的开支，不过没有任何子句匹配，因此我们得到一个模式匹配报错，系统打印了一条错误消息，其中包含着发生错误处的文件名及行号。


回到购物清单。假设我们有个这样的购物清单：

```erlang
1> Buy = [{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}].
[{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}]
```

并想要计算该列表中，所有项目的总价值。一种方法是如下定义一个函数 `shop1:total/1`：


[`shop1.erl`](http://media.pragprog.com/titles/jaerlang2/code/shop1.erl)


```erlang
-module(shop1).
-export([total/1]).

total([{What, N}|T]) -> shop:cost(What) * N + total(T);
total([]) -> 0.
```

我们来以这段代码实验一下：

```erlang
2> c(shop1).
{ok,shop1}
3> shop1:total([]).
0
```


这里返回了 `0`，因为 `total/1` 的第二个子句是 `total([]) -> 0`。


下面是个更复杂的查询：


```erlang
4> shop1:total([{milk,3}]).
21
```


这条命令原理如下。调用 `shop1:total([{milk,3}])` 以绑定 `What = milk`、`N = 3` 及 `T = []`，与以下子句匹配：


```erlang
total([{What, N}|T]) -> shop:cost(What) * N + total(T);
```


在此之下，该函数主题中的代码就会被求值，因此我们必须计算这个表达式。


```erlang
shop:cost(milk) * 3 + total([]);
```

`shop:cost(milk)` 为 `7`，而 `total([])` 为 `0`，因此最终返回值为 `21`。


我们可以一个更复杂的参数，测试这个函数（`shop1:total/1`）。

```erlang
5> shop1:total([{pears,6},{milk,3}]).
75
```

同样，第 5 行以绑定 `What = pears`、`N = 6` 及 `T = [{milk,3}]`，匹配了函数 `total/1` 的第一个子句。


```erlang
total([{What, N}|T]) -> shop:cost(What) * N + total(T);
```


变量 `What`、`N` 与 `T`，就被替换到该子句的主体中，而 `shop:cost(pears) * 6 + total([{milk,3}])` 被会被求值，结果变为 `9 * 6 + total([{milk,3}])`。

而我们之前已经计算出了 `total([{milk,3}])` 是 `21`，所以最终结果是 `9*6 + 21 = 75`。


最后：


```erlang
6> shop1:total(Buy).
123
```


在告别这个小节前，我们应更详细地了解一下这个函数 `total`。`total(L)` 的工作原理，是对参数 `L` 进行情况分析。有两种可能的情况；`L` 是个非空的列表，或者 `L` 是个空列表。我们为每种可能情形，分别编写了个子句，就像这样：


```erlang
total([Head|Tail]) ->
    some_function_of(Head) + total(Tail);
total([]) ->
    0.
```


在我们的例子中，`Head` 为模式 `{What,N}`。当第一个子句匹配到一个非空列表时，他会挑出该列表中的头部，对这个头部完成一些操作，然后调用自身处理该列表的尾部。在列表已缩减为空列表（`[]`）时，第二个子句就会匹配。


这个函数 `total/1` 实际上完成了两件不同事情。他查找了该列表中每个元素的价格，然后对将所有价格与所购买物品数量的乘积求和。我们可以将查找单个物品价值，与价值求和分开的方式，重写这个 `total` 函数。得到的代码将更加清晰易懂。为此，我们将编写两个名为 `sum` 和 `map` 的小的列表处理函数。要编写 `map`，我们必须引入 `funs` 的概念。之后，我们将在模组 `shop2.erl` 中，编写一个改进版的 `total` 函数，咱们可在 [4.4 节 “简单的列表处理”](#简单的列表处理) 末尾处，找到这个模组。


## `funs`：抽象的基本单元


Erlang 是门函数式编程语言。除开其他方面，这意味着函数可被用作其他函数的参数，且函数可返回函数。操作函数的函数，被称做 *高阶函数*，同时 Erlang 中表示函数的数据类型，称为 `fun`。

高阶函数是函数式编程语言的精髓--函数式程序不仅可以操作常规数据结构，还可以操作转换数据的函数。咱们一旦学会使用他们，就会爱上他们。今后我们将看到更多的高阶函数。

`funs` 可依以下方式使用：

- 对列表中的每个元素，执行同样操作。在这种情况下，我们将 `funs` 作为参数传递给诸如 `lists:map/2`、`lists:filter/2` 等函数。`funs` 的这种用法相当常见；
- 创建咱们自己的控制抽象。这种技术非常有用。例如，Erlang 没有 `for` 循环。但我们可以轻松创建我们自己的 `for` 循环。创建我们自己的控制抽象的好处,是我们可以让他们刚好做我们想做的事，而不是依赖预定义的一组，行为可能不完全符合我们要求的控制抽象；
- 实现诸如可重入解析代码、解析器组合器，或惰性求值器等物件。在这种情况下，我们会编写一些返回 `funs` 的函数。这是一种非常强大的技术，但可能造成难以调试的程序。


*知识点*：


- functional programming language
- higher-order function
- functions be used as arguments to functions
- functions returned by functions
- the data type which represents a function, `fun`
- control abstraction
- reentrant parsing code
- parser combinator
- lazy evaluators


`funs` 是一些 “匿名” 函数。之所以这么叫，是因为他们没有名字。在其他编程语言中，咱们可能看到他们被称为 *lambda 抽象*。我们来开始试验；首先，我们将定义一个 `fun`，并将其赋值给一个变量。

```erlang
1> Double = fun(X) -> 2*X end.
#Fun<erl_eval.42.113135111>
```


在我们定义某个 `fun` 时，Erlang shell 会打印 `#Fun<erl_eval.N.M>`，其中 `N` 和 `M` 是些奇怪数字。现在不用担心这个。

我们只能用 `fun` 做一件事，那就是将其应用到某个参数，就像这样：


```erlang
2> Double(2).
4
```


`funs` 可以有任意数量的参数。我们可以写个计算直角三角形斜边的函数，就像这样：


```erlang
3> Hypot = fun(X, Y) -> math:sqrt(X*X + Y*Y) end.
#Fun<erl_eval.41.113135111>
4> Hypot(3,4).
5.0
5> Hypot(5).
** exception error: interpreted function with arity 2 called with one argument
```

其中的错误信息告诉我们，`Hypot` 需要两个参数，而我们只提供了一个。请记住，`arity` 为某个函数接受参数的个数。


`funs` 可以有多个不同子句。下面是个在华氏温度和摄氏温度之间进行转换的函数：


```erlang
6> TempConvert = fun({c, C}) -> {f, 32 + C*9/5};
   ({f, F}) -> {c, (F-32)*5/9}
   end.
#Fun<erl_eval.42.113135111>
7> TempConvert({c,100}).
{f,212.0}
8> TempConvert({f,212}).
{c,100.0}
9> TempConvert({c,0}).
{f,32.0}
```

*注意*：第 6 行中的表达式跨了好几行。在我们输入这个表达式时，每输入一行，shell 就会重复提示 ` .. `。这意味着表达式不完整，shell 希望输入更多内容。


### 以函数作为其参数的函数


标准库中的 `lists` 模组，导出了数个参数为 `funs` 的函数。其中最有用的是 `lists:map(F,L)`。这是个通过将 `fun` `F`，应用于列表 `L` 中的每个元素，从而返回一个列表的函数。


```erlang
10> L = [1,2,3,4].
[1,2,3,4]
11> lists:map(fun(X) -> 2*X end, L).
[2,4,6,8]
```


另一有用函数，则是返回一个其中包含 `L` 中，`P(E)` 为 `true` 的所有元素新列表的 `lists:filter(P,L)`。


我们来定义一个在 `X` 是个偶数时为 `true` 的函数 `Even(X)`。


```erlang
12> Even = fun(X) -> (X rem 2) =:= 0 end.
#Fun<erl_eval.42.113135111>
```


这里 `X rem 2` 计算的是 `X` 除以 `2` 后的余数，而 `=:=` 则是相等测试。现在我们可以测试 `Even`，然后将其作为 `map` 与 `filter` 的参数。


```erlang
12> Even(8).
true
13> Even(7).
false
14> lists:map(Even, [1,2,3,4,5,6,8]).
[false,true,false,true,false,true,true]
15> lists:filter(Even, [1,2,3,4,5,6,8]).
[2,4,6,8]
```

我们将诸如 `map` 和 `filter` 这样的，在一次函数调用中，对整个列表执行一些处理的操作，称为 *list-at-a-time* 操作。使用这些  list-at-a-time 操作，可使我们的程序变得小巧易懂；他们之所以易懂，是因为我们可将对整个列表的每次操作，视为咱们程序中的单个概念性步骤。否则，我们就必须把对列表元素的单个操作，视为咱们程序中的单个步骤。


### 返回 `funs` 的函数


`funs` 不仅可用作函数（如 `map` 和 `filter`）的参数，而且函数也可以 *返回* `funs`。


下面是个示例 -- 假设我有个清单，比如水果：

```erlang
1> Fruit = [apple,pear,orange].
[apple,pear,orange]
```


现在，我可以定义一个将某事物列表 (`L`)， 转化为一个检查其参数，是否在该列表 `L` 中的测试函数的函数 `MakeTest(L)`。


```erlang
2> MakeTest = fun(L) -> (fun(X) -> lists:member(X, L) end) end.
#Fun<erl_eval.42.113135111>
3> IsFruit = MakeTest(Fruit).
#Fun<erl_eval.42.113135111>
```

当 `X` 是列表 `L` 的成员时，`lists:member(X, L)` 会返回 `true`；否则返回 `false`。现在我们就已构建了个测试函数，可以试试看。


```erlang
4> IsFruit(pear).
true
5> IsFruit(apple).
true
6> IsFruit(dog).
false
```


我们还可将其用作 `lists:filter/2` 的一个参数。


```erlang
7> lists:filter(IsFruit, [dog,orange,cat,apple,bear]).
[orange,apple]
```


返回 `funs` 的 `funs` 这种写法，需要一点时间来适应，因此我们来剖析一下这种写法，以便更清楚地了解发生了什么。返回某个 “正常”  值的函数是这样的：


```erlang
1> Double = fun(X) -> (X * 2) end.
#Fun<erl_eval.42.113135111>
2> Double(5).
10
```

其中括号内的代码（换句话说，`2 * X`），明显就是该函数的 “返回值”。现在我们来试着将一个 `fun` 放入这对括号。


请记住，括号里的内容，*即为* 返回值。


```erlang
3> Mult = fun(Times) -> ( fun(X) -> X * Times end ) end.
#Fun<erl_eval.42.113135111>
```

其中括号内的 `fun` 为 `fun(X) -> X * Times end`；这就是个 `X` 的函数，`Times` 属于 “外层” `fun` 的参数。


对 `Mult(3)` 求值，会返回 `fun(X) -> X * 3 end`，即以 `3` 替换 `Times` 后的那个内部 `fun` 的主体。现在我们可以测试这点。


```erlang
4> Triple = Mult(3).
#Fun<erl_eval.42.113135111>
5> Triple(5).
15
```

这样来看，`Mult` 是 `Double` 的一种 *泛化*。与其计算某个值，*他返回了个在调用时，将计算所要求值的函数*。


### 定义咱们自己的控制抽象


到目前为止，我们还没看到任何 `if` 语句、`switch` 语句、`for` 语句或 `while` 语句，但这似乎并不重要。一切都使用模式匹配及高阶函数编写出来。


当我们需要额外控制结构时，我们可以构造咱们自己的。下面是个示例；Erlang 没有 `for` 循环，所以我们来构造一个：


[`lib_misc.erl`](http://media.pragprog.com/titles/jaerlang2/code/lib_misc.erl)


```erlang
for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].
```


那么，比如对 `for(1,10,F)` 求值，就会创建处一个列表 `[F(1)，F(2)，...，F(10)]`。

现在我们有了个简单的 `for` 循环。我们可以用他构造一个从 `1` 到 `10` 的整数列表。


```erlang
1> lib_misc:for(1,10,fun(I) -> I end).
[1,2,3,4,5,6,7,8,9,10]
```

或者，我们可计算出 `1` 到 `10` 的整数平方。


```erlang
2> lib_misc:for(1,10,fun(I) -> I*I end).
[1,4,9,16,25,36,49,64,81,100]
```


随着咱们更有经验，就会发现创建咱们自己控制结构的能力，可以大大减小咱们程序的大小，并有时还会使他们更加清晰。这是因为咱们可以创建出刚好解决咱们问题所需的控制结构，而且咱们不受编程语言自带的一小套固定控制结构约束。


## 简单的列表处理

既然我们已介绍了 `funs`，咱们就可以继续编写 `sum` 和 `map` 了，我们改进版的 `total` 就需要他们（我相信你一定没有忘记！）。

我们将从 `sum` 开始，他会计算某个列表中元素的总和。



[`mylists.erl`](http://media.pragprog.com/titles/jaerlang2/code/mylists.erl)

```erlang
sum([H|T]) -> H + sum(T);
sum([])	   -> 0.
```


请注意 `sum` 中两个子句的顺序并不重要。这是因为第一个子句会匹配某个非空列表，同时第二个子句匹配的是个空列表，而这两种情况是互斥的。我们可以如下测试 `sum`：


```erlang
1> c(mylists).
{ok,mylists}
2> L = [1,3,10].
[1,3,10]
3> mylists:sum(L).
14
```


第 1 行编译了 `mylists` 这个 模组。从现在起，我（作者）会经常省略编译模组的命令，所以咱们必须记住自己完成这件事。要理解这段代码是如何工作很容易。我们来跟踪一下执行过程。


1. `sum([1,3,10])`；
2. `sum([1,3,10]) = 1 + sum([3,10])`（按照第一个子句 `sum([H|T]) -> H + sum(T)`）；
3. `= 1 + 3 + sum([10])`（按照第一个子句）；
4. `= 1 + 3 + 10 + sum([])`（按照第一个子句）；
5. `= 1 + 3 + 10 + 0`（按照第二个子句）；
6. `= 14`


最后，我们来看看我们前面曾见过的 `map/2`。下面是他的定义：


[`mylists.erl`](http://media.pragprog.com/titles/jaerlang2/code/mylists.erl)


```erlang
map(_, [])	    -> [];
map(F, [H|T])	-> [F(H)|map(F, T)].
```


1. 其首个子句说的是如何处理空列表。在空列表（什么也没有！）的元素上映射任何函数，都会产生一个空列表；
2. 第二个子句是一条有关如何处理有着头部 `H`、尾部 `T` 的某个列表的规则。仅构建出一个头部为 `F(H)`，尾部为 `map(F,T)` 的列表。


*注意*：`map/2` 的定义，是从标准库模组 `lists` 复制到 `mylists` 的。咱们可随意修改 `mylists.erl` 中的代码。在任何情况下，都不要尝试创建咱们自己的 `lists` 模组 -- 若咱们在 `lists` 中犯了任何错误，都很可能会严重破坏系统。


我们可使用一些将列表中的元素加倍及平方的函数，运行这个 `map`，如下所示：


```erlang
1> L = [1,2,3,4,5].
[1,2,3,4,5]
2> mylists:map(fun(X) -> 2*X end, L).
[2,4,6,8,10]
3> mylists:map(fun(X) -> X*X end, L).
[1,4,9,16,25]
```


稍后，我们将在 [26.3 节 “并行化顺序代码” 中](../part-v/Ch26-programming_multicore_CPUs.md#并行序列代码)，展示使用列表综合，编写的一个更简短版本的 `map`，我们将展示咱们如何以 *并行* 方式，计算映射 的所有元素（这将加快我们程序在多核计算机上的运行速度） -- 但这已太超前。既然我们已经知道了 `sum` 和 `map`，咱们就可以用这两个函数，重写 `total` 了：


[`shop2.erl`](http://media.pragprog.com/titles/jaerlang2/code/shop2.erl)

```erlang
-module(shop2).
-export([total/1]).
-import(lists, [map/2, sum/1]).

total(L) ->
    sum(map(fun({What, N}) -> shop:cost(What) * N end, L)).
```


通过查看涉及到那些步骤，我们就可以了解这个函数的工作原理。


```erlang
1> Buy = [{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}].
[{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}]
2> L1=lists:map(fun({What,N}) -> shop:cost(What) * N end, Buy).
[20,8,20,54,21]
3> lists:sum(L1).
123
```


> **我要怎样编写程序**
>
> 在编写某个程序时，我（作者）的方法是 “写一点” 就 “测试一点”。我会先编写一个包含几个函数的小模组，然后编译他，并在 shell 下用几个命令测试他。一旦我对他感到满意，我会再写几个函数，编译他们，测试他们，以此类推。
>
> 通常情况下，我并未真正决定在我的程序中，需要什么样的数据结构，而当我运行一些小的示例时，我就能明白，我所选择的数据结构是否合适。
>
> 我倾向于 “养大” 程序，而不是在编写他们之前，就将其完全想好。这样就不会在我发现错误之前，就犯下大错。最重要的是，这样做很有趣，我会立即得到反馈，并且在输入程序时，就立即会看到我的想法是否可行。
>
> 一旦在 shell 下搞清楚怎样完成某事，我通常就会去写个 makefile，以及一些重现我在 shell 下所掌握内容的代码。


还要注意这个模组中 `-import` 与 `-export` 声明的使用。


- 声明 `-import(lists,[map/2,sum/1]).` 表示函数 `map/2` *导入* 自模组 `lists` 中，以此类推。这意味着我们可以写下 `map(Fun,...)` 代替 `lists:map(Fun,...)`。`cost/1` 未在某个导入声明中声明，因此我们必须使用 “完全限定” 的名字 `shop:cost`；
- 声明 `-export([total/1])` 表示可从 `shop2` 这个模组外部，调用函数 `total/1`。只有从某个模组导出的函数，才能从该模组外部调用。


这时，咱们可能会认为我们这个 `total` 函数无法再被改进了，但咱们错了。进一步的改进是可行的。为此，我们将用到列表综合。


*知识点*：

- list comprehension
- fully qualified name


## 列表综合


所谓 *列表综合*，是一些不必用到 `funs`、映射或过滤器，即可创建出列表的表达式。这会使得我们的程序更加简短易懂。


我们将从一个示例开始。设想我们有个列表 `L`。


```erlang
1> L = [1,2,3,4,5].
[1,2,3,4,5]
```

并假设说我们打算把这个列表中的每个元素都翻倍。我们以前做过这个，但我还是要提醒一下。


```erlang
2> lists:map(fun(X) -> X*2 end, L).
[2,4,6,8,10]
```


不过还有种更简单的，使用列表综合的方法。

```erlang
3> [2*X || X <- L ].
[2,4,6,8,10]
```


`[ F(X) || X <- L]` 这种写法，表示 “`F(X)` 的列表，其中 `X` 取自列表 `L`”。因此，`[2*X || X <- L ]` 就表示 “`2*X` 的列表，其中 `X` 取自列表 `L`"。


要了解如何使用列表综合，我们可以在 shell 下输入几个表达式，看看会发生什么。我们以定义 `Buy` 开始。


```erlang
1> Buy = [{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}].
[{oranges,4},{newspaper,1},{apples,10},{pears,6},{milk,3}]
```


现在我们来把这个原始列表中每个项目的数量加倍。


```erlang
2> [{Name, 2*Number} || {Name, Number} <- Buy].
[{oranges,8},{newspaper,2},{apples,20},{pears,12},{milk,6}]
```


请注意，`||` 符号右侧的元组 `{Name, Number}`，是个会与列表 `Buy` 中每个元素匹配的 *模式*。而左边的元组 `{Name, 2*Number}`，则 是个 *构造函数*。


设想我们想要计算出那个原始列表中，所有元素的总费用；我们可以如下完成这点。首先用该列表中每项物品的价格，替换其名字。


```erlang
3> [{shop:cost(A), B} || {A, B} <- Buy].
[{5,4},{8,1},{2,10},{9,6},{7,3}]
```


现在乘以数量。


```erlang
4> [shop:cost(A) * B || {A, B} <- Buy].
[20,8,20,54,21]
```


再对他们求和。


```erlang
5> lists:sum([shop:cost(A) * B || {A, B} <- Buy]).
123
```


最后，若我们打算将其构造为一个函数，就可以写出下面的代码：


```erlang
total(L) ->
    lists:sum([shop:cost(A) * B || {A, B} <- L]).
```


> **译注**：在传入给此版本的 `total/1` 函数为空列表时，其仍能计算出结果为 `0`。


```erlang
8> shop3:total(Buy).
123
9> shop3:total([]).
0
```

> 请思考这是为什么......


列表综合会让咱们代码变得非常简短易读。例如，我们可以定义一个更简短版本的 `map`。


```erlang
map(F, L) -> [F(X) || X <- L].
```


列表综合最一般形式，是下面这种形式的表达式：


```erlang
[X || Qualifier1, Qualifier2, ...]
```


`X` 是个任意表达式，而每个限定符要么是个生成器，或位串生成器，要么是个过滤器。


- 生成器的写法是 `Pattern <- ListExpr`，其中 `ListExpr` 必须是个求值为项目列表的表达式；
- 位串生成器的写法是 `BitStringPattern <= BitStringExpr`，其中 `BitStringExpr` 必须是求值为位串的表达式。关于位串模式和生成器的更多信息，请参阅 [Erlang 参考手册](https://www.erlang.org/doc/system/reference_manual.html);
- 过滤器既可以是谓词（返回 `true` 或 `false` 的函数），也可以是布尔表达式。

请注意，列表综合的生成器部分，工作原理就像过滤器一样；下面是一个示例：


```erlang
1> [ X || {a, X} <- [{a,1},{b,2},{c,3},{a,4},hello,"wow"]].
[1,4]
```

我们将以几个简短示例结束这一小节。


*知识点*：

- a generator
- a bitstring generator
- a filter


### 快速排序


下面是使用两个列表综合，编写排序算法的方法：


[`lib_misc.erl`](http://media.pragprog.com/titles/jaerlang2/code/lib_misc.erl)


```erlang
qsort([]) -> [];
qsort([Pivot|T]) ->
    qsort([X || X <- T, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- T, X >= Pivot]).
```

请注意，其中的 `++` 是下位附加运算符。这段代码是为了显示其优雅，而非高效。以这种方式使用 `++`，一般不被认为是良好的编程实践。更多信息，请参阅 [4.9 节 “以自然顺序构建列表”](#以自然顺序构建列表)。


```erlang
1> L=[23,6,2,9,27,400,78,45,61,82,14].
[23,6,2,9,27,400,78,45,61,82,14]
2> lib_misc:qsort(L).
[2,6,9,14,23,27,45,61,78,82,400]
```


要了解这段代码的工作原理，我们将逐步执行。我们从列表 `L` 开始，调用 `qsort(L)`。以下命令以绑定 `Pivot → 23` 及 `T → [6,2,9,27,400,78,45,61,82,14]`，匹配了 `qsort` 的第二个子句：


```erlang
3> [Pivot|T] = L.
[23,6,2,9,27,400,78,45,61,82,14]
```

现在我们将 `T` 分成两个列表，一个包含 `T` 中所有小于 `Pivot` 的元素，另一个包含所有大于或等于 `Pivot` 的元素。


```erlang
4> Smaller = [X || X <- T, X < Pivot].
[6,2,9,14]
5> Bigger = [X || X <- T, X >= Pivot].
[27,400,78,45,61,82]
```


现在，我们对 `Smaller` 及 `Bigger` 进行排序，并将他们与 `Pivot` 结合起来。


```erlang
qsort( [6,2,9,14] ) ++ [23] ++ qsort( [27,400,78,45,61,82] )
= [2,6,9,14] ++ [23] ++ [27,45,61,78,82,400]
= [2,6,9,14,23,27,45,61,78,82,400]
```

*知识点*：

- the infix append operator


### 勾股数

**Pythagorean Triplets**

勾股定理三连式（毕达哥拉斯三元数组），是一组整数 `{A、B、C}`，其中 A<sup>2</sup> + B<sup>2</sup> = C<sup>2</sup>。


函数 `pythag(N)` 会生成一个包含所有整数 `{A,B,C}` 的列表，其中 A<sup>2</sup> + B<sup>2</sup> = C<sup>2</sup>，且两侧之和小于或等于 `N`。


[`lib_misc.erl`](http://media.pragprog.com/titles/jaerlang2/code/lib_misc.erl)


```erlang
pythag(N) ->
    [ {A, B, C} ||
      A <- lists:seq(1, N),
      B <- lists:seq(1, N),
      C <- lists:seq(1, N),
      A+B+C =< N,
      A*B + B*B =:= C*C
    ].
```


只要解释几句：`lists:seq(1, N)` 会返回一个从 `1` 到 `N` 的所有整数列表。因此，`A <- lists:seq(1, N)` 表示 `A` 会取 `1` 到 `N` 的所有可能值。因此，我们的程序读着：“取 `A` 从 `1` 到 `N` 的所有值，`B` 从 `1` 到 `N` 的所有值，`C` 从 `1` 到 `N` 的所有值，使得 `A + B + C` 小于或等于 `N`，并且 `A*A + B*B = C*C`”。


```erlang
1> lib_misc:pythag(16).
[{3,4,5},{4,3,5}]
2> lib_misc:pythag(30).
[{3,4,5},{4,3,5},{5,12,13},{6,8,10},{8,6,10},{12,5,13}]
```


### 字迷


若咱们对英文填字游戏感兴趣，就会经常发现咱们是在计算排列组合。我们来用 Erlang，以一个漂亮小函数 `perms`，查找某个字符串的所有排列组合。


[`lib_misc.erl`](http://media.pragprog.com/titles/jaerlang2/code/lib_misc.erl)


```erlang
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L -- [H])].
```


```erlang
1> lib_misc:perms("123").
["123","132","213","231","312","321"]
2> lib_misc:perms("cats").
["cats","cast","ctas","ctsa","csat","csta","acts","acst",
 "atcs","atsc","asct","astc","tcas","tcsa","tacs","tasc",
 "tsca","tsac","scat","scta","sact","satc","stca","stac"]
```


其中 `X -- Y` 是列表减法运算符。他会从 `X` 减去 `Y` 中的元素；更精确的定义见 [8.16 节，列表操作 `++` 和 `--`](Ch08-the_rest_of_sequential_erlang.md#列表操作-++-与---)。

`perms` 相当整洁。他的工作原理如下：假设我们要计算字符串 `"cats"` 的所有排列组合。首先，我们分离出该字符串的首字符，即 `c`，然后计算去掉 `c` 这个字符后，该字符串的所有排列。`"cats"` 去掉 `c` 的就是字符串 `"ats"`，而 `"ats"` 的所有排列,是 `["ats","ast","tas","tsa","sat","sta"]` 这些字符串。接下来，我们把 `c` 追加到这每个字符串开头，形成 `["cats","cast","ctas","tsa","tsat","sta"]`。然后，我们重复上述算法，分离出第二个字符，以此类推。


这正是 `perms` 函数所做的事情。


```erlang
[[H|T] || H <- L, T <- perms(L -- [H])]
```


这表示以所有可行方式，取出 `L` 中的 `H`，然后以所有可行方式，自 `perms(L - - [H])`（即列表 `L` 移除 `H` 后的所有排列组合）中取出 `T`，并返回 `[H|T]`。


> **译注**：以下命令的输出比较奇怪。


```erlang
3> lib_misc:perms("tweet").
["tweet","twete","tweet","twete","twtee","twtee","tewet",
 "tewte","teewt","teetw","tetwe","tetew","tewet","tewte",
 "teewt","teetw","tetwe","tetew","ttwee","ttwee","ttewe",
 "tteew","ttewe","tteew","wteet","wtete","wteet","wtete",
 [...]|...]
```

## BIFs


所谓 BIF，即某个 *内置函数*；BIFs 是一些被定义为 Erlang 语言一部分的函数。有些 BIFs 是以 Erlang 实现的，但大多数 BIFs 是作为 Erlang 虚拟机中的原语操作实现的。

BIFs 提供了到操作系统的接口，或执行那些在 Erlang 下不可行，或效率极低的操作。例如，要将某个列表转换为元组，或查找当前时间与日期，就不可行。要执行这样的操作，我们需要调用某个 BIF。


例如，BIF `list_to_tuple/1` 会将某个列表转换为元组，而 `time/0` 则会以时、分、秒形式返回当前时间。

```erlang
1> list_to_tuple([123,cat,"hello"]).
{123,cat,"hello"}
2> time().
{9,21,59}
```

所有 BIF 的行事，都像他属于 `erlang` 这个模组一样，不过一些最常见的 BIFs（比如 `list_to_tuple`）是 *自动导入的*，因此我们可以调用 `list_to_tuple(...)`，而不是 `erlang:list_to_tuple(...)`。


咱们可在咱们 Erlang 发行版中的 `erlang` 手册页面，或 [`erlang`](http://www.erlang.org/doc/man/erlang.html) 处，找到所有 BIFs 的完整列表。贯穿本书其余部分，我（作者）将只介绍那些，对于理解书中特定章节必需的 BIFs。实际上，系统中有着比我在书中介绍更多的 BIFs，因此我建议打印处那个手册页面，并尝试了解所有的 BIF。


## 守护符

所谓守护符，是一种我们可用以增强模式匹配能力的结构。通过使用守护，我们可对模式中的变量，执行简单测试和比较。假设我们要编写个计算 `X` 和 `Y` 最大值的函数 `max(X, Y)`。我们可使用一个守护，编写此函数：


```erlang
max(X, Y) when X > Y -> X;
max(X, Y) -> Y.
```


当 `X` 大于 `Y` 时，第一个子句就会匹配，同时结果为 `X`。


若第一个子句不匹配，则第二个子句就会被尝试。第二个子句始终返回第二个参数 `Y`。`Y` 必定大于或等于 `X`；否则，第一个子句就已经匹配了。


咱们可在函数定义的头部处，以 `when` 关键字引入守护而使用他们；咱们也可以在语言中，任何允许使用表达式的地方使用守护。当他们被作为表达式使用时，他们会求值为 `true` 或 `false` 两个原子中的一个。在守护求值为 `true` 时，我们就说该次计算 *成功*；否则他就 *失败*。

### 守护序列

所谓 *守护序列*，可以是单个的守望，也可以是以分号 (`;`) 分隔的条件序列。条件序列 `G1; G2;...;Gn` 在其中至少有一个 -- 即 `G1;G2;...` -- 为 `true` 时，就会求值为 `true`。

而一个 *条件*，则是一系列以逗号（`,`）分隔的 *条件表达式*。条件 `GuardExpr1, GuardExpr2,...,GuardExprN` 在所有条件表达式 -- 即 `GuardExpr1,GuardExpr2,...,` -- 求值为 `true` 时为 `true`。


有效条件表达式的集合，是所有有效 Erlang 表达式的一个子集。之所以将条件表达式限制为 Erlang 表达式的子集，是因为我们希望保证计算条件表达式，免于一些副作用的影响。条件表达式是模式匹配的一项扩展，而因为模式匹配没有副作用，我们就不希望条件的求值有副作用。

此外，条件（表达式）不能调用用户定义的函数，因为我们希望保证这些条件（表达式）免于副作用与终止的影响。


下列句法形式，在条件表达式中是合法的：


- 原子 `true`；
- 其他常量（项及绑定变量）；在某个条件表达式中，这些都会求值为 `false`；
- 对 [表 1 *条件谓词*](#条件谓词) 中的那些条件谓词，与对 [表 2 *条件的内建函数*](#条件的内建函数) 中的那些 BIFs 的调用；
- 项的比较（[表 6 *项的比较*](Ch08-the_rest_of_sequential_erlang.md#项的比较)）；
- 算术表达式（[表 3 *算术表达式*](Ch08-the_rest_of_sequential_erlang.md#算术表达式)）；
- 布尔表达式（[8.3 节，*布尔表达式*](Ch08-the_rest_of_sequential_erlang.md#布尔表达式)）；
- 短路布尔表达式（[8.23 节 *短路布尔表达式*](Ch08-the_rest_of_sequential_erlang.md#短路布尔表达式)）。


*注意*：在读到 [条件谓词](#条件谓词) 和 [条件内建函数](#条件内建函数) 时，咱们将发现一些到我们尚未讨论到数据类型的引用。他们包含在这两个表格中，是为完整性目的。


在计算条件表达式时，会用到 [8.20 节，*操作符优先级*](Ch08-the_rest_of_sequential_erlang.md#运算符优先级) 中，描述的优先级规则。


*知识点*：


- guards
- the `when` keyword
- guard sequence
- guard
- guard expression
- the atom `true`
- constant
- terms and bound variables
- the guard predicates
- term comparison
- arithmetic expression
- boolean expression
- short-cut boolean expression
- the precedence rules


### 条件示例

我们已讨论过条件的语法，而他们可能相当复杂；下面是几个示例：


```erlang
f(X, Y) when is_integer(X), X > Y, Y < 6 -> ...
```

这表示 “当 `X` 是个整数，`X` 大于 `Y`，且 `Y` 小于 `6`。" 其中分隔了这个条件中测试的逗号，表示 “且”。


下面的表格列出了所有条件谓词（即返回布尔值的条件），与所有的条件函数。




| *谓词* | *意义* |
| :-- | :-- |
| `is_atom(X)` | `X` 是个原子。 |
| `is_binary(X)` | `X` 是个二进制值。 |
| `is_constant(X)` | `X` 是个常量。 |
| `is_float(X)` | `X` 是个浮点数。 |
| `is_function(X)` | `X` 是个函数。 |
| `is_integer(X)` | `X` 是个整数。 |
| `is_list(X)` | `X` 是个列表。 |
| `is_map(X)` | `X` 是个映射。 |
| `is_number(X)` | `X` 是个整数或浮点数。 |
| `is_pid(X)` | `X` 是个进程标识符。 |
| `is_pmod(X)` | `X` 是个实例或参数化模组。 |
| `is_port(X)` | `X` 是个端口。 |
| `is_reference(X)` | `X` 是个引用。 |
| `is_tuple(X)` | `X` 是个元组。 |
| `is_record(X,Tag)` | `X` 是条类型为 `Tag` 的记录。 |
| `is_record(X,Tag,N)` | `X` 是条类型为 `Tag`且大小为 `N` 的记录。 |

<a name="条件谓词"></a>
表 1 -- 条件谓词


| *函数* | *意义* |
| :-- | :-- |
| `abs(X)` | `X` 的绝对值。 |
| `byte_size(X)` | `X` 中字节数量。`X` 必须是个位串或二进制值。 |
| `element(N,X)` | `X` 的第 N 个元素。请注意 `X` 必须是个元组。 |
| `float(X)` | 将 `X` 转换为浮点数，`X` 必须是个数字。 |
| `hd(X)` | 列表 `X` 的头部。 |
| `length(X)` | 列表 `X` 的长度。 |
| `node()` | 当前节点。 |
| `node(X)` | `X` 被创建处的节点。`X` 可以是个进程、某个标识符、某个引用，或某个端口。 |
| `round(X)` | 将 `X` 转换为整数，`X` 必须是个数字。 |
| `self()` | 当前进程的进程标识符。 |
| `size(X)` | `X` 的大小。`X` 可以是个元组或二进制值。 |
| `trunc(X)` | 截取 `X` 为一个整数，`X` 必须是个数字。 |
| `tl(X)` | 列表 `X` 的尾部。 |
| `tuple_size(X)` | 元组 `X` 的大小。 |

<a name="条件的内建函数"></a>
表 2 -- 条件的内建函数


```erlang
is_tuple(T), tuple_size(T) =:= 6, abs(element(3, T)) > 5
element(4, X) =:= hd(L)
...
```


其中第一行表示 `T` 是个由六元素的元组，且 `T` 的第三个元素绝对值大于 `5`。第二行表示元组 `X` 的第 4 个元素与列表 `L` 的头部相同。


```erlang
X =:= dog; X =:= cat
is_integer(X), X > Y; abs(Y) < 23
```

其中第一个条件表示 `X` 要么是个 `cat` 要么是个 `dog`，这个条件中的分号（`;`）表示 “或”。第二个条件表示 `X` 是个整数且大于 `Y`，或 `Y` 的绝对值小于 `23`。


下面是一些用到短路布尔表达式的条件示例：


```erlang
A >= -1.0 andalso A+1 > B
is_atom(L) orelse (is_list(L) andalso length(L) > 2)
```

在条件中允许布尔表达式的原因，是为了使条件在语法上，与其他表达式相似。而引入 `orelse` 和 `andalso` 两个运算符的原因，是因为布尔运算符 `and/or` 最初被定义为对二者的两个参数进行求值。在条件中，`and` 与 `andalso` 及 `or` 与 `orelse` 间，可能有些差别。例如，请看下面两个条件：


```erlang
f(X) when (X == 0) or (1/X > 2) ->
    ...

g(X) when (X == 0) orelse (1/X > 2) ->
    ...
```


在 `X` 为零时，`f(X)` 中的条件会失败，但 `g(X)` 中的则会成功。

在实践中，很少有程序会用到复杂条件，对于大多数程序来说，简单 (`,`) 条件就足够了。


### `true` 这个条件的使用

咱们可能想知道，为何我们需要 `true` 这个条件？原因是 `true` 这个原子，可用作 `if` 表达式最后的 “概括” 条件，就像下面这样：


```erlang
if
    Guard -> Expressions;
    Guard -> Expressions;
    ...
    true  -> Expressions
end
```


`if` 将在 [`if` 表达式](#if-表达式) 中讨论。


## `case` 与 `if` 表达式


到目前为止，我们都是使用模式匹配，处理 *所有事情*。这使得 Erlang 代码小巧且一致。但有时定义所有内容的单独函数子句并不方便。这时，我们可使用 `case` 或 `if` 表达式。



### `case` 表达式


`case` 有着如下语法：

```erlang
case Expression of
    Pattern1 [when Guard1] -> Expr_seq1;
    Pattern2 [when Guard2] -> Expr_seq2;
    ...
end
```


`case` 会按如下过程被求值：首先，其中的 `Expression` 会被求值；假设求值结果为 `Value`。然后，`Value` 会依次与 `Pattern1` （带有可选的条件 `Guard1`）、`Pattern2` 匹配，依此类推，直到找到匹配为止。一旦找到匹配，对应的表达式序列就会被求值 -- 表达式序列的求值结果，就是这个 `case` 表达式的值。在没有匹配到模式时，就会抛出一个异常。


早先，我们曾用到一个名为 `filter(P, L)` 的函数；他会返回 `L` 中 `P(X)` 为 `true` 的 `X` 所有元素列表。使用 `case`，我们可将 `filter` 定义为如下：


```erlang
filter(P, [H|T]) ->
    case P(H) of
        true  -> [H|filter(P, T)];
        false -> filter(P, T)
    end;
filter(P, []) -> [].
```

严格来说，`case` 并非必要。以下是使用纯模式匹配，定义 `filter` 的方法：


```erlang
filter(P, [H|T]) -> filter1(P(H), H, P, T);
filter(P, []) -> [].

filter1(true, H, P, T)   -> [H|filter(P, T)];
filter1(false, H, P, T)  -> filter(P, T).
```


这个定义相当丑陋；我们必须创造一个额外函数（称为 `filter1`），并将 `filter/2` 的所有参数传递给他。

> **过时的条件函数**
>
> 若咱们遇到了一些几年前编写的 Erlang 旧代码，那么条件测试的名称会有所不同。旧代码用到的名称为 `atom(X)`、`constant(X)`、`float(X)`、`integer(X)`、`list(X)`、`number(X)`、`pid(X)`、`port(X)`、`reference(X)`、`tuple(X)` 和 `binary(X)` 等条件测试。这些测试与名为 `is_atom(X)` ...... 等现代测试含义相同。在现代代码中使用旧名称，是不可取的。


### `if` 表达式


同样提供了第二个条件原语 `if`。语法如下：

```erlang
if
    Guard1 ->
        Expr_seq1;
    Guard2 ->
        Expr_seq2;
    ...
end

```


这会按如下过程被求值：首先 `Guard1` 会被求值。在求值结果为 `true` 时，那么 `if` 的值就会通过计算值表达式序列 `Expr_seq1`  得到。若 `Guard1` 未成功，则 `Guard2` 会被求值，依此类推，直到有个条件成功为止。`if` 表达式中必须至少有一个条件值为 `true`，否则抛出一个异常。


通常某个 `if` 表达式中的最后添加是原子 `true`，其保证了在所有其他条件都失效的情况下，表达式中的最后一个形式将被求值。


容易引起混淆的一点，是 `if` 表达式中最后的 `true` 条件的用法。若咱们来自类似于 C 的某门语言，咱们可能会写下一个没有 `else` 部分的 `if` 语句，就像这样：


```c
if (a > 0) {
    do_this();
}
```

因此，咱们可能会想以 Erlang，写出以下代码：


```erlang
if
    A > 0 ->
        do_this()
end
```


在 Erlang 下这可能会导致问题，因为 `if` 是个表达式，而所有表达式都应该有值。在 `A` 小于或等于零的情形下，这个 `if` 表达式就没有值。在 Erlang 下这就是个错误，而会导致程序崩溃。但在 C 中其却不是个错误。


为避免可能的异常，Erlang 程序员通常会在 `if` 表达式末尾，添加一个 `true` 条件。当然，若他们希望产生一次异常，就会省略附加的 `true` 条件。



## 以自然顺序构造列表


构建列表最有效的方法，是将一些元素添加到某个既有列表的头部，因此我们会经常看到这种模式的代码：


```erlang
some_function([H|T], ..., Result, ...) ->
    H1 = ... H ...,
    some_function(T, ..., [H1|Result], ...);
some_function([], ..., [H1|Result], ...) ->
    {..., Result, ...}.
```

这段代码沿着某个列表往下，提取出该列表的头部 `H`，并根据这个函数计算某个值（我们可称之为 `H1`）；其然后将 `H1` 添加到输出列表 `Result` 中。当输入列表耗尽时，最后一个子句就会匹配，同时输出变量 `Result` 会自该函数返回。


`Result` 中元素的顺序,与原始列表中元素顺序相反，这可能是个问题，也可能不是，但若他们的顺序错误，在最后一步中也可以很容易地将其颠倒过来。


基本理念相当简单。


1. 始终将元素添加到某个列表头部；
2. 提取 `InputList` 头部的元素，并将其添加到 `OutputList` 头部，会导致 `OutputList` 有着与 `InputList` 相反的顺序；
3. 在顺序很重要时，则要调用 `lists:reverse/1`，这是个高度优化的函数；
4. 请避免违背这些建议。


*注意*：无论咱们何时打算反转某个列表，咱们都应调用 `lists:reverse`，而不是其他。若咱们查看 `lists` 这个模组的源码，就会发现 `reverse` 的定义。不过，此定义只是用于演示目的。编译器在发现某个 `lists:reverse` 调用时，会调用该函数更高效的内部版本。


若咱们曾看到类似下面这样的代码，那么咱们脑袋里就会响起警钟 -- 这是非常低效的，只有在 `List` 很短情况下才能接受：


```erlang
List ++ [H]
```


尽管 `++` 运算符可能导致代码效率低下，但在清晰度和性能之间，也有权衡。在没有性能问题的前提下，使用 `++` 可能会使程序更加清晰。最好是先尽可能清晰地编写咱们的程序，然后，若出现性能问题，则要在进行任何优化之前，首先进行测量。


## 累加器


我们经常会希望从某个函数，返回两个列表。例如，我们可能打算编写某个将整数列表，拆分成分别包含原始列表中偶数和奇数的两个列表的函数。下面是完成的一种方法：

[`lib_misc.erl`](http://media.pragprog.com/titles/jaerlang2/code/lib_misc.erl)


```erlang
odds_and_evens(L) ->
    Odds  = [X || X <- L, (X rem 2) =:= 1],
    Evens = [X || X <- L, (X rem 2) =:= 0],
    {Odds, Evens}.
```

```erlang
5> lib_misc:odds_and_evens([1,2,3,4,5,6]).
{[1,3,5],[2,4,6]}
```



