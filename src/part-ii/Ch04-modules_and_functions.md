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



