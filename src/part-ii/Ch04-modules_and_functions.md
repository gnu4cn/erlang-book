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
