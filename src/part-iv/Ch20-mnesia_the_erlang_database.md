# Mnesia：Erlang 的数据库


设想咱们打算编写一款多用户游戏、构造一个新的 web 站点，或创建一个在线支付系统。咱们将可能需要一个数据库管理系统（DBMS）。


Mnesia 是一个以 Erlang 编写的数据库，用于要求苛刻的电信应用，同时是标准 Erlang 发行版的一部分。他可被配置为两个物理上分离节点上的 RAM 复制，提供快速的容错数据存储。他提供了事务能力，并带有其自己的查询语言。

Mnesia 速度极快，同时其可存储任何类型的 Erlang 数据结构。他还是高度可配置的。数据库的表可以被存储在内存中（出于速度原因）或磁盘上（出于持久性原因），而且这些表可以在不同的机器上复制，以提供容错行为。


## 查询初始数据库


在我们可完成任何事情前，我们必须创建个 Mnesia 数据库。咱们只需做一次这事。

```erlang
1> mnesia:create_schema([node()]).
ok
2> init:stop().
ok
$ ls
Mnesia.nonode@nohost
```

`mnesia:create_schema(NodeList)` 会在 `NodeList`（这必须是个有效的 Erlang 节点列表）中的所有节点上，初始化一个新的 Mnesia 数据库。在我们的示例中，我们给出的节点列表是 `[node()]`，即当前节点。Mnesia 将被初始化，并创建一个名为 `Mnesia.nonode@nohost` 的目录结构，以存储该数据库。


> **为何这个 DBMS 被叫做 Mnesia**？
>
> 最初的名字叫 Amnesia。我们的一位老板不喜欢这个名字。他说：“你们可不能把他叫做 Amnesia -- 你们可不能有一个会忘记事情的数据库！” 于是，我们去掉了 *A*，这个名字就沿用下来了。


然后，我们从 Erlang shell 退出，执行操作系统的 `ls` 命令验证这点。


当我们以一个名为 `joe` 的分布式节点，重复这一练习时，我们会得到以下结果：


```erlang
$ erl -name joe@host.xfoss.net
(joe@host.xfoss.net)1> mnesia:create_schema([node()]).
ok
(joe@host.xfoss.net)2> q().
ok
$ ls
Mnesia.joe@host.xfoss.net  Mnesia.nonode@nohost
```

或者，我们可以在启动 Erlang 时，指向某个特定数据库。


```erlang
$ erl -mnesia dir '"/home/hector/Documents/Mnesia.xfoss.com"' -name joe@host.xfoss.net
(joe@host.xfoss.net)1> mnesia:create_schema([node()]).
ok
(joe@host.xfoss.net)2> q().
ok
$ ls ~/Documents/Mnesia.xfoss.com
FALLBACK.BUP
```

其中 `/home/hector/Documents/Mnesia.xfoss.com` 是数据库将被存储于其下的目录名字。


## 数据库查询

在我们已创建了数据库后，我们就可以开始使用他。我们将从 Mnesia 的查询开始。当咱们看完之后，咱们可能会惊讶于 Mnesia 的查询，看起来很同时像 SQL 及列表综合，所以实际上要入门咱们几乎不需要学习什么。事实上，列表综合和 SQL 二者看起来很像并不奇怪。因为二者都基于数学的集合论。


在所有我们的示例中，我（作者）将假设咱们已创建了个有两个，分别叫作 `shop` 和 `cost` 表的数据库。这两个表包含的数据，在 [表 8, *`shop` 表*](#table-8)和 [表 9，*`cost` 表*](#table-9) 给出了。

Mnesia 中的表，是行的集合或包，其中每行都是一条 Erlang 的记录。要在 Mnesia 中表示这些表，我们需要一些定义了表中各个列的记录定义。这些定义如下所示：


| *Item* | *Quantity* | *Cost* |
| :- | :- | :- |
| apple | 20 | 2.3 |
| orange | 100 | 3.8 |
| pear | 200 | 3.6 |
| banana | 420 | 4.5 |
| potato | 2456 | 1.2 |

<a name="table-8"></a>

**表 8** -- **`shop` 表**


| *Name* | *Price* |
| :- | :- |
| apple | 1.5 |
| orange | 2.4 |
| pear | 2.2 |
| banana | 1.5 |
| potato | 0.6 |

<a name="table-9"></a>
**表 9** -- **`cost` 表**

```erlang
{{#include ../../projects/ch19-code/test_mnesia.erl:18:19}}
```

在可操作数据库前，我们需要创建一个数据库 schema、启动该数据库、添加一些数据表的定义以及停止数据库，并重启他。这些只需执行这一过程一次。下面即这段代码：


```erlang
{{#include ../../projects/ch19-code/test_mnesia.erl:22:28}}
```

```erlang
1> test_mnesia:do_this_once().
=INFO REPORT==== 16-Oct-2025::09:04:52.441325 ===
    application: mnesia
    exited: stopped
    type: temporary

stopped
```

现在，我们就可以继续咱们的示例了。


### 选取表中的所有数据


下面是选择 `shop` 表中所有数据的代码。(对于你们中了解 SQL 的来说，每个代码片段都以显示要执行相应操作的等价 SQL 开头。）


```erlang
{{#include ../../projects/ch19-code/test_mnesia.erl:34:38}}
```

这段代码的核心，是对 `qlc:q` 的调用，其会将查询（他的参数）编译为用于查询数据库的某种内部形式。我们把生成的查询，传递给一个名为 `do()` 的函数，其定义在靠近 `test_mnesia` 底部处。他负责运行查询并返回结果。为便于从 erl 中调用所有这些，我们将其映射到函数 `demo(select_shop)`。

在可使用数据库前，我们需要一个启动他并加载表定义的例程。这个例程必须在使用数据库前被运行，但每个 Erlang 会话中其只能被运行一次。


```erlang
{{#include ../../projects/ch19-code/test_mnesia.erl:30:32}}
...
{{#include ../../projects/ch19-code/test_mnesia.erl:77:90}}
...
{{#include ../../projects/ch19-code/test_mnesia.erl:133:139}}
```

现在我们就可以启动数据库，并进行一次查询了。


```erlang
1> test_mnesia:start().
ok
2> test_mnesia:reset_tables().
{atomic,ok}
3> test_mnesia:demo(select_shop).
[{shop,pear,200,3.6},
 {shop,apple,20,2.3},
 {shop,orange,100,3.8},
 {shop,potato,2456,1.2},
 {shop,banana,420,4.5}]
```

*注意*：表中的行可以任意顺序出现。

这个示例中，建立查询的行如下：


```erlang
qlc:q([X || X <- mnesia:table(shop)])
```

这看起来很像列表综合（参见 [4.5 节，列表综合](../part-ii/Ch04-modules_and_functions.md#列表综合)）。事实上，`qlc` 就代表查询列表理解。它是我们用来访问 Mnesia 数据库中数据的模块之一。



