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

这看起来很像列表综合（参见 [4.5 节，列表综合](../part-ii/Ch04-modules_and_functions.md#列表综合)）。事实上，`qlc` 代表 *query list, comprehensions, 查询列表综合*。他是我们用以访问 Mnesia 数据库中数据的模组之一。

`[X || X <- mnesia:table(shop)]` 表示 “`X` 的一个列表，其中 `X` 取自 `shop` 这个 Mnesia 表"。`X` 的值，是 Erlang 的一些 `shop` 记录。


*注意*：`qlc:q/1` 的参数，必须是个列表综合的字面量，而不能是某个可求值为此类表达式的其他内容。因此，以下代码与这个示例中的代码， 并 *不* 等同：


```erlang
Var = [X || X <- mnesia:table(shop)],
qlc:q(Var)
```

### 选择某个表中的数据


下面是个选取 `shop` 表中 `item` 和 `quantity` 列的查询：


```erlang
{{#include ../../projects/ch19-code/test_mnesia.erl:41:45}}
```

```erlang
4> test_mnesia:demo(select_some).
[{pear,200},
 {apple,20},
 {orange,100},
 {potato,2456},
 {banana,420}]
```

在上一各查询中，`X` 的值为 `shop` 类型的一些记录。当咱们回顾 [5.2 小节 *记录下的命名元组项目*](../part-ii/Ch05-records_and_maps.md#记录下的命名元组项目) 中，描述的记录语法时，咱们就会记得 `X#shop.item` 指向了 `shop` 记录的 `item` 字段。因此，元组 `{X#shop.item, X#shop.quantity}` 便是 `X` 的 `item` 和 `quantity` 字段的元组。


### 有条件地选取表中的数据


下面是个列出 `shop` 表中，库存数量小于 250 的所有物品的查询。也许我们将使用这个查询，决定要重新订购哪些物品。请注意，其中的条件，是如何作为列表综合的一部分自然描述的。


```erlang
{{#include ../../projects/ch19-code/test_mnesia.erl:48:55}}
```

```erlang
5> test_mnesia:demo(reorder).
[pear,apple,orange]
```

### 选取两个表中的数据（联合）


现在我们假设，我们打算只重新订购库存少于 250 件，且价格低于 2.0 个货币单位的物品。要完成这一操作，我们需要访问两个表。下面是这个查询：


```erlang
{{#include ../../projects/ch19-code/test_mnesia.erl:56:69}}
```


这里的关键，是 `shop` 表中物品名称，与 `cost` 表中名称之间的连接。

`X#shop.item =:= Y#cost.name`


## 添加与移除数据库中的数据


同样，我们将假设我们已经创建了数据库并定义了个 `shop` 表。现在我们打算从表中添加或删除某行。


### 添加行

我们可如下往 `shop` 表中添加一行：

```erlang
{{#include ../../projects/ch19-code/test_mnesia.erl:92:97}}
```

这个函数会创建一条 `shop` 记录，并将其插入该表。


```erlang
1> test_mnesia:start().
ok
2> test_mnesia:reset_tables().
{atomic,ok}
%% list the shop table
3> test_mnesia:demo(select_shop).
[{shop,pear,200,3.6},
 {shop,apple,20,2.3},
 {shop,orange,100,3.8},
 {shop,potato,2456,1.2},
 {shop,banana,420,4.5}]
%% add a new row
4> test_mnesia:add_shop_item(orange, 236, 2.8).
{atomic,ok}
%% list the shop table again so we can see the change
5> test_mnesia:demo(select_shop).
[{shop,pear,200,3.6},
 {shop,apple,20,2.3},
 {shop,orange,236,2.8},
 {shop,potato,2456,1.2},
 {shop,banana,420,4.5}]
```

*注意*：`shop` 表的 *主键*，是该表中的第一列，即 `shop` 记录中的 `item` 字段。该表为 “集合” 类型（请参阅 [19.1 节，*数据表的类型*](Ch19-storing_data_with_ets_and_dets.md#数据表的类型) 中集合与包类型的讨论）。当新创建的记录主键，有着与数据库表中某个既有行同样的主键时，他将覆盖该行；否则，一条新记录将被创建。


### 移除行

要移除某行，我们需要知道该行的对象 ID（OID）。这是由表的名字和主键的值组成。


```erlang
{{#include ../../projects/ch19-code/test_mnesia.erl:99:104}}
```

```erlang
6> test_mnesia:remove_shop_item(pear).
{atomic,ok}
%% list the table -- the pear has gone
7> test_mnesia:demo(select_shop).
[{shop,apple,20,2.3},
 {shop,orange,236,2.8},
 {shop,potato,2456,1.2},
 {shop,banana,420,4.5}]
8> mnesia:stop().
=INFO REPORT==== 16-Oct-2025::14:26:47.497779 ===
    application: mnesia
    exited: stopped
    type: temporary

stopped
```

## Mnesia 的事务


当我们添加或移除数据库中的数据，或执行某个查询时，我们如下编写的代码：

```erlang
do_something(...) ->
    F = fun() ->
            % ...
            mnesia:write(Row)
            % ... or ...
            mnesia:delete(Oid)
            % ... or ...
            qlc:e(Q)
        end,
    mnesia:transaction(F)
```


其中 `F` 是个零参数的函数。在 `F` 内部，我们调用了 `mne-sia:write/1`、`mnesia:delete/1` 或 `qlc:e(Q)`（其中 `Q` 是以 `qlc:q/1` 编译过的一个查询）的某种组合。建立了这个 fun 后，我们会调用 `mnesia:transaction(F)`，他会计算这个 fun 中的表达式序列。


事务会防止错误的程序代码，但更重要的是会防止对数据库的并发访问。设想我们有两个试图同时访问同一数据的进程。例如，假设我（作者）在我的银行账户里有 10 美元。现在设想两个试图同时从该账户中，提取 8 美元的人。我会希望这两个事务中一个成功，另一个失败。


这正是 `mnesia:transaction/1` 所提供的保证。在某个特定事务中，对数据库中表的全部读写，都会要么成功，要么全都失败。当全都失败时，该事务即被称作失败。当该事务失败时，就不会有任何更改将在该数据库上执行。

Mnesia 为此所使用的策略，是 *悲观锁* 的一种形式。当 Mnesia 的事务管理器访问某个表时，他会根据上下文，尝试锁定记录或整个表。当其发现这可能导致死锁时，他会立即中止事务，并撤销其所做的任何更改。


当事务由于其他进程正访问数据，而已开始就失败时，系统会等待一小段时间，然后重试该事务。这样做的一个后果，是该事务 fun 内的代码，可能会被执行很多次。

出于这一原因，事务 fun 中的代码，不应执行任何有副作用的事情。例如，当我们打算写出以下代码时：


```erlang
F = fun() ->
        ...
        io:format("reading ..."), %% don't do this
        ...
    end,
mnesia:transaction(F),
```

我们可能会得到大量输出，因为这个 fun 可能会被重试很多次。


*注意 1*：`mnesia:write/1` 和 `mnesia:delete/1`，都只应在某个由 `mnesia:transaction/1` 处理的 fun 中调用。


*注意 2*：咱们绝不应该在 Mnesia 的访问函数（`mnesia:write/1`、`mnesia:delete/1` 等）中，编写显式捕获异常的代码，因为 Mnesia 的事务机制本身，依赖于这些函数在失败时抛出异常。当咱们捕获了这些异常，并试图咱们自己处理他们时，咱们将破坏事务机制。


### 中止事务

我们商店附近有个农场。农场主种了苹果。农场主喜欢橘子，他会用苹果付橘子的钱。目前的比率是两个苹果换一个橘子。因此，要买 `N` 个橘子，农场主就要支付 `2*N` 个苹果。下面是个当农场主买了些橘子时，更新数据库的函数：


```erlang
{{#include ../../projects/ch19-code/test_mnesia.erl:107:130}}
```

这段代码写得非常愚蠢，因为我（作者）想要展示事务机制的工作原理。首先，我（作者）要更新数据库中苹果的数量。这是我在检查橙子数量 *前* 完成的。我执行这个操作的原因，是要展示当事务失败时，这一更改会被 “撤销”。通常情况下，我（作者）会将写回橙子和苹果数据到数据库，延迟到我已确定我有着足够橙子之后。

我们来在操作中展示这一点。早上，农场主来了商店，并买了 50 个橘子。


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
4> test_mnesia:farmer(50).
{atomic,ok}
5> test_mnesia:demo(select_shop).
[{shop,pear,200,3.6},
 {shop,apple,120,2.3},
 {shop,orange,50,3.8},
 {shop,potato,2456,1.2},
 {shop,banana,420,4.5}]
```

下午，农夫打算再买 100 个橘子（天哪，这家伙真爱吃橘子）。

```erlang
6> test_mnesia:farmer(100).
{aborted,oranges}
7> test_mnesia:demo(select_shop).
[{shop,pear,200,3.6},
 {shop,apple,120,2.3},
 {shop,orange,50,3.8},
 {shop,potato,2456,1.2},
 {shop,banana,420,4.5}]
```

当事务失败时（在我们调用 `mnesia:abort(Reason)` 后），由 `mnesia:write` 执行的那些更改会被撤销。因此，数据库状态就被恢复到了我们进入该事务前的状态。


### 加载测试数据



