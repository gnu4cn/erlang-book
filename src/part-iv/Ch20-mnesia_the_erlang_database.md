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
