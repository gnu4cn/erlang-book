# 并发介绍


让我们暂时忘掉电脑；我（作者）要从窗户向外看，告诉你们我看到了什么。


我（作者）看到一位女士带着狗散步。我看到一辆小汽车在寻找停车位。我看到一架飞机从头顶飞过，一艘船从旁边驶过。所有这些事情都是 *并行* 发生的。在本书中，我们将学习怎样把一些并行的活动，描述为一组相互通信的并行进程。我们将学习如何编写 *并发程序*。


在日常用语中，*并发*、*同步*及 *并行* 等词的意思几乎相同。但在编程语言中，我们需要更加精确。特别是，我们需要区分并发程序和并行程序。

*知识点*：

- concurrent, 并发
- simultaneous, 同步
- parallel, 并行


若我们只有一部单核的计算机，那么我们就永远无法在他上面运行某个并行的程序。这是因为我们只有一个 CPU，而他一次只能做一件事。不过，我们可以在一部单核计算机上，运行并发程序。计算机会在不同任务之间进行分时，从而维持不同任务并行运行的假象，the computer time-shares between the different tasks, maintaining the illusion that the different tasks run in parallel。


在接下来的小节中，我们将从一些简单的并发建模开始，进而了解使用并发解决问题的好处，最后看看突出并发和并行之间区别的一些精确定义。


## 建模并发


我们将从一个简单示例开始，建立一个日常场景的并发模型。请设想一下，我看到四个人出来散步。有两只狗和许多兔子。人们在互相交谈，而狗则想追逐那些兔子。


要用 Erlang 模拟这种情况，我们需要创建四个模组，分别称为 `person`、`dog`、`rabbit` 及 `world`。`person` 的代码将位于一个名为 `person.erl` 的文件中，看起来可能是这样的：


```erlang
-module(person). 
-export([init/1]).

init(Name) -> ...
```


其中第一行 `-module(person).`，表示该文件包含了名为 `person` 模组的代码。这应与文件名相同（不包括 `.erl` 文件扩展名）。模组名 *必须* 以小写字母开头。从技术上讲，模组名是个 *原子*；我们将在 [第 3.5 节 “原子”](../part-ii/Ch03-basic_concepts.md#原子) 中详细介绍何为原子。


模组声明之后，是个 *导出声明*。导出声明指出模组中的哪些函数，可以从该模组 *外部* 调用。他们就像许多编程语言中的 `public` 声明。不在导出声明中的函数属于私有的，而不能从模组外部调用。

*知识点*：

- the module declaration, 模组声明
- export declaration，导出声明
- public
- private


`-export([init/1]).` 这种语法，表示有一个参数（这是 `/1` 的意思；不是除以一 的意思）的函数 `init`，可从该模组外部调用。在我们打算导出多个函数时，可使用这种语法：


```erlang
-export([FuncName1/N1, FuncName2/N2, .....]).
```

其中方括号 `[ ...]` 的意思是 “列表”，因此这个声明表示我们打算从模组中导出一个函数列表。

我们还需编写 `dog` 与 `rabbit` 的代码。


### 启动模拟


要启动这个程序，我们将调用 `world:start()`。这是定义在一个名为 `world` 模组中的，其开头如此：


```erlang
-module(world). 
-export([start/0]).

start() ->
    Joe = spawn(person, init, ["Joe"]),
    Susannah = spawn(person, init, ["Susannah"]),
    Dave = spawn(person, init, ["Dave"]),
    Andy = spawn(person, init, ["Andy"]),
    Rover = spawn(dog, init, ["Rover"]),
    ...
    Rabbit1 = spawn(rabbit, init, ["Flopsy"]),
   ...
```

`spawn` 是个创建并发进程，并返回一个进程标识符的 Erlang 原语。`spawn` 的调用如下：


```erlang
spawn(ModName, FuncName, [Arg1, Arg2, ..., ArgN])
```

当 `spawn` 被求值时，Erlang 的运行时系统，会创建出一个新进程（不是个操作系统进程，而是个由 Erlang 系统管理的轻量级进程）。一旦该进程被创建出来，他就会开始计算由那些参数所指定的代码。`ModName` 是有着我们打算执行代码的模组名称。`FuncName` 是模组中函数的名称，`[Arg1, Arg2, ...]` 是个包含了我们要计算函数参数的列表。因此，以下调用表示要启动一个执行函数 `person:init("Joe")` 的进程：


```erlang
spawn(person, init, ["Joe"])
```


`spawn` 的返回值，是个可用于与这个新创建进程交互的 *进程标识符，PID*。


*知识点*：

- lightweight process, 轻量级进程
- process identifier，进程标识符


> 以对象类比
>
> Erlang 中的模组，就像面向对象编程语言，object-oriented programming language, OOPL, 中的类，而进程就像 OOPL 中的对象（或类的实例）。
>
> 在 Erlang 中，`spawn` 通过运行某个模组中定义的一个函数，创建出一个新的进程。在 Java 中，`new` 通过运行某个类中定义的一个方法，创建出一个新对象。
>
> 在某门 OOPL 中，我们可以有一个类，而有数千个类实例。同样，在 Erlang 中，我们可以有一个模组，而有数千甚至数百万个执行该模组中代码的进程。所有 Erlang 的进程，都并发且独立执行，如果我们有一台百万核的计算机，他们甚至可以并行运行。 



### 发送消息


一旦模拟启动，我们就会打算在程序中的不同进程间发送消息。在 Erlang 中，进程间不共享内存，而只能通过发送消息进行交互。这正是现实世界中，对象的行为方式。


假设 Joe 想要对 Susannah 说些什么。在程序中，我们就要写下面这样一行代码：

```erlang
Susannah ! {self(), "Hope the dogs don't chase the rabbits"}
```


`Pid ！Msg` 这种语法，表示将信息 `Msg` 发送到进程 `Pid`。花括号中的 `self()` 参数，指的是发送信息的进程（此情形下为 `Joe`）。


### 接收消息

要让 Susannah 的进程接收来自 Joe 的消息，我们就要写下这些代码：


```erlang
receive
    {From, Message} ->
        ...
end
```


当 Susannah 的进程收到一条信息时，其中的变量 `From` 将绑定到 `Joe`，这样 Susannah 就知道信息来自谁，而变量 `Message` 将包含这条信息。


我们可以设想扩展我们的模型，让狗子互相发送 `"woof woof rabbits"` 的消息，让兔子互相发送 `"panic go and hide"` 的消息。


我们应该记住的关键点是，我们的编程模型是基于对现实世界的观察。我们有三个模组（人、狗和兔子），因为在我们的示例中有三种并发事物。世界模组需要一个顶级进程来启动一切。因为有两只狗，所以我们创建了两个狗进程；因为有四个人，所以我们创建了四个人进程。程序中的信息反映了我们示例中观察到的信息。
