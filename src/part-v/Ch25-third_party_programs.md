# 第三方程序

本章讨论第三方程序，即由我们的用户编写及发布的 Erlang 程序。此类程序的主要来源是 GitHub。在本章中，我们将介绍 GitHub 上的三个流行程序。我们还将了解如何在 GitHub 上创建和发布新项目，以及如何在咱们自己的应用中，包含某个 GitHub 项目。我们将学习以下内容：

- [`rebar`](https://github.com/erlang/rebar3) 由 Dave Smith 编写的 Rebar，已成为管理 Erlang 项目的事实标准。使用 `rebar`，用户可以创建新项目、编译项目、打包项目，并将其与其他项目集成。Rebar 与 GitHub 集成，因此用户可以轻松获取 GitHub 上的其他 `rebar` 项目，并将其集成到他们自己的应用；

- [`bitcask`](https://github.com/basho/bitcask) Bitcask 由 [Basho 公司](http://basho.com/) 的人编写，是一个持久性键值磁盘存储。他速度快且 “崩溃友好”，这意味着他会在崩溃后重启时快速恢复；

- [`cowboy`](https://github.com/ninenines/cowboy) Cowboy 由 Loïc Hoguin 编写，是个 Erlang 的高性能 web 服务器，在实现嵌入式 web 服务器方面越来越受欢迎。我们曾将一个 cowboy 服务器，用于 [第 18 章 *使用 Websockets 和 Erlang 浏览*](../part-iv/Ch18-browsing_with_websockets_and_erlang.md) 处的代码开发。


## 使用 Rebar 构造可共享归档以及管理咱们的代码


在这一小节中，我们将介绍构造一个我们将托管在 GitHub 上的开源 Erlang 项目的必要步骤。我（作者）假设咱们已在 GitHub 上有个账户。我们将使用 `rebar` 管理这个项目。


我们将完成以下事项：

1. 安装 `rebar`；
2. 在 GitHub 上创建一个新项目；
3. 将该项目克隆到本地；
4. 使用 `rebar` 添加该项目的样板文件代码；
5. 使用 `rebar` 编译咱们的项目；
6. 上传咱们的项目到 GitHub。


### 安装 `rebar`


Rebar 可从 [erlang/rebar3](https://github.com/erlang/rebar3) 获取。咱们可在 [rebar3.org](https://www.rebar3.org/) 上找到 `rebar` 的预编译二进制程序。要安装 `rebar`，请复制这个文件，将文件模式改为可执行，并将其放在咱们路径中的某处。


> **译注**：`rebar` 经历了从 [basho/rebar](https://github.com/basho/rebar) 到 [rebar/rebar](https://github.com/rebar/rebar)，并最终来到 [erlang/rebar3](https://github.com/erlang/rebar3)，这也反映了 Erlang 生态的一些变迁。


完成此操作后，咱们应测试一下咱们是否可运行 `rebar3`。


```console
$ rebar3 -v
rebar 3.25.1 on Erlang/OTP 28 Erts 16.0.3
```


### 在 GitHub 上构造一个新的项目

假设我们打算构造一个名为 `bertie` 的新项目（我(作者)以 Alexander McCall Smith 书中的人物命名我的一些项目）。第一步是创建一个新的 GitHub 项目。为此，我（作者）登录了自己的 GitHub 账户，并按照指示创建了一个新的代码仓库。

1. 点击登录页面工具栏右上方的 “创建新软件仓库” 图标（这图标看起来像上面有个加号的一本书）；

2. 将其构造为有个 readme 文件的公共存储库；

3. 然后点击创建存储库。


### 将该项目克隆到本地

在我（作者）家中的计算机上，有个名为 `${HOME}/published` 的单个目录，我把所有共享项目都放在这里。我移步到我的 `published` 目录下，并克隆这个 GitHub 仓库。


```console
$ cd published
$ git clone git@github.com:gnu4cn/bertie.git
正克隆到 'bertie'...
警告：您似乎克隆了一个空仓库。
```

现在，我（作者）通常会检查我是否可向这个版本库写入更改。因此，我会修改那个 readme 文件并将其推送回版本库。


```console
$ vim README.md
$ git add README.md
$ git commit README.md
$ git push
枚举对象中: 3, 完成.
对象计数中: 100% (3/3), 完成.
写入对象中: 100% (3/3), 240 字节 | 240.00 KiB/s, 完成.
总共 3（差异 0），复用 0（差异 0），包复用 0（来自  0 个包）
To github.com:gnu4cn/bertie.git
 * [new branch]      main -> main
```

在这个结果下，我（作者）松了一口气，并对现代科技的奥秘感到惊叹。

现在，我（作者）在 `~/published/bertie` 下有了个被同步到 `git@github.com:joearms/bertie.git` 处 GitHub 仓库的本地目录。


### 构造一个 OTP 应用


现在我们切换到这个 `bertie` 目录，使用 `rebar3` 创建一个标准 OTP 应用。


```console
$ cd published
$ rebar3 new release bertie
===> Writing bertie/apps/bertie/src/bertie_app.erl
===> Writing bertie/apps/bertie/src/bertie_sup.erl
===> Writing bertie/apps/bertie/src/bertie.app.src
===> Writing bertie/rebar.config
===> Writing bertie/config/sys.config
===> Writing bertie/config/vm.args
===> Writing bertie/.gitignore
===> Writing bertie/LICENSE.md
===> Writing bertie/README.md
```

> **译注**：原文中创建新 OTP 的命令为 `rebar create-app appid=bertie`。但在 `rebar3` 中 `create-app` 不是有效命令。该命令在旧版本的 `rebar`（特别是 `rebar2`）中出现过，但 `rebar3` 采用了不同的项目创建方法。
>
> 要使用 `rebar3` 创建新的 Erlang 应用，咱们应使用带有相应模板的 `new` 命令。标准应用最常用的模板是 `release`。


现在我们将把单个模组 `berti.erl`，添加到 `~/published/bertie/apps/bertie/src`。


```erlang
-module(bertie).
-export([start/0]).

start() -> io:format("Hello my name is Bertie~n").
```


> **译注**：`rebar3` 项目目录结构如下。
>
>
> ```console
> $ tree ~/published/bertie
> .
> ├── apps
> │   └── bertie
> │       └── src
> │           ├── bertie_app.erl
> │           ├── bertie.app.src
> │           ├── bertie.erl
> │           └── bertie_sup.erl
> ├── _build
> │   └── default
> │       └── lib
> │           └── bertie
> │               ├── ebin
> │               │   ├── bertie.app
> │               │   ├── bertie_app.beam
> │               │   ├── bertie.beam
> │               │   ├── bertie_sup.beam
> │               │   ├── elog4.config
> │               ├── include -> ../../../../apps/bertie/include
> │               ├── priv -> ../../../../apps/bertie/priv
> │               └── src -> ../../../../apps/bertie/src
> ├── config
> │   ├── sys.config
> │   └── vm.args
> ├── LICENSE.md
> ├── README.md
> ├── rebar.config
> └── rebar.lock
>
> 11 directories, 18 files
> ```
>
> 可见 `rebar3` 与 `rebar` 目录结构相比已发生变化。故这里 `bertie.erl` 所在位置已不同于原书中的 `~/published/bertie/src`。


