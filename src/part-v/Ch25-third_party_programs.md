# 第三方程序

本章讨论第三方程序，即由我们的用户编写及发布的 Erlang 程序。此类程序的主要来源是 GitHub。在本章中，我们将介绍 GitHub 上的三个流行程序。我们还将了解如何在 GitHub 上创建和发布新项目，以及如何在咱们自己的应用中，包含某个 GitHub 项目。我们将学习以下内容：

- [`rebar`](https://github.com/erlang/rebar3) 由 Dave Smith 编写的 Rebar，已成为管理 Erlang 项目的事实标准。使用 `rebar`，用户可以创建新项目、编译项目、打包项目，并将其与其他项目集成。Rebar 与 GitHub 集成，因此用户可以轻松获取 GitHub 上的其他 `rebar` 项目，并将其集成到他们自己的应用；

- [`bitcask`](https://github.com/basho/bitcask) Bitcask 由 [Basho 公司](http://basho.com/) 的人编写，是一个持久性键值磁盘存储。他速度快且 “崩溃友好”，这意味着他会在崩溃后重启时快速恢复；

- [`cowboy`](https://github.com/ninenines/cowboy) Cowboy 由 Loïc Hoguin 编写，是个 Erlang 的高性能 web 服务器，在实现嵌入式 web 服务器方面越来越受欢迎。我们曾将一个 cowboy 服务器，用于 [第 18 章 *使用 Websockets 和 Erlang 浏览*](../part-iv/Ch18-browsing_with_websockets_and_erlang.md) 处的代码开发。



