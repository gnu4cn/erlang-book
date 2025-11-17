## SEE 的 API

`see.erl` 这个单一模组，导出了如下这些函数：

- `main()`

    启动系统。

- `load_module(Mod)`

    加载模组 `Mod`。

- `log_error(Error)`

    在标准输出上打印 `Error` 的值。

- `make_server(Name, FunStart, FunHandler)`

    创建一个名为 Name 的永久服务器。该服务器的初始状态，由执行 `FunStart()` 决定，同时该服务器的 “处理器” 函数为 `fun FunHandler`（稍后我们将详细介绍这个函数）。

- `rpc(Name, Query)`

    向服务器 `Name` 构造一次远程过程调用 `Query`。

- `change_behaviour(Name, FunHandler)`

    通过向服务器发送一个新的处理器函数 `FunHandler`，修改该服务器的行为。

- `keep_alive(Name, Fun)`

    确保始终有个注册名为 `Name` 的进程。这个进程是由执行 `Fun()` 启动（或重启）。

- `make_global(Name, Fun)`

    以注册的名字 `Name` 构造一个全局进程。该进程自身会生成 fun `Fun()`。

- `on_exit(Pid, Fun)`

    监控进程 `Pid`。当这个进程以原因 `{'EXIT', Why}` 退出时，随后将执行 `Fun(Why)`。

- `on_halt(Fun)`

    设置一个当某个停止系统的请求发出时，则 `Fun()` 将被执行的条件。在指定了多个 Fun 的情形下，则所有这些 Fun 将被调用。

- `stop_system(Reason)`

    以原因 `Reason` 停止系统。

- `every(Pid, Time, Fun)`

    只要 `Pid` 进程未被终止，那么就要每 `Time` 毫秒执行一次 `Fun()`。

- `lookup(Key, [{Key, Val}]) -> {found, val} | not_found`

    查找某个字典中的某个 `Key`。

- `read() -> [string()] | eof`

    读取标准输入中的下一行。

- `write([string()]) -> ok`

    将 `string` 写入标准输出。

- `env(Name)`

    返回环境变量 `Name` 的值。



这些函数均可被一些简单 Erlang 程序使用。


