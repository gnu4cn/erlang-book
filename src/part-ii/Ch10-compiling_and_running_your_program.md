# 编译及运行咱们的程序

在前几章中，我们尚未讲到较多有关编译和运行咱们程序的内容 -- 我们只使用了 Erlang shell。这对于小型示例来说没问题，但当咱们的程序变得愈加复杂时，咱们就会希望自动化这个过程，让生活变得更轻松。这就是 makefile 的用武之地。


实际上有三种不同方法运行咱们的程序。在本章中，我们将介绍这三种方法，以便咱们在任何特定场合，都能选择最佳方法。

有时，事情会出错：makefile 会失败，环境变量会出错，咱们的检索路径也会不正确。我们通过了解出错时该怎么办，帮助咱们解决这些问题。



## 修改开发环境


当咱们开始用 Erlang 编程时，咱们可能会把咱们的所有模组和文件，放在同一目录下，并在这个目录下启动 Erlang。当咱们这样做时，Erlang 的加载器就能顺利找到咱们的代码。然而，当咱们的应用变得愈加复杂时，咱们就会打算将他们分割成易于管理的小代码块，并将代码放在不同目录中。而在咱们要包含其他项目的代码时，这些外部代码将有着他们自己的目录结构。


### 设置加载代码的检索路径


Erlang 的运行时系统，会运用一种代码自动加载机制。为了这种机制正常工作，咱们必须设置一些检索路径，以便找到咱们代码的正确版本。


这种代码加载机制，实际上是以 Erlang 编程的 -- 在 [8.10 节 “动态代码加载”](Ch08-the_rest_of_sequential_erlang.md#动态代码加载) 中，我们曾谈到了这点。代码加载是 “按需” 执行的。


当系统试图调用某个尚未加载模组中的某个函数时，一个异常就会出现，同时系统会尝试找到这个缺失模组的目标代码文件。若缺失模组名为 `myMissingModule`，那么代码加载器将在当前加载路径下的所有目录中，检索名为 `myMissingModule.beam` 的文件。检索会在第一个匹配文件处停止，同时该文件中的目标代码将被加载到系统中。


通过启动 Erlang shell 并执行命令 `code:get_path()`，咱们就可以找到当前加载路径的值。下面是一个示例：


```erlang
1> code:get_path().
[".","c:/Users/ZBT7RX/erlang-otp/lib/kernel-10.3.2/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/stdlib-7.0.2/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/xmerl-2.1.5/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/wx-2.5.1/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/tools-4.1.2/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/tftp-1.2.3/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/syntax_tools-4.0/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/ssl-11.3.2/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/ssh-5.3.2/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/snmp-5.19/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/sasl-4.3/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/runtime_tools-2.2/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/reltool-1.0.2/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/public_key-1.18.2/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/parsetools-2.7/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/os_mon-2.11/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/odbc-2.16/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/observer-2.18/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/mnesia-4.24/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/megaco-4.8/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/jinterface-1.15",
 "c:/Users/ZBT7RX/erlang-otp/lib/inets-9.4/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/ftp-1.2.4/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/eunit-2.10/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/et-1.7.2/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/erts-16.0.2/ebin",
 "c:/Users/ZBT7RX/erlang-otp/lib/erl_interface-5.6/ebin",
 [...]|...]
```

我们用于操作加载路径的两个最常用函数如下：


- `-spec code:add_patha(Dir) -> true | {error, bad_directory}`

将一个新目录 `Dir`，添加到加载路径的开头。

- `-spec code:add_pathz(Dir) -> true | {error, bad_directory}`


将一个新目录 `Dir`，添加到加载路径的末尾。


通常情况下，咱们使用那个函数并不重要。唯一需要注意的是，使用 `add_patha` 和 `add_pathz` 会产生不同结果。当咱们怀疑加载了某个不正确模组时，咱们可调用 `code:all_loaded()`（这会返回所有已加载模组的列表）或 `code:clash()`，帮助咱们调查出了什么问题。

### 停止 Erlang
