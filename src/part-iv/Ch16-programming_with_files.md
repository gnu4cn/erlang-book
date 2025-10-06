# 文件编程

在本章中，我们将介绍一些最常用到的文件处理函数。标准的 Erlang 发布，有着大量用于处理文件的函数。我们将集中讨论其中的一小部分，这我（作者）会用他们编写我的大部分程序，咱们也将相当频繁地用到。我们还将看到几个，编写高效文件处理代码技巧的示例。此外，我（作者）还会简要提及几种较少用到的文件操作，以便咱们知道他们存在。当咱们想要这些很少用到技术的细节时，请查阅手册页面。

我们将着重于以下几个方面：

- 用于操作文件的几个主要模组概述；
- 读取文件的几种不同方式；
- 写文件的几种不同方式；
- 目录操作；
- 查找文件信息。


## 操作文件的一些模组


文件操作的函数，被组织在四个模组中。

- `file`：这个模组有着打开、关闭、读取与写入文件；以及列出目录等的一些例程。[表 7，*文件操作摘要（`file` 模组中的）*](#table-7) 给出了 `file` 中一些常用函数的简短摘要。详情请查阅 [`file` 模组的手册页面](https://www.erlang.org/doc/apps/kernel/file.html)；

- `filename`：这个模组有着一些以平台独立方式，操作文件名的例程，因此咱们可在数种不同操作系统上，运行同一代码；

- `filelib`：这个模组是 `file` 的扩展，其包含数种用于列出文件、检查文件类型等等实用工具。大部分这些工具都是使用 `file` 中的函数编写的；

- `io`：这个模组有着一些作用于一些打开文件的例程。他包含了解析文件中数据，及将格式化数据写入文件的一些例程。


## 读取文件的方式

我们来看看读取文件时的一些选项。我们将以编写五个会以数种方式，打开某个文件并输入其中数据的小程序开始。

某个文件的内容，只是个字节序列。他们是否有意义，取决于这些字节的解释。

要演示这点，我们将对我们的所有示例，使用同一个输入文件。他实际上包含着一个 Erlang 项的序列。根据我们打开于读取这个文件的方式，我们可将这些内容，解释为

- 一个 Erlang 项的序列，a sequence of Erlang terms；
- 一个文本行的序列，a sequence of text lines；
- 或无特定解释的二进制数据原始块，raw chunks of binary data with no particular interpretation。


以下是这个文件中的原始数据：

[`data1.dat`](http://media.pragprog.com/titles/jaerlang2/code/data1.dat)

```erlang
{{#include ../../projects/ch16-code/data1.dat}}
```

现在，我们将通过数种方式，读取这个文件的那些部分。


### 读取该文件中的全部项

`data1.dat` 包含着一个 Erlang 项的序列；通过调用 `file:consult`，我们可读取到所有这些项，如下所示：


```erlang
1> file:consult("data1.dat").
{ok,[{person,"joe","armstrong",
             [{occupation,programmer},{favoriteLanguage,erlang}]},
     {cat,{name,"zorro"},{owner,"joe"}}]}
```


`file:consult(File)` 会假定 `File` 包含着一个 Erlang 项的序列。当他可以读取到该文件中的所有项时，他会返回 `{ok，[Term]}`；否则，他会返回 `{error，Reason}`。


| 函数 | 描述 |
| :-- | :-- |
| `change_group` | 修改某个文件的组。 |
| `change_owner` | 修改某个文件的所有者。 |
| `change_time` | 修改某个文件的修改时间，或上次访问的时间。 |
| `close` | 关闭某个文件。 |
| `consult` | 自某个文件中读取 Erlang 项。 |
| `copy` | 拷贝某个文件的内容。 |
| `del_dir` | 删除某个目录。 |
| `delete` | 删除某个文件。 |
| `eval` | 执行某个文件中的 Erlang 表达式。 |
| `format_error` | 返回某个错误原因的描述性字符串。 |
| `get_cwd` | 获取当前工作目录。 |
| `list_dir` | 列出某个目录下的文件。 |
| `make_dir` | 构造一个目录。 |
| `make_link` | 构造到某个文件的一个硬链接。 |
| `make_symlink` | 构造到某个文件或目录的一个符号链接。 |
| `open` | 打开某个文件。 |
| `position` | 设置某个文件中的位置。 |
| `pread` | 于某个确切位置读取某个文件。 |
| `pwrite` | 于某个确切位置写入某个文件。 |
| `read` | 读取某个文件。 |
| `read_file` | 读取整个文件。 |
| `read_file_info` | 获取某个文件的信息。 |
| `read_link` | 查看某个链接指向何处。 |
| `read_file_info` | 获取某个链接或文件的信息。 |
| `rename` | 重命名某个文件。 |
| `script` | 执行并返回某个文件中 Erlang 表达式的值。 |
| `set_cwd` | 设置当前工作目录。 |
| `sync` | 将某个文件的内存状态，与其在物理介质上的内容同步。 |
| `truncate` | 截取某个文件。 |
| `write` | 写入某个文件。 |
| `write_file` | 写入整个文件。 |
| `write_file_info` | 修改某个文件的信息。 |

<a name="table-7"></name>
**表 7** -- **文件操作摘要（`file` 模组中的）**


### 一次读取文件中的一个项

当咱们打算逐个读取文件中的项时，我们要首先以 `file:open` 打开该文件，然后以 `io:read` 读取单个项，直到文件结束，最后以 `file:close` 关闭该文件。

下面这个 shell 会话，显示了当我们一次读取一个文件中的项时，所发生的事情：


```erl
1> {ok, S} = file:open("data1.dat", read).
{ok,<0.87.0>}
2> io:read(S, '').
{ok,{person,"joe","armstrong",
            [{occupation,programmer},{favoriteLanguage,erlang}]}}
3> io:read(S, '').
{ok,{cat,{name,"zorro"},{owner,"joe"}}}
4> io:read(S, '').
eof
5> io:read(S, '').
eof
6> file:close(S).
ok
```

这里我们用到的函数如下：

- `-spec file:open(File, read) -> {ok, IoDevice} | {error, Why}`

    会尝试以读取目的打开 `File`。当其可以打开该文件时，会返回 `{ok, IoDevice}`；否则返回 `{error, Reason}`。`IoDevice` 是个用于访问该文件的 I/O 设备。

- `-spec io:read(IoDevice, Prompt) -> {ok, Term} | {error, Why} | eof`

    会从 `IoDevice` 读取一个 Erlang 项。当 `IoDevice` 代表着某个已打开文件时，则 `Prompt` 会被忽略。当我们使用 `io:read` 从标准输入读取时，则 `Prompt` 就仅用于提供某个提示符。

- `-spec file:close(IoDevice) -> ok | {error, Why}`

    关闭 `IoDevice`。

运用这些例程，我们就可以实现我们在上一小节中，曾用到的 `file:consult`。下面是 `file:consult` 可能的定义：


[`lib_misc.erl`](http://media.pragprog.com/titles/jaerlang2/code/lib_misc.erl)


```erlang
{{#include ../../projects/ch16-code/lib_misc.erl:122:137}}
```

这 *不* 是 `file:consult` 真正定义的样子。标准库使用了带有更好错误报告的改进版本。


现在是检视标准库中所包含版本的好时机。当咱们已经理解前面那个版本时，那么咱们应会很容易跟上库中的代码。只有一个问题：我们需要找到 `file.erl` 代码的源码。为了找到该源码，我们就要使用可找到任何已加载模组目标代码的函数 `code:which`。


```erlang
1> code:which(file).
"/usr/lib/erlang/lib/kernel-10.3.2/ebin/file.beam"
```

在标准发布下，每个库都有两个子目录。一个名为 `src`，包含着源码。另一个名为 `ebin`，包含编译后的 Erlang 代码。因此，`file.erl` 的源码，应是在下面的目录中：

```console
/usr/lib/erlang/lib/kernel-10.3.2/src/file.erl
```

> **译注**：在译者使用的基于 ArchLinux 的 Manjaro 发行版中，`file.erl` 位于上述位置。而原文为 `/usr/local/lib/erlang/lib/kernel-2.16.1/src/file.erl`。

当其他方法都不奏效，而手册页面又没有提供咱们有关代码的问题时，那么快速查看源码，往往就能揭示答案。现在我（作者）知道这种情况应不会发生，但我们都是人，而文档有时也不能回答咱们所有的问题。

### 一次读取文件中的一行


当我们将 `io:read` 改为 `io:get_line` 时，我们就能一次读取文件中的一行。`io:get_line` 会读取字符，直到他遇到换行符或文件结束符。下面是一个示例：

```erlang
1> {ok, S} = file:open("data1.dat", read).
{ok,<0.87.0>}
2> io:get_line(S, '').
"{person, \"joe\", \"armstrong\",\n"
3> io:get_line(S, '').
"        [{occupation, programmer},\n"
4> io:get_line(S, '').
"         {favoriteLanguage, erlang}]}.\n"
5> io:get_line(S, '').
"\n"
6> io:get_line(S, '').
"{cat, {name, \"zorro\"},\n"
7> io:get_line(S, '').
"      {owner, \"joe\"}}.\n"
8> io:get_line(S, '').
eof
9> file:close(S).
ok
```

> *知识点*：
>
> - a line-feed character
>
> - end-of-file



### 将整个文件读取到一个二进制值中


咱们可使用 `file:read_file(File)`，使用单个的原子操作，读取整个文件到某个二进制值。


```erlang
1> file:read_file("data1.dat").
{ok,<<"{person, \"joe\", \"armstrong\",\n        [{occupation, programmer},\n         {favoriteLanguage, erlang}]}.\n\n{cat"...>>}
```



当 `file:read_file(File)` 成功执行时， 其会返回 `{ok, Bin}`，否则返回 `{error, Why}`。这是迄今为止，读取文件的最有效方法，也是我（作者）经常使用的方式。对于大多数操作，我（作者）都是在一次操作中，将整个文件读入内存，然后操作文件内容，并在一次操作中存储文件（使用 `file:write_file`）。稍后我们给出一个这方面的示例。

### 以随机访问读取某个文件

当我们打算读取的文件非常大，或其包含了某种外部定义格式的二进制数据时，那么我们可以 `raw` 模式打开该文件，并使用 `file:pread` 读取他的任意部分。

下面是个示例：


```erlang
1> {ok, S} = file:open("data1.dat", [read, binary, raw]).
{ok,{file_descriptor,prim_file,
                     #{handle => #Ref<0.3050558618.3133800480.77952>,
                       owner => <0.85.0>,
                       r_buffer => #Ref<0.3050558618.3133800452.78324>,
                       r_ahead_size => 0}}}
2> file:pread(S, 22, 46).
{ok,<<"rong\",\n        [{occupation, programmer},\n    ">>}
3> file:pread(S, 1, 10).
{ok,<<"person, \"j">>}
4> file:pread(S, 2, 10).
{ok,<<"erson, \"jo">>}
5> file:close(S).
ok
```

`file:pread(IoDevice,Start,Len)` 会精确读取 `IoDevice` 从字节 `Start` 开始的 `Len` 个字节（文件中的字节被编号了，因此文件中的第一个字节，就处于位置 0 处）。他会返回 `{ok，Bin}` 或 `{error，Why}`。

最后，我们将使用随机文件访问的例程，编写在下一章我们会需要的一个使用工具历程。在 [17.6 节 *SHOUTcast 服务器*](./Ch17-programming_with_sockets.md#SHOUTcat-服务器) 中，我们将开发一个简单的 SHOUTcast 服务器（这是个所谓的流媒体服务器，在此情形下用于串流 MP3）。这个服务器的一部分，需要能够查找 MP3 文件中嵌入的艺术家及音轨名字。我们将在下一小节完成这个功能。


### 读取 MP3 元数据

MP3 是种用于存储压缩后音频数据的二进制格式。MP3 文件本身不包含文件内容的信息，因此，比如在某个包含着音乐的 MP3 文件中，录制该音乐的艺术家的名字，就不会包含在音频数据中。这一数据（音轨名字、艺术家名字等），会以一种称为 ID3 的标记块格式，存储在 MP3 文件内。ID3 标签，是由一位名叫 Eric Kemp 的程序员发明，以存储描述某个音频文件内容的元数据。ID3 格式实际上有很好几种，但出于我们的目的，我们将编写只访问两种最简单形式 ID3 标签的代码，即 ID3v1 和 ID3v1.1 两种标签。


ID3v1 标签有着简单的结构 -- 文件的最后 128 个字节，包含了个固定长度的标签。其中前 3 个字节，包含 ASCII 字符 `TAG`，然后是几个固定长度的字段。整个 128 字节被打包如下：

| 长度（字节） | 内容 |
| :-- | :-- |
| 3 | 包含字符 `TAG` 的头部 |
| 30 | 标题 |
| 30 | 艺术家 |
| 30 | 专辑 |
| 4 | 年份 |
| 30 | 评论 |
| 1 | 流派 |


在 ID3v1 标签中，没有添加音轨编号的位置。Michael Mutschler 在 ID3v1.1 格式中，提出了做到这点的一种方法。该想法是将 30 字节的评论字段，改为下面这样：


| 长度（字节） | 内容 |
| :-- | :-- |
| 28 | 评论 |
| 1 | `0`（一个零） |
| 1 | 音轨编号 |

要编写个尝试读取 MP3 文件中 ID3v1 标记，并使用二进制的位匹配语法，匹配这些字段的程序并不难。下面是这个程序：


[`id3_v1.erl`](http://media.pragprog.com/titles/jaerlang2/code/id3_v1.erl)


```erlang
{{#include ../../projects/ch16-code/id3_v1.erl}}
```

我们程序的主要入口，是 `id3_v1:dir(Dir)`。我们所做的第一件事，是调用 `lib_find:find(Dir, "*.mp3",true)`（稍后在 [16.6 节 *一个查找实用工具*](#一个查找实用工具) 中给出）找出我们的全部 MP3 文件，他会递归地扫描 `Dir` 下的目录，查找 MP3 文件。


找到文件后，我们就要调用 `read_id3_tag` 解析标签。因为我们可只使用 [比特匹配语法](../part-ii/Ch07-binaries_and_the_bit_syntax.md#位语法) 就能完成解析，因此解析就得以大大简化，然后我们通过移除作为字符串定界符的尾部空白和零填充字符，我们就可以修整艺术家与音轨名字。最后，我们将结果转储到一个文件中，供今后使用（`lib_misc:dump` 于 [*转存到文件*](./Ch21-profiling_debugging_and_tracing.md#转储到文件) 小节讲述）。

大多数音乐文件都以 ID3v1 打了标签，即使他们还有 ID3v2、v3 及 v4 的标签 -- 这些后来标签标准，是在文件开头（或更少见的在文件中间），添加了不同格式标签。标签程序通常会同时添加 ID3v1 的标签，并在文件开头添加额外（更难读取）的标签。就我们的目的而言，我们将只关注那些包含有效的 ID3v1 与 ID3v1.1 标记的文件。

现在我们知道了如何读取某个文件，那么我们就可以开始了解写入文件的不同方法。

## 写入文件的方式

写入文件，涉及与读取文件基本相同的操作。我们来看看他们。

### 将项的列表写入文件

设想我们打算创建一个可以 `file:consult` 读取的文件。标准库中实际上并未包含用于这一目的的函数，所以我们将自己写一个。我们把这个函数叫做 `unconsult`。


```erlang
{{#include ../../projects/ch16-code/lib_misc.erl:146:149}}
```


我们可在 shell 中运行他，创建出一个名为 `test1.dat` 的文件。


```erlang
1> lib_misc:unconsult("test1.dat", [{cats, ["zorrow", "daisy"]}, {weather, snowing}]).
ok
```

我们来检查一下其是否正常。

```erlang
2> file:consult("test1.dat").
{ok,[{cats,["zorrow","daisy"]},{weather,snowing}]}
```

`unconsult` 会以 `write` 模式打开文件，并调用 `io:format(S,"~p.~n",[X])` 将一些项写入该文件。


`io:format` 是创建格式化输出的主力工具。要生成格式化的输出，我们要调用下面的函数：

- `-spec io:format(IoDevice, Format, Args) -> ok`

    其中 `IoDevice` 是某种 I/O 设备（其必须要以 `write` 模式打开），`Format` 是个包含了格式化代码的字符串，而 `Args` 则是要输出的一个项的列表。


对于 `Args` 中的每个项目，格式字符串中都必须要有条格式化命令。格式化命令以波浪号（`~`）字符开头。下面是一些最常用的格式化命令：


- `~n` 写下一个换行符。`~n` 很聪明，会以平台独立的方式写下换行符。那么，在 Unix 的机器上，`~n` 将把 ASCII (`10`) 写到输出流，而在 Windows 机器上，他将把回车的换行符 ASCII (`13`、`10`)，写到输出流；
- `~p` 美化打印参数；
- `~s` 参数是个字符串、I/O 列表或原子，将以不带任何引号下打印出来；
- `~w` 以标准语法写入数据。用于输出 Erlang 的项。


格式字符串有 14 倍多的参数，正常人都记不住。咱们可在 [`io` 模组的手册页面](https://www.erlang.org/doc/apps/stdlib/io.html)，找到完整的参数列表。


我（作者）只记得 `~p`、`~s` 和 `~n`。当咱们从这些开始时，咱们就不会有太多问题。


### 旁白

我（作者）撒谎了 -- 咱们需要的可能不止 `~p`、`~s` 和 `~n`。下面是几个例子：


| 格式 | 结果 |
| :-- | :-- |
| <code>io:format("&#124;~10s&#124;", ["abc"])</code> | <code>&#124;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;abc&#124;</code> |
| <code>io:format("&#124;~-10s&#124;", ["abc"])</code> | <code>&#124;abc&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#124;</code> |
| <code>io:format("&#124;~10.3.+s&#124;", ["abc"])</code> | <code>&#124;+++++++abc&#124;</code> |
| <code>io:format("&#124;~10.7.+s&#124;", ["abc"])</code> | <code>&#124;+++abc++++&#124;</code> |
| <code>io:format("&#124;~-10.10.+s&#124;", ["abc"])</code> | <code>&#124;abc+++++++&#124;</code> |


### 写入行到文件

以下示例与前面的类似 -- 我们只是使用了不同的格式化命令：


```erlang
1> {ok, S} = file:open("test2.dat", write).
{ok,<0.87.0>}
2> io:format(S, "~s~n", ["Hello readers"]).
ok
3> io:format(S, "~w~n", [123]).
ok
4> io:format(S, "~s~n", ["that's it"]).
ok
5> file:close(S).
ok
```


这就创建了个名为 `test2.dat`，有着以下内容的文件：


```console
Hello readers
123
that's it
```


### 在一次操作中写入整个文件


这是写入某个文件的最有效率方式。`file:write_file(File, IO)` 会将作为某个 I/O 列表的 `IO` 中的数据，写入到 `File`。(所谓 I/O 列表，是个其元素为一些 I/O 列表、二进制文件或 0 至 255 整数的列表。当 I/O 列表作为输出时，他会被自动 “扁平化”，即全部列表的括号或被移除）。这种方法非常高效，也是我（作者）经常使用的一种方法。下一小节中的程序，会演示这点。


- **列出某个文件中的 URL**

我们来编写个名为 `urls2htmlFile(L, File)` 的简单函数，其会接收一个 URL 的列表 `L`，并创建一个其中 URL 将以可点击链接形式呈现的 HTML 文件。这就让我们可操练在一次 I/O 操作中，创建整个文件的技术。我们将在 `scavenge_urls` 模组中编写咱们的程序。首先是程序头部：


[`scavenge_urls.erl`](http://media.pragprog.com/titles/jaerlang2/code/scavenge_urls.erl)


```erlang
{{#include ../../projects/ch16-code/scavenge_urls.erl:1:3}}
```


该程序有两个入口点。`urls2htmlFile(Urls, File)` 会取一个 URL 的列表，并创建一个包含每个 URL 可点击链接的 HTML 文件；`bin2urls(Bin)` 会检索某个二进制值，并返回包含在这个二进制值中的所有 URL 的列表。`urls2htmlFile` 如下所示：


[`scavenge_urls.erl`](http://media.pragprog.com/titles/jaerlang2/code/scavenge_urls.erl)


```erlang
{{#include ../../projects/ch16-code/scavenge_urls.erl:5:17}}
```

这段代码会返回一个字符的 I/O 列表。请注意，我们并未尝试展开列表（进行扁平化处理，这样做效率相当低）；我们构造了个字符的深层列表，然后直接将其丢给了输出例程。当我们以 `file:write_file` 将某个 I/O 列表写入文件时，I/O 系统会自动展开列表（也就是说，其会只输出列表中嵌入的字符，而不会输出列表的括号本身）。最后，下面是从二进制值中提取 URL 的代码：


[`scavenge_urls.erl`](http://media.pragprog.com/titles/jaerlang2/code/scavenge_urls.erl)

```erlang
{{#include ../../projects/ch16-code/scavenge_urls.erl:19:29}}
```


要运行这个程序，我们需要获取一些要解析的数据。输入数据（一个二进制值）是个 HTML 页面的内容，因此我们需要一个 HTML 页面来清理。为此，我们将使用 `socket_examples:nano_get_url`（参见 [从服务器获取数据](./Ch17-programming_with_sockets.md#自服务器获取数据)）。

我们将在 shell 下一步步完成。

```erlang
1> B = socket_examples:nano_get_url("erlang.xfoss.com", "/"),
   L = scavenge_urls:data2urls(B),
   scavenge_urls:urls2htmlFile(L, "gathered.html").
ok
```

这会产生文件 `gathered.html`。

```html
{{#include ../../projects/ch16-code/gathered.html}}
```


> **译注**：这里用到的 `socket_examples.erl` 代码，与原文不同。用到的代码如下。
>
> ```erlang
> {{#include ../../projects/ch16-code/socket_examples.erl}}
> ```
>
> 译者尝试使用接受消息方式，接受 TLSSocket 下的数据，但并未成功。故转而使用了 `ssl:recv/2` 函数接收。
> 此时接收到的并非二进制数据，而是列表数据。
>
> 参考：
>
> - [`ssl` 手册页](https://www.erlang.org/docs/26/man/ssl)


### 写入某个随机读写文件




## 一个查找实用工具

