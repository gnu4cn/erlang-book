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

- `file`：这个模组有着打开、关闭、读取与写入文件；以及列出目录等的一些例程。[表 7，*文件操作摘要（`file` 模组中的）](#table-7) 给出了 `file` 中一些常用函数的简短摘要。详情请查阅 [`file` 模组的手册页面](https://www.erlang.org/doc/apps/kernel/file.html)；

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


## 写文件的方式

### 将项的列表写到某个文件

### 将整个文件读入某个二进制值

