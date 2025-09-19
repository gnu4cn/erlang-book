# 二进制值与位语法

所谓 *二进制值* 是一种设计用于以节省空间的方式，存储大量原始数据的数据结构。Erlang 虚拟机在二进制值的高效输入、输出和消息传递等方面，进行了优化。

在存储大量非结构化数据内容，例如大型字符串或文件的内容时，应尽可能使用二进制值。


在大多数情况下，某个二进制值的比特（二进制位）数，将正好能被 8 整除，而因此会对应到一个字节序列。当比特数不能被 8 整除时，我们就会用 *比特串* 来指代该数据。当咱们讲 “比特串” 时，就是要强调该数据的位数，不是 8 的整数倍。


二进制值、比特串及位级的模式匹配被引入 Erlang，是为简化网络编程，其中我们经常需要探究协议数据包的位与字节级别的结构。


在本章中，我们将首先详细介绍二进制值。二进制值上的大多数操作，都会以同样方式作用于比特串，因此在了解二进制值后，我们将重点介绍比特串与二进制值的不同之处。


> *知识点*：
>
>- binary
>- bitstring
- bit-level pattern match


## 二进制值


二进制值是以由成对的小于和大于括符，括起来的整数或字符串序列形式写下及打印出来的。下面是个示例：


```erlang
1> <<5,10,20>>.
<<5,10,20>>
2> <<"hello">>.
<<"hello">>
3> <<65,66,67>>.
<<"ABC">>
```


当咱们在某个二进制值中用到整数时，则每个整数必须在范围 `0` 到 `255` 中。二进制值 `<<"cat">>` 是 `<<99,97,116>>` 的简称；也就是说，二进制是由该字符串中那些字符的 ASCII 字符编码组成的。


与字符串一样，当某个二进制值的内容，是个可打印字符串时，那么 shell 将把该二进制值，打印为一个字符串；否则，他将被打印为一个整数序列。


使用 BIF 或位语法（参见 [7.2 小节，位语法](#位语法)），我们可构建出一个二进制值，以及提取某个二进制值中的元素。在这一小节中，我们将只讨论操作二进制值的那些 BIFs。


### 使用二进制值

我们可使用 BIFs， 或 `binary` 模组中的那些函数操作二进制值。`binary` 中导出的许多函数,都是作为原生代码实现的。下面是一些最重要的函数：


- `list_to_binary(L) -> Bin`

    `list_to_binary` 返回一个经由 *展开* （所谓 *展开*，是指移除全部列表括号）该 *iolist* `L` 中的所有元素，构建出的二进制值。而所谓 *iolist*，则被递归地元素是 `0..255` 中整数、一些二进制值或 `iolists` 的列表。

    ```erlang
    1> Bin1 = <<1,2,3>>.
    <<1,2,3>>
    2> Bin2 = <<4,5>>.
    <<4,5>>
    3> Bin3 = <<6>>.
    <<6>>
    4> list_to_binary([Bin1, 1, [2,3,Bin2], 4|Bin3]).
    <<1,2,3,1,2,3,4,5,4,6>>
    ```

    *注意*：第 1 行中等号两边的空格是必要的。若没有这个空格，那么被 Erlang 的标记符号转换器看到的第二个符号，将是原子 `'=<'`，即等于或小于运算符。有时，我们必须在二进制字面值周围，加上空格或括号，避免语法错误。

> *知识点*：tokenizer, 分词器

- `split_binary(Bin, Pos) -> {Bin1, Bin2}`

    这会将二进制值 `Bin`，从 `Pos` 位置处分割成两个部分。

```erlang
1> split_binary(<<1,2,3,4,5,6,7,8,9,10>>, 3).
{<<1,2,3>>,<<4,5,6,7,8,9,10>>}
```

- `term_to_binary(Term) -> Bin`

    这会将任何的 Erlang 项转换位一个二进制值。

    由 `term_to_binary` 生成的二进制，以所谓的外部项格式表示。已使用 `term_to_binary` 转换为二进制值的项，可被存储在文件中，也可经由网络以报文形式发送等等，同时构造出这些二进制值的原始项，可被重建出来。对于在文件中存储复杂数据结构，或向远端机器发送复杂数据结构，这就非常有用。


- `binary_to_term(Bin) -> Term`

    这是 `term_to_binary` 的逆过程。


```erlang
1> B = term_to_binary({binaries, "are", useful}).
<<131,104,3,119,8,98,105,110,97,114,105,101,115,107,0,3,
  97,114,101,119,6,117,115,101,102,117,108>>
2> binary_to_term(B).
{binaries,"are",useful}
```

- `byte_size(Bin) -> Size`


    这会返回二进制值中的字节数。


```erlang
3> byte_size(<<1,2,3,4,5>>).
5
```


所有这些 BIFs 中，`term_to_binary` 和 `binary_to_term` 绝对是我（作者）的最爱。他们俩相当有用。`term_to_binary` 会任何项都转换为一个二进制值。在二进制值中（若咱们偷偷看一下），就会发现以 “Erlang 外部项格式”（定义在 Erlang 文档中）<sup>1</sup>存储的数据。一旦我们将某个项转换为了二进制值，我们就可以经由套接字，以报文形式发送该二进制值，或将其存储在某个文件中。这是用于实现分布式 Erlang 的基本底层方法，也被用于许多数据库中。


> *注*
>
> <sup>1</sup> [External Term Format](https://www.erlang.org/doc/apps/erts/erl_ext_dist.html)



## 位语法


所谓位语法，是一种用于提取及打包二进制数据中，单个比特或比特序列的写法。当咱们在编写打包与解包二进制数据的底层代码时，咱们会发现位语法不可思议地有用。位语法是为协议编程开发的（这正是 Erlang 所擅长的），而会产生出操作二进制数据的高效代码。


> *知识点*

- protocol programming


设想我们有三个变量 -- `X`、`Y` 和 `Z` -- 我们打算把他们打包进某个 16 位的内存区域。`X` 在结果中应占 3 位，`Y` 应占 7 位，而 `Z` 则应占 6 位。在大多数语言中，此操作都会涉及一些凌乱的底层操作，包括移位与掩码等。而在 Erlang 中，我们只要写出以下代码：


```erlang
M = <<X:3, Y:7, Z:6>>.
```


这会创建一个二进制值，并将其存储在变量 `M` 中。请注意：`M` 属于类型 `binary`，因为该数据的总位长是 16 位，其正好能被 8 整除。 当我们将 `X` 的大小改为 `2` 位，而写下以下代码：


```erlang
M = <<X:2, Y:7, Z:6>>.
```


那么 `M` 中的总位数就是 15，因此得到的数据结构为 `bitstring` 类型。


全部的位语法略微复杂，因此我们将渐次展开。首先，我们将学习一些将 RGB 颜色数据打包及解包为 16 位字的简单代码。然后，我们将深入研究位语法表达式的细节。最后，我们将看卡从实际代码中选取的三个用到位语法的示例。


### 打包及解包 16 位色彩数据


我们将以一个非常简单的示例开始。设想我们打算表示某种 16 位的 RGB 颜色。我们决定分配 5 位给红色通道，6 位给绿色通道，及 5 位给蓝色通道。(因为人眼对绿光更敏感，所以我们对绿色通道多分配了一位。）


我们可创建一个包含着单个 RGB 三连字符的 16 位内存区域 `Mem`，方法如下：


```erlang
1> Red = 2.
2
2> Green = 61.
61
3> Blue = 20.
20
4> Mem = <<Red:5, Green:6, Blue:5>>.
<<23,180>>
```

请注意，在表达式 4 中，我们创建了个包含 16 位数量的 2 字节二进制值。shell 会将其打印为 `<<23,180>>`。


要打包该内存，我们只要写下表达式 `<<Red:5, Green:6, Blue:5>>`。


而要将这个二进制值解包为整数变量 `R1`、`G1` 和 `B1`，我们要写个模式。


```erlang
5> <<R1:5, G1:6, B1:5>> = Mem.
<<23,180>>
6> R1.
2
7> G1.
61
8> B1.
20
```


这很简单。如果你不相信我（作者），那么就以你最喜欢的编程语言，使用移位和逻辑运算的与和或，试试完成此操作。


实际上，我们可使用位语法完成的事情，要远比这个简单示例所揭示的多得多，不过首先我们需要掌握一种相当复杂的语法。一旦掌握了完成这件事情，我们将能够写出打包和解包复杂二进制数据结构的，非常简短的代码。


### 位语法表达式

位语法表达式，被用于构造二进制值或位串。他们有着如下形式：


```erlang
<<>>
<<E1, E2, ..., En>>
```


其中每个元素 `Ei` 都指定出了该二进制值或位串的一个单独 *区段*。每个元素 `Ei` 可以有四种可能的形式。


```erlang
Ei = Value |
     Value:Size |
     Value/TypeSpecifierList |
     Value:Size/TypeSpecifierList
```


当表达式中的总位数能被 8 平均整除时，那么其将被构造为一个二进制值；否则，他将被构造为一个位串。


当咱们构造某个二进制值时，`Value` 必须是

- 绑定的变量、
- 字面的字符串、
- 或某个求值为整数、浮点数或二进制值的表达式。

当在某次模式匹配操作中用到时，`Value` 则可以是

- 绑定或未绑定的变量、
- 整数、
- 字面的字符串、
- 浮点数、
- 或二进制值。


其中 `Size` 必须是个求值为整数的表达式。在模式匹配中，`Size` 必须是个整数，或其值为整数的某个绑定变量。当 `Size` 位于模式中需要该值的某处时，`Size` 必须是个绑定变量。`Size` 的值可以从该二进制值较早的模式匹配中获得。例如下面这个：


```erlang
<<Size:4, Data:Size/binary, ...>>
```


就是个合法的模式，因为 `Size` 的值从该二进制值的前四位解包出来了，然后被用于注解该二进制值中的下一个区段大小。


`Size` 的值指定了区段的大小。而默认值则取决于类型。对于整数，默认值为 8，对于浮点数其为 64，而对于二进制值，则是该二进制值的大小。在模式匹配中，默认值只对最后一个元素有效。如果某个区段大小为被指定，那么将假定一个默认值。


其中 `TypeSpecifierList` 是个以连字符（`-`）分隔的 `End-Sign-Type-Unit` 形式的项目清单。前面的任何项目都可以省略，而且这些项目可以任何顺序出现。而在省略了某个项目时，就会使用该项目的默认值。


> *知识点*：

- the form `End-Sign-Type-Unit`

说明符清单中的项目，可以有以下这些值：


- `End` 为 `big | little | native`

    这个项目指定了机器的字节序。`native` 字节序于运行时确定，取决于咱们机器的 CPU。默认值为 `big`，也称为 *网络字节序*。该值的唯一意义，在于打包和解包二进制值中的整数和浮点数。在不同字节序的机器上打包和解包二进制值中的整数时，咱们应注意使用正确的字节序。


    在编写位语法表达式时，可能一些试验可能是必要的。为确信咱们正在执行正确的操作，请尝试以下 shell 命令：

```erlang
1> {<<16#12345678:32/big>>,<<16#12345678:32/little>>,<<16#12345678:32/native>>,<<16#12345678:32>>}.
{<<18,52,86,120>>,
 <<120,86,52,18>>,
 <<120,86,52,18>>,
 <<18,52,86,120>>}
```
    该输出结果展示给我们的，正是如何使用位语法，把一些整数打包在某个二进制值中。
    若咱们存有担心，那么 `term_to_binary` 和 `binary_to_term` 在打包和解包整数时，会 “完成正确的事情”。因此，举例来说，咱们可在某 big-endian 的机器上，创建一个包含一些整数的元组。然后使用 `term_too_binary` 将该元组转换为一个二进制值，并将这个值发送到某 little-endian 的机器上。在这台 little-endian 机器上，咱们执行`binary_to_term`，此时元组中的所有整数，都将具有正确的值。

- `Sign` 为 `signed | unsigned`

    这个参数只被用于模式匹配中。默认值为 `unsigned`。

- `Type` 为 `integer | float | binary | bytes | bitstring | bits | utf8 | utf16 | utf32`

    默认值为 `integer`。

- `Unit` 被写作 `unit:1 | 2 | ... 256`


    对于 `integer`、`float` 及 `bitstring`，`Unit` 的默认值为 `1`，对于 `binary`，默认值为 `8`。`utf8`、`utf16` 及 `utf32` 类型下则不需要该值。

    区段的总大小为长 `Size x Unit` 位。二进制值类型区段，则必须有着能被 8 平均整除的某个大小。



若咱们发现这里的位语法描述有点令人生畏，请不要慌张。要让位语法模式正确，可能非常棘手。达到此目的的最佳方式，是在 shell 中试验咱们所需的模式，直到令其正确为止，然后将剪切并粘贴结果到咱们的程序中。我（作者）就是这么做的。



### 真实世界的位语法示例


掌握位语法属于一些额外的努力，但好处却是巨大的。本小节有三个现实生活中的示例。这里的所有代码，都是从现实世界程序中剪切和粘贴过来的。


第一个示例会查找 MPEG 音频数据中的那些同步点。该示例展示了位语法模式匹配的强大；代码非常容易理解，并与 MPEG 的头部帧规范，有明确的对应关系。第二个示例用于构建 Microsoft 通用对象文件格式 (COFF) 的二进制数据文件。打包和解包二进制数据文件（比如 COFF），就通常是使用二进制值及二进制模式匹配完成的。最后一个示例展示了如何解包某个 IPv4 数据报。


- **找出 MPEG 数据中的同步帧**


    设想我们打算编写一个处理 MPEG 音频数据的程序。我们可能打算以 Erlang 编写个流媒体服务器，或者提取描述 MPEG 音频流内容的数据标签。要完成此目的，我们需要识别出某个 MPEG 流中的数据帧，并与之同步。

    MPEG 音频数据由若干的帧组成。每帧都有自己的帧头部，其后是音频信息 -- 没有文件头部，且原则上咱们可将某个 MPEG 文件，剪切成一些片段，并播放这些片段中的任何一个。任何读取 MPEG 数据流的任何软件，都必须找到那些头部帧，然后才能同步 MPEG 数据。

    MPEG 头部会以 11 位的 *帧同步* 开始，其由 11 个连续的二进制位 `1` 组成，后跟描述（该头部）后面数据的信息：


```text
AAAAAAAA AAABBCCD EEEEFFGH IIJJKLMM
```


| 项目 | 作用 |
| :-- | :-- |
| `AAAAAAAA AAA` | 同步字（11 位，全部为 `1`）。 |
| `BB` | 这 2 个位是该 MPEG 音频的版本 ID。 |
| `CC` | 这 2 个位为这个层的描述。 |
| `D`  | 这 1 个为，是个保护位。 |

等等......

> *知识点*：

- the data frames in an MPEG stream
- the header frames
- frame sync


    这些比特位的具体细节，在此无需赘述。基本上，只要知道 `A` 到 `M` 的值，我们就能计算出某个 MPEG 帧的总长度。


    为了找到同步点，我们首先假定我们已正确定位在某个 MPEG 头部的起点。随后我们就要尝试计算出该帧的长度。然后会出现以下情况之一：

    - 我们的假设是正确的，那么当我们向前跳过该帧的长度时，我们将发现另一个 MPEG 头部；
    - 我们的假设不正确；要么我们未处于某个标记头部的 11 个连续 `1` 的序列，要么是这个字的格式不正确，以致我们无法计算该帧的长度；
    - 我们的假设不正确，但我们处在几个恰好看起来像某个头部的开始处。在这种情况下，我们可以计算出帧的长度，但当我们跳过这个长度时，我们无法找新头部。


    要确保万无一失，我们就要查找三个连续头部。这个同步例程如下：


[`mp3_sync.erl`](http://media.pragprog.com/titles/jaerlang2/code/mp3_sync.erl)

```erlang
find_sync(Bin, N) ->
    case is_header(N, Bin) of
        {ok, Len1, _} ->
            case is_header(N + Len1, Bin) of
                {ok, Len2, _} ->
                    case is_header(N + Len1 + Len2, Bin) of
                        {ok, _, _} -> {ok, N};
                        error -> find_sync(Bin, N+1)
                    end;
                error -> find_sync(Bin, N+1)
            end;
        error -> find_sync(Bin, N+1)
    end.

```
    `find_sync` 会尝试找到三个连续的 MPEG 头部帧。当 `Bin` 中的第 `N` 个字节，是某个头部帧的开始时，那么 `is_header(N, Bin)` 将返回 `{ok, Length, Info}`。而当 `is_header` 返回 `error` 时，则说明 `N` 未能指向某个正确帧的开始。
    我们可在 shell 中完成一个快速测试，确保这会工作。


```erlang
1> c(mp3_sync).
{ok,mp3_sync}
2> {ok, Bin} = file:read_file("shili_pinghu.mp3").
{ok,<<73,68,51,3,0,0,0,0,1,34,84,80,69,49,0,0,0,9,0,0,1,
      255,254,196,158,17,151,...>>}
3> mp3_sync:find_sync(Bin, 1).
{ok,172}
```
    这个函数使用了 `file:read_file` 将整个文件读入一个二进制值（请参阅 [将整个文件读入某个二进制值](../part-iv/Ch16-programming_with_files.md#将整个文件读入一个二进制值)）。现在是 `is_header`：



[`mp3_sync.erl`](http://media.pragprog.com/titles/jaerlang2/code/mp3_sync.erl)


```erlang
is_header(N, Bin) ->
    unpack_header(get_word(N, Bin)).

get_word(N, Bin) ->
    {_,<<C:4/binary,_/binary>>} = split_binary(Bin, N),
    C.

unpack_header(X) ->
    try decode_header(X)
    catch
	    _:_ -> error
    end.
```
    这段代码稍微复杂一些。首先，我们提取 32 位数据来分析（通过 `get_word` 完成）；然后我们使用 `decode_header` 解包这个头部。现在，`decode_header` 被编写为当其参数不属于某个头部的开头时，就要崩溃（通过调用 `error/0`）。为捕获任何的错误，我们将到 `decode_header` 的调用，封装在一个 `try...catch` 语句中（请在 [6.1 节 “处理顺序代码中的错误”](Ch06-error_handling_in_sequential_programs.md#顺序代码中的错误处理) 中阅读有关此问题的更多内容）。这也将捕获任何可能由 `framelength/4` 中的错误代码引起的错误。`decode_header` 是全部乐趣开始之处。



[`mp3_sync.erl`](http://media.pragprog.com/titles/jaerlang2/code/mp3_sync.erl)


```erlang
decode_header(<<2#11111111111:11,B:2,C:2,_D:1,E:4,F:2,G:1,Bits:9>>) ->
    Vsn = case B of
              0 -> {2,5};
              1 -> exit(badVsn);
              2 -> 2;
              3 -> 1
          end,
    Layer = case C of
                0 -> exit(badLayer);
                1 -> 3;
                2 -> 2;
                3 -> 1
            end,
    %% Protection = D,
    BitRate = bitrate(Vsn, Layer, E) * 1000,
    SampleRate = samplerate(Vsn, F),
    Padding = G,
    FrameLength = framelength(Layer, BitRate, SampleRate, Padding),
    if
        FrameLength < 21 ->
            exit(frameSize);
        true ->
            {ok, FrameLength, {Layer,BitRate,SampleRate,Vsn,Bits}}
    end;
decode_header(_) -> exit(badHeader).
```
    神奇之处藏在这段代码第一行中，那个令人震惊的表达式里。


```erlang
decode_header(<<2#11111111111:11,B:2,C:2,_D:1,E:4,F:2,G:1,Bits:9>>) ->
```
    其中 `2#11111111111` 是个底数为 2 的整数，因此该模式会匹配 11 个连续的比特数 `1`，将 2 位匹配到 `B` 中，2 位匹配到 `C` 中，以此类推。请注意，这段代码完全遵循了早先给出的 MPEG 头部的位级规范。要写出更漂亮、更直接的代码会很难。这段代码优美而高效。Erlang 的编译器会将位语法的模式，转换为以最佳方式提取字段的高度优化代码。

- **解包 COFF 数据**

    数年前，我（决定）决定编写个构造可在 Windows 上运行的独立 Erlang 程序的程序 -- 我打算在任何可运行 Erlang 的机器上，构建出 Windows 的可执行文件。完成这点涉及理解及操作微软的通用对象文件格式，COFF，格式化的文件。了解 COFF 的细节相当困难，但 C++ 程序的各种应用程序接口都有记录。C++ 程序使用了 `DWORD`、`LONG`、`WORD` 及 `BYTE` 等的类型声明；这些类型声明对于那些编写过 Windows 内部程序的程序员，将不陌生。


```c
typedef struct _IMAGE_RESOURCE_DIRECTORY {
    DWORD Characteristics;
    DWORD TimeDateStamp;
    WORD MajorVersion;
    WORD MinorVersion;
    WORD NumberOfNamedEntries;
    WORD NumberOfIdEntries;
} IMAGE_RESOURCE_DIRECTORY, *PIMAGE_RESOURCE_DIRECTORY;
```
    要编写出我的 Erlang 程序，我（作者）首先定义了在 Erlang 源码文件中，必须包含的四个宏。


```erlang
-define(DWORD, 32/unsigned-little-integer).
-define(LONG, 32/unsigned-little-integer).
-define(WORD, 16/unsigned-little-integer).
-define(BYTE, 8/unsigned-little-integer).
```
    *注意*：宏会在 [8.17 节 “宏”](../part-ii/Ch08-the_rest_of_sequential_erlang.md#宏) 中解释。为扩展这些宏，我们使用了 `?DWORD`、`?LONG` 等语法。例如，宏 `?DWORD` 将扩展为字面文本 `32/unsigned-little-integer`。
    这些宏特意使用了与其 C 语言对应宏相同的名称。有了这些宏，我（作者）就可以轻松写出一些将图像资源数据，解包为二进制数据的代码。


```erlang
unpack_image_resource_directory(Dir) ->
    <<Characteristics : ?DWORD,
      TimeDateStamp : ?DWORD,
      MajorVersion : ?WORD,
      MinorVersion : ?WORD,
      NumberOfNamedEntries : ?WORD,
      NumberOfIdEntries : ?WORD, _/binary>> = Dir,
    ..
```
    当咱们比较 C 和 Erlang 的代码时，就会发现他们非常相似。因此，通过留意这些宏的名字及 Erlang 代码的布局，我们就能缩小 C 代码和 Erlang 代码间的语义差距，这会使我们的程序更易理解，更不易出错。
    下一步是解包 `Characteristics` 中的数据，等等。
    `Characteristics` 是个由标识集合组成的 32 位字。使用比特语法解包这些标识非常简单；我们只要这样写下代码即可：


```erlang
<<ImageFileRelocsStripped:1, ImageFileExecutableImage:1, ...>> =
<<Characteristics:32>>
```
    代码 `<<Characteristics:32>>` 将整数的 `Characteristics`，转换为了个 32 位的二进制值。然后，下面的代码将所需的那些位，解包到变量 `ImageFileRelocsStripped`、`ImageFileExecutableImage` 等中：


```erlang
<<ImageFileRelocsStripped:1, ImageFileExecutableImage:1, ...>> = ...
```
    同样，我（作者）保留了与 Windows API 中同样的名字，以便将规范与 Erlang 程序间的语义差距，降至最低。
    使用这些宏，使解包 COFF 格式数据变得......嗯，我（作者）真的不能用 *简单* 这个词，但代码尚可理解的。



- **解包 IPv4 数据报头部**

    这个示例说明了于单个模式匹配运算中，解析某个 Internet 协议版本 4，IPv4，的数据报：


```erlang
-define(IP_VERSION, 4).
-define(IP_MIN_HDR_LEN, 5).

...
DgramSize = byte_size(Dgram),
case Dgram of
    <<?IP_VERSION:4, HLen:4, SrvcType:8, TotLen:16,
      ID:16, Flags:3, FragOff:13,
      TTL:8, Proto:8, HdrChkSum:16,
      SrcIP:32,
      DestIP:32, RestDgram/binary>> when HLen >= 5, 4*HLen =< DgramSize ->
        OptsLen = 4*(HLen - ?IP_MIN_HDR_LEN),
        <<Opts:OptsLen/binary,Data/binary>> = RestDgram,
```
    这段代码以单个的模式匹配表达式，匹配某个 IP 数据报。其中的模式比较复杂，并说明了那些不在字节边界上的数据（例如，3 位和 13 位长的 `Flags` 和 `FragOff` 两个字段）。模式匹配后的这个 IP 数据报，就会在第二次模式匹配运算时，其报头和数据部分就会被提取到。
    现在我们已经介绍了对二进制值的位字段操作。请回想一下，二进制值必须是 8 位的倍数长。下一节会介绍用于存储比特序列的比特字串。


## 位串：处理位级数据


位串上的模式匹配，是在位级别上进行的，因此我们可以在一次运算中，将位的序列打包或解包为某个位串。在编写需要操作比特级数据，比如未按 8 位边界对齐的数据，或长度是以比特而非字节表示的变长数据代码时，这尤其有用。


我们可在 shell 中，说明位级别的数据处理。


```erlang
1> B1 = <<1:8>>.
<<1>>
2> byte_size(B1).
1
3> is_binary(B1).
true
4> is_bitstring(B1).
true
5> B2 = <<1:17>>.
<<0,0,1:1>>
6> is_binary(B2).
false
7> is_bitstring(B2).
true
8> byte_size(B2).
3
9> bit_size(B2).
17
```


> **位级存储**
>
> 大多数编程语言中，存储的最小可寻址单元都通常为 8 位宽。例如，大多数 C 的编译器，会将一个字符（存储的最小可寻址单元）定义为 8 位宽。操作某个字符内的位，会较为复杂，因为要访问单个的位，他们必须要掩码并移位到寄存器中。编写这样的代码既麻烦又容易出错。
>
> Erlang 下存储的最小可寻址单元是位，且某个位串中的单个位序列，可在无需任何移位和掩码运算下，即可直接访问。



在前面的示例中，`B1` 是个二进制值，而 `B2` 则是个位串，因为他是 17 位长。我们以语法 `<<1:17>>` 构造的 `B2`，且其被打印为作 `<<0,0,1:1>>`，即作为一个第三区段长度为 1 的位串的二进制字面值。`B2` 的位大小为 17，同时字节大小为 3（这实际上是包含这个位串的二进制值的大小）。


处理位串很麻烦。例如，我们无法将某个位字符串，写到某个文件或套接字（二进制值就可以），因为文件和套接字工作在字节单位下。


我们将以一个提取某个字节各个位的示例，结束这一小节。为此，我们将使用一种称为 *比特综合* 的新结构。比特综合之于二进制值，就如同列表综合之于列表。列表综合会迭代列表，而返回一些列表。比特综合则会迭代二进制值，并产生一些列表或二进制值。


下面这个示例，展示了如何从某个字节，提取一些比特位：


```erlang

1> B = <<16#5f>>.
<<"_">>
2> [ X || <<X:1>> <=B ].
[0,1,0,1,1,1,1,1]
3> << <<X>> || <<X:1>> <= B >>.
<<0,1,0,1,1,1,1,1>>
```


在第 1 行，我们构造了个包含单个字节的二进制值。`16#5f` 是个十六进制的常数。shell 会将其打印为 `<<"_">>`，因为 `16#f5` 是字符 `_` 的 ASCII 码。在第 2 行中，语法 `<<<X:1>>` 是个表示一个比特位的模式。结果就是该字节中，那些位的一个列表。第 3 行与第 2 行类似，只是我们从这些比特位，构造了个二进制值，而不是一个列表。


位综合的语法不会在此描述，但可在 [Erlang 参考手册](https://www.erlang.org/doc/system/reference_manual.html) 中找到。更多位串处理的示例，可在论文 ["Bit-Level Binaries and Generalized Comprehensions in Erlang"](https://dl.acm.org/doi/10.1145/1088361.1088363) （[这里](bincomp_erlang05.pdf) 直接查看）中找到。


现在我们明白了二进制值与位字符串。当我们想要管理大量非结构化数据时，二进制值就会在 Erlang 系统内部用到。在后面的章节中，我们将看到二进制值如何通过套接字在报文中发送，以及如何存储于文件中。


我们几乎就要完成顺序编程。剩下的是些小的主题；再也没有什么真正的基础知识，或令人兴奋的内容，但他们却是些有用的主题。


## 练习


1. 请编写个颠倒某个二进制值中，字节顺序的函数；

2. 请编写个返回由长度 4 字节的头部 `N`，后跟由调用 `term_to_binary(Term)` 生成的 `N` 字节数据，所组成的一个二进制值的函数 `term_to_packet(Term) -> Packet`；

3. 请编写上一函数逆过程的反函数 `packet_to_term(Packet) -> Term`；

4. 请以 [“添加测试到咱们的代码”](Ch04-modules_and_functions.md#将测试添加到咱们的代码) 的样式，编写一些测试前两个函数，是否能正确地将项编码成数据包，以及通过解码数据包，恢复原始项的测试；

5. 请编写一个倒转某个二进制值中位的函数。

