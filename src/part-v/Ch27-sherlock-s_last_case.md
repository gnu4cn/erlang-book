# 福尔摩斯的最后一案

![福尔摩斯](../images/sherlock.png)

“我们有的，只是该程序的一个片段，” Armstrong 说，“但我们对他来自哪里，关于什么毫无头绪。”

“给我看看，” Holmes 说。

“是这样的，” Armstrong 说，然后他向 Holmes 倾身，给他看了一张有些奇怪符号、括号和箭头，其间夹杂着一些英文文本的小纸条。


“那是什么？” Armstrong 问道。“Lestrade 说这是某种强大的黑魔法，来自未来。”

“这是计算机程序的一部分，” Holmes 抽着烟斗说道。“作为我时间旅行实验的一部分，我设法地传送了来自未来的一台计算机，及一大堆文件。在这些文件中，我发现了 Erlang 邮件列表中的 73,445 封邮件。我认为，通过将你那纸片上的文本，与这些邮件进行比对，我们将能发现这些奇怪符号的意义。但是怎么做呢？我拿出了我的斯特拉迪瓦里琴，演奏了一首帕格尼尼的曲子。然后我就想到了。邮件列表中与你纸条上文本最相似的帖子，必定是与文档中单词的 TF*IDF 分数余弦值相似度最大的......”


“这太棒了，” Armstrong 说，“但是 TF*IFD 分数是什么呢？”

“这是基本知识，亲爱的 Armstrong，” Holmes 说。 “这是项的频率，乘以逆文档频率。我要解释一下......”

## 找出数据相似性

在写作本文时，Erlang 邮件列表中有 73,445 封邮件。<sup>1</sup> 这表示大量可通过多种方式重用的知识。我们可用其回答有关 Erlang 的问题，或寻求灵感。

> *译注*：<sup>1</sup> `erlang-questions` 邮件列表已于 2022 年 4 月关闭。
>
> 参考：
>
> - [The erlang-questions Archives](http://erlang.org/pipermail/erlang-questions/)
>
> - [erlang-questions -- General Erlang/OTP discussions](http://erlang.org/mailman/listinfo/erlang-questions)


设想咱们正编写某个程序并需要帮助。这就是 Sherlock 发挥作用的地方。Sherlock 会分析咱们的程序，然后给出 Erlang 邮件列表中，所有可能会对咱们有所帮助的先前邮件里，最相似的帖子。


那么下面就是总体规划。

- 第 1 步是下载整个 Erlang 邮件列表，并将其存储在本地；
- 第 2 步是组织和解析所有邮件；
- 第 3 步是计算可用于相似性检索的邮件某些属性；
- 第 4 步是查询这些邮件，找出与咱们程序最相似的邮件。


这是这一章的核心思想。其中有很多变种。我们可下载所有曾编写过的 Erlang 模组，并检索他们的相似之处；或者我们可取得大量推文，或咱们感兴趣的任何大型数据集。


还有一种可用于帮助咱们对数据分类的变种。设想咱们刚写下一条简短笔记，并打算将其存储在某个文件中。确定文件名或存储文件的目录，是个难题。或许这与磁盘上的某个我（作者）几年前编写的某个别的文件非常相似，又或者其与我甚至不知道的其他某人编写的某个别的文档类似。在这两种情况下，我（作者）都希望从一个极大文档集中，找到与我的新文档最相似的文档。


当我们不知道相似之处为何时，Sherlock 会找出事物间的相似之处。我们会用他找出与 Erlang 邮件列表中那些贡献的相似之处。


在我们开始讨论实施细节前，我们将于实际操作中，展示 Sherlock 这个程序。



## 与 Sherlock 的一次对话


在这一小节中，我们将拿 Sherlock 来试用一下。首先，我们必须经由获取及分析 Erlang 邮件列表归档中的数据，初始化 Sherlock。完成此操作后，我们就将能够以各种方式，查询这些数据。

> **译注**：Sherlock 程序的源码可在 [`julienXX/erlang_exercices`](https://github.com/julienXX/erlang_exercices) 处下载。
>
> 代码目录结构为：
>
>
> ```console
> $ tree -L 2 ~/erlang-book/projects/ch27-code
> /home/hector/erlang-book/projects/ch27-code
> ├── _build
> │   └── default
> ├── elog4.config
> ├── erl_crash.dump
> ├── LOG
> ├── LOG_test
> ├── Makefile
> ├── rebar.lock
> └── src
>     ├── mochiweb_html.erl
>     ├── sherlock_app.erl
>     ├── sherlock.app.src
>     ├── sherlock_best.erl
>     ├── sherlock.erl
>     ├── sherlock_get_mails.erl
>     ├── sherlock.hrl
>     ├── sherlock_lib.erl
>     ├── sherlock_mails.erl
>     ├── sherlock_parse_mails.erl
>     ├── sherlock_similar.erl
>     ├── sherlock_tfidf.erl
>     └── text_analyzers.erl
>
> 4 directories, 19 files
> ```
>
> 这里已将 Makefile 根据现代 `rebar3` 构建工具做了修改。
>
> ```Makefile
> test_search:
> 	rebar3 compile
> 	erl -pa ./_build/default/lib/sherlock/ebin -s sherlock_mails test_search -s init stop > LOG_test
>
> test_build:
> 	rebar compile
> 	erl -pa ./_build/default/lib/sherlock/ebin -s sherlock_mails test_build -s init stop > LOG_build
> ```
>
> 在 `~/erlang-book/projects/ch27-code` 目录下，运行 `erl -boot start_sasl -config elog4 -smp +S 4 -pa _build/default/lib/sherlock/ebin` 命令，即可在 Erlang shell 下，执行这个程序的函数。


### 获取及预处理数据

这一小节中的某些命令用时很长，因此我们只会执行他们一次。首先我们要初始化系统。


```erlang
1> sherlock:init().
Making ${HOME}/.sherlock/mails
sherlock_cache_created
```

这将在咱们的主目录下，创建一个目录结构。当设置环境变量 `HOME` 未设置时，其将失败。 Sherlock 会把所有数据，存储在 `${HOME}/.sherlock` 这个顶级目录下的目录结构中。

```erlang
2> sherlock:fetch_index().
Written: /home/hector/.sherlock/mails/questions.html
Written: /home/hector/.sherlock/mails/questions.term
284 files must be fetched
ok
```

`fetch-index` 会获取 Erlang 邮件列表中邮件的索引。索引来自我们可从中提取到一个存储着那些邮件的所有文件名的列表的一个 HTML 文件。


```erlang
3> sherlock:fetch_mails().
fetching:"http://erlang.org/pipermail/erlang-questions/1997-January.txt.gz"
written:/home/hector/.sherlock/mails/cache/1997-January.txt.gz
fetching:"http://erlang.org/pipermail/erlang-questions/1997-May.txt.gz"
written:/home/hector/.sherlock/mails/cache/1997-May.txt.gz
...
```

一旦我们知道了所有文件名，我们所要做的就是去抓取他们。各个文件都将年份和月份嵌入了文件名。我们把获取的所有文件，都存储在名为 `${HOME}/.sherlock/mail/cache` 的目录下。只要我们想要运行程序，这些数据都应保留着。当我（作者）写下这一章并运行前面的命令时，我下载了 176 个文件与 37MB 的压缩邮件数据，这代表 73,445 封单独邮件。

> **译注**：译者下载到截至 2022 年 3 月该邮件列表关闭时，283 个文件 54M 的数据。

我们可询问 Sherlock 已收集到的数据。


```erlang
4> sherlock:mail_years().
["1997","1998","1999","2000","2001","2002","2003","2004",
 "2005","2006","2007","2008","2009","2010","2011","2012",
 "2013","2014","2015","2016","2017","2018","2019","2020",
 "2021","2022"]
```

这表明我们设法下载到了 1997 年至 2022 年的邮件。

下一步是将这些数据，划分（分区）到年份。我们会对每一年，创建一个新目录。因此，例如，2007 年的数据会被存储在目录 `${HOME}/.sherlock/mail/2007` 下，依此类推。然后，对于每一年，我们都会收集当年的所有邮件，并将结果写入相应目录。完成这一操作后，我们解析并后处理每年的数据。


```erlang
5> sherlock:process_all_years().
Parsing mails for: 1997
Parsing: 1997-January.txt.gz
Parsing: 1997-May.txt.gz
Written: /home/hector/.sherlock/mails/1997/parsed.bin
Year: 1997 #entries = 3 size =   0.00 Megabytes average =354.33 bytes/engtry
Computing mail IDF for: 1997
Adding synthetic keywords for:1997
Written binary store:/home/hector/.sherlock/mails/1997/mails.bin
Written listing:"/home/hector/.sherlock/mails/1997/mails.list"
1997 had 3 mails
Parsing mails for: 1998
Parsing: 1998-December.txt.gz
Written: /home/hector/.sherlock/mails/1998/parsed.bin
Year: 1998 #entries = 61 size =   0.03 Megabytes average =537.15 bytes/engtry
Computing mail IDF for: 1998
Adding synthetic keywords for:1998
Written binary store:/home/hector/.sherlock/mails/1998/mails.bin
Written listing:"/home/hector/.sherlock/mails/1998/mails.list"
1998 had 61 mails
Parsing mails for: 1999
Parsing: 1999-January.txt.gz
Parsing: 1999-February.txt.gz
Parsing: 1999-March.txt.gz
Parsing: 1999-April.txt.gz
...
```

这一操作会耗时数分钟，并得到 17MB 的数据。我们只必须执行一次此操作。


### 找出于某个给定文件最相似的邮件

现在我们准备好检索这些数据了。查询项是 *我（作者）正在其上工作的文件*。当我开发这个程序时，我正在编写一个名为 `sherlock_tfidf` 的模组。我很好奇的想知道，我的搜索引擎是否有效，因此我输入了以下查询：

```erlang
1> sherlock:find_mails_similar_to_file("2009", "./src/sherlock_tfidf.erl").
** searching for a mail in 2009 similar to the content of file:./src/sherlock_tfidf.erl
Searching for=[<<"idf">>,<<"word">>,<<"remove">>,<<"words">>,<<"tab">>,
               <<"duplicates">>,<<"ets">>,<<"keywords">>,<<"bin">>,<<"skip">>,
               <<"file">>,<<"index">>,<<"binary">>,<<"frequency">>,<<"dict">>]
7260 : 0.27 Word Frequency Analysis
7252 : 0.27 Word Frequency Analysis
7651 : 0.18 tab completion and word killing in the shell
4297 : 0.17 ets vs process-based registry + local vs global dispatch
5324 : 0.16 ets memory usage
5325 : 0.15 ets memory usage
1917 : 0.14 A couple of design questions
1860 : 0.12 leex and yecc spotting double newline
5361 : 0.11 dict slower than ets?
1991 : 0.11 Extending term external format to support   shared substructures
[7260,7252,7651,4297,5324,5325,1917,1860,5361,1991]
```

这一查询请求 Sherlock 在 2009 年的邮件中检索，与 `sherlock_tfidf.erl` 这个文件内容类似的帖子。


输出很有趣。首先，Sherlock 认为 `sherlock_tfidf.erl` 最好以关键字 `idf`、`word` 等描述。这些关键字被列在 `Searching for` 行中。这后面，Sherlock 检索了 2009 年发布的所有邮件，查找这些关键字。输出如下所示：

```console
7260 : 0.27 Word Frequency Analysis
7252 : 0.27 Word Frequency Analysis
7651 : 0.18 tab completion and word killing in the shell
4297 : 0.17 ets vs process-based registry + local vs global dispatch
...
```

每行都有一个邮件索引编号，后跟相似权重值于一个邮件主题行。相似度权重，是个 0 到 1 之间的数字。1 表示文档极为相似。 0 表示无相似之处。得分最高的邮件，为邮件编号 7260，有着相似度权重值 0.27。这可能很有趣，因此我们将更详细地了解他。


```erlang
2> sherlock:print_mail("2009", 7260).
----
ID: 7260
Date: Fri, 04 Dec 2009 17:57:03 +0100
From: =?ISO-8859-15?Q?Johann_H=F6chtl?=
Subject: Word Frequency Analysis
Hello!

I need to compute a word frequency analysis of a fairly large corpus. At
present I discovered the disco database
http://discoproject.org/

which seems to include a tf-idf indexer. What about couchdb? I found an
article that it fails rather quickly (somewhere between 100 and 1000
wikipedia text pages)
...
```

当我（作者）第一次运行这个程序时，我非常兴奋。在我不曾告诉系统查找 TF*IDF 索引时，系统就已发现一封谈论 TF*IDF 索引的邮件。我只是说了 “找出与我文件中的代码类似的任何邮件。”


那么，现在我知道邮件 7260 很有趣。也许有一些与此邮件类似的邮件。我们可以问问夏洛克。


```erlang
3> sherlock:find_mails_similar_to_mail("2009", "2009", 7260).
Searching for a mail in 2009 similar to mail number 7260 in 2009
Searching for=[<<"indexer">>,<<"analysis">>,<<"couchdb">>,<<"wortd">>,
               <<"idf">>,<<"knuthellan.com">>,<<"frequncy">>,<<"corpus">>,
               <<"dbm">>,<<"discoproject.org">>,<<"disco">>]
7260 : 0.84 Word Frequency Analysis
7252 : 0.84 Word Frequency Analysis
6844 : 0.21 couchdb in Karmic Koala
6848 : 0.21 couchdb in Karmic Koala
6847 : 0.20 couchdb in Karmic Koala
6849 : 0.19 couchdb in Karmic Koala
7264 : 0.17 Re: erlang search engine library?
6843 : 0.16 couchdb in Karmic Koala
2895 : 0.15 CouchDB integration
69 : 0.14 dialyzer fails when using packages and -r
[7252,6844,6848,6847,6849,7264,6843,2895,69]
```

这次我（作者）检索了某封与邮件 7260 相似的邮件。邮件 7252 有着最高的相似度得分，且证实与 7260 非常相似，但邮件 7264 则最终被发现更有趣。光靠相似度分数，不足以确定哪个文件最有趣；系统仅给出了可能相似的那些文件。实际上我们必须查看结果，并选择我们认为最有趣的帖子。

自 `sherlock_tfidf.erl` 的代码开始，我（作者）发现了 [`disco`](http://discoproject.org/) 这个数据库有个 TF*IDF 索引器。几次查询后，我发现了一个到 [`couchdb`](https://couchdb.apache.org/) 的连接。夏洛克为我找出了要调查的各种有趣事情。


### 按特定作者、日期或主题检索邮件


我们还可对数据执行所谓的 *分面* 检索。所谓文档的一个方面，是比如文档中的用户名或主题。所谓分面检索，是在特定字段或字段集内的检索。解析后的文档，会被表示为一些 Erlang 记录。我们可在任何这些字段上，执行一次所有文档的特定检索。下面是个分面检索的示例：

> *知识点*：
>
> - faceted search
>
> 参考：[Faceted search](https://en.wikipedia.org/wiki/Faceted_search)


```erlang
1> sherlock:search_mails_regexprs("2009", "*Armstrong*", "*Protocol*", "*").
946:  UBF and JSON Protocols
5994:  Message protocol vs. Function call API
Query took:4 ms #results=2
[946,5994]
```

这个命令会在 2009 年邮件中，检索其作者与正则表达式 `"*Armstrong*"` 匹配、主题行与 `"*Protocol*"` 匹配，以及任意内容的项目。其中有两个匹配项：邮件 946 与 5994。我们像这样检查其中第一个：

```erlang
2> sherlock:print_mail("2009", 946).
----
ID: 946
Date: Sun, 15 Feb 2009 12:39:10 +0100
From: Joe Armstrong
Subject: UBF and JSON Protocols
For a long time I have been interested in describing protocols. In
2002 I published a contract system called UBF for defining protocols.
...
```

如此等等。此时此刻，我们可进行更多查询，检索特定数据，或找出与前面这些帖子类似的帖子。


现在我们已看到了行动中的 Sherlock，我们将看看其实现。


## 数据分区/分片的重要性


我（作者）现在将告诉咱们一个秘密。数据分区，是程序并行化的关键。我们用以构建数据存储的 `process_all_years()`，被定义如下：


```erlang
process_all_years() ->
    ...
    L = [process_year(I) || I <- mail_years()],
    ...
```

`process_year(Year)` 处理某个特定年份的所有数据，`mail_years/0` 会返回一个年份列表。

要并行化这个程序，咱们要做的就是修改 `process_all_years` 的定义，并调用我们在 [26.3 小节，*并行化顺序代码*](Ch26-programming_multicore_CPUs.md#并行化序列代码) 中，讨论过的 `pmap`。在这个小更改下，我们的函数看起来像下面这样：


```erlang
    ...
    L = lib_misc:pmap(fun(I) -> process_year(I) end, mail_years()),
    ...
```

> **译注**：
>
> 在以命令 `erl -boot start_sasl -config elog4 -smp +S 4 -pa _build/default/lib/sherlock/ebin`（注意命令行参数 `-smp +S 4`）启动的 Erlang shell 中， 执行并行化版本的 `sherlock:process_all_years/0` 结果如下。
>
> ```console
> ...
> 102150 files in 25 seconds (4086.00 files/second)
> ok
> ```
>
> 同时任务管理器中 CPU 使用情况截图如下。
>
> ![在 `lib_misc:pmap` 及 `-smp +S 4` 的 `erl` 命令行参数下的 CPU 使用情况](../images/with_pmap_and_smp_4.png)
>
> 而原版的 `sherlock:process_all_years/0` 的结果如下。
>
> ```console
> ...
> 102150 files in 64 seconds (1596.09 files/second)
> ok
> ```
>
> 同时任务管理器中 CPU 使用情况截图如下。
>
> ![在未使用 `lib_misc:pmap` 及无 `-smp +S 12`  `erl` 命令行参数下的 CPU 使用情况](../images/no_pmap_and_smp.png)
>
>
> 而在以命令 `erl -boot start_sasl -config elog4 -smp +S 12 -pa _build/default/lib/sherlock/ebin` （注意 `-smp +S 12` 命令行参数）启动的 Erlang shell 中，并行化版本的 `sherlock:process_all_years_parall/0` 的输出如下。
>
> ```console
> ...
> 102150 files in 24 seconds (4256.25 files/second)
> ok
> ```
>
> 同时任务管理器中 CPU 使用情况截图如下（译者计算机为 12 核心）。
>
> ![在 `lib_misc:pmap` 及 `-smp +S 12` 的 `erl` 命令行参数下的 CPU 使用情况](../images/with_pmap_and_smp_12.png)
>
> 可以看出使用 `lib_misc:pmap/2` 与使用 `-smp +S 12` 命令行参数后，带来了 3 倍的速度提升。当将 `-smp +S n` 命令行参数 `n`，设置到与机器核心数一样时，会利用到全部的 CPU 核心。

这里有个额外的不太明显好处。*要测试我们的程序运作，我们只能以其中一个年份进行*。当我（作者）有台十七核或更多核的怪物机器时，我就可以并行运行全部年份的计算。这里有 17 年（新的 26 年）的数据，所以我（作者）至少需要十七个 CPU，以及允许十七个并行输入操作的磁盘控制器。现代固态磁盘有多个磁盘控制器，但我是在一台有着传统磁盘，和双核心处理器的机器上写这篇文章，所以我不能指望当我并行化该程序时的大幅提速。


我（作者）一直在经由分析 2009 年的数据，测试我的程序。当我像要加快该程序速度时，我就需要某台怪物机器的访问，我只需通过将那个顶级的列表综合更改为 `pmap`，即会并行化这个程序。


这种形式的并行化，正是一种 Map-Reduce 架构中用到的形式。要快速搜索邮件数据，我们就要使用十七台机器，每台查看一年的数据。我们将同一查询，分配到所有十七台机器，然后收集结果。这就是 `map-reduce` 中的主要思想，同时其曾在 [26.5 小节，*使用 `mapreduce` 的计算并行化*](Ch26-programming_multicore_CPUs.md#mapreduce-下的计算并行化) 中讨论过。

咱们会注意到，这一章中的所有示例，都使用了 2009 年作为基准年。 2009 年有足够多的邮件，训练所有软件（2009 年有 7,906 封邮件），而处理这么多邮件，并没有耗时太长，这在开发软件时很重要，因为程序必须更改并运行很多次。

由于我（作者）已将数据组织未一些独立的年份集合，所以我需要做的就是，编写代码一个年份的代码。我清楚在有必要时，我的程序可被简单的修改，以在更强大机器上运行。

## 添加关键字到帖子


当我们查看 Erlang 邮件列表中的帖子时，咱们将发现他们没有任何关键字。但即便他们有，还有个更根本的问题。两个不同人可能阅读着同一文档，但对于应该使用哪些关键字描述该文档，会存在分歧。因此，当我（作者）执行一次关键字检索时，若文档作者选择的关键字，不同于我选择用于搜索文档的关键字时，检索就将不起作用。

Sherlock 会计算邮件列表中每个帖子的一个关键字矢量值。我们可询问 Sherlock，2009 年发布 946 号帖子的关键词矢量值为何。


```erlang
1> sherlock:get_keyword_vector("2009", 946).
[{"protocols",0.6983839995773734},
 {"json",0.44660371850799946},
 {"ubf",0.38889626945542854},
 {"widely",0.2507841072279312},
 {"1.html",0.17649029959852883},
 {"recast",0.17130091468424488},
 ...
```


所谓关键字矢量值，是派生关键字与该单词在文档中显著性的一个列表。文档中单词的显著性，实际上就是该单词在文档中的 TF*IDF 权重值。这是个从 0 到 1 的数字，其中 0 表示该词不显著，1 表示该词非常显著。


要计算两个文档间的相似度，我们就要计算每个文档的关键字矢量值，然后计算这些关键字矢量值的归一化叉积。这被称为文档的 *余弦相似度*。当两个文档有着重叠的关键词时，那么他们在一定程度上是相似的，而余弦相似度，就是这方面的一种量度。

> *知识点*：
>
> - the significance of the word in the document
>
> - the TF*IDF weight of the word in the document
>
> - the normalized cross product of the keyword vectors
>
> - the cosine similarity of the documents
>
> 参考：
>
> - [`tf-idf`](https://en.wikipedia.org/wiki/Tf-idf)
>
> - [Cosine similarity](https://en.wikipedia.org/wiki/Cosine_similarity)


现在我们将看一下这种算法的细节。


### 单词的意义：TF*IDF 权重

衡量单词重要性的一种常用方法，便是所谓的 TF*IDF 权重。 TF 代表 *term frequncy*，IDF 代表 *inverse document frequency*。许多搜索引擎，都使用了文档中单词的 TF*IDF 权重，对单词的重要性排名，并找出某个集合中的相似文档。在这一小节中，我们将了解 TF*IDF 权重是如何计算的。


在描述检索方法的文献中，咱们会发现 *语料库，corpus* 这个词被大量使用。所谓语料库，是一个很大的参考文档集合。在我们的情形下，语料库则是 Erlang 邮件列表的 73,445 封邮件的集合。


要计算 TF*IDF 权重，我们需要做的第一件事，是将我们感兴趣的文档拆分为一个单词序列。我们将假设单词是一些由以非字母字符分隔的字母字符序列。现在设想我们在某个特定文档中，找到单词 *socket*；这个词是否显著，取决于该词的上下文。在评估单个单词重要性前，我们将需要分析大量文档。

假设单词 *socket* 出现于 %1 的语料库所有文档中。通过分析语料库中的全部文档，并计算有多少文档包含 *socket* 这个单次，我们便可计算出这一结果。


当我们查看某单个文档时，他可能同样包含了 *socket* 这个单词。文档中包含某个单词的次数，除以该文档中单词的总数，就叫做该单词的 *词频，term frequency*。因此，当某个文档包含单词 *socket* 五次，并且该文档有一百个单词时，则单词 socket 的词频即为百分之 5。当语料库中单词 *socket* 的频率为百分之 1 时，那么我们文档使用该单词 5% 的时间，这一事实就高度显著，因此我们就会选择 *socket*，作为我们可将其与该文档关联的关键字。当我们文档中的词频为百分之 1 时，那么他就与母语料库中的出现概率相同，而因此其有着小的显著性。

文档中某个单词的 *词频，term Frequency, TF*，就是该单词在文档中出现的次数，除以文档中单词的总数。

而某个单词 `W` 的 *逆文档频率，inverse document frequency, IDF*，被定义为 `log(Tot/(N+1))` ，其中 `Tot` 是语料库中文档的总数，`N` 是包含 `W` 这个单词的文档数。

例如，设想我们有个 1,000 文档的语料库。假设 *orange* 一词出现于 25 个文档中。单词 *orange* 的 IDF 便是 `log(1000/26) (= 1.58)`。当单词 *orange* 出现于某个 100 单词的文档中时，那么 *orange* 的 TF 便是 `10/100 = (0.1)`，而这个单词的 TM*IDF 权重，便是 0.158。


要找出某个特定文档的一组关键字，我们就要计算该文档中，每个单词的 TF*IDF 权重，然后取那些有着最高 TF*IDF 权重的单词。我们会略去任何带有非常低 TF*IDF 权重的单词。

在这些准备工作都顾及到了下，现在我们就可以计算文档中的哪些单词，属于好的关键字。这是个两遍过程。第一遍计算语料库中每个单词的 IDF。第二遍计算语料库中各个文档的关键字。


### 余弦相似度：两个加权矢量值的相似度


给出两个关键字的矢量值，我们就可以一个简单示例，计算余弦相似度。假设我们有两个关键字的矢量值：`K1` 的关键字 `a`、`b` 和 `c`，`K2` 的关键字 `a`、`b` 和 `d`。这些关键字及其关联的 `TF*IDF` 权重如下：


```erlang
1> K1 = [{a,0.5}, {b,0.1}, {c,0.2}].
[{a,0.5},{b,0.1},{c,0.2}]
2> K2 = [{a,0.3}, {b,0.2}, {d,0.6}].
[{a,0.3},{b,0.2},{d,0.6}]
```


`K1` 和 `K2` 的叉积，为有着同样关键字的实体，权重乘积之和。


```erlang
3> Cross = 0.5*0.3 + 0.1*0.2.
0.16999999999999998
```


而要计算余弦相似度，我们就要将这个叉积，除以每个向量的范数。所谓向量的范数，是权重平方和的平方根。


```erlang
4> Norm1 = math:sqrt(0.5*0.5 + 0.1*0.1 + 0.2*0.2).
0.5477225575051662
5> Norm2 = math:sqrt(0.3*0.3 + 0.2*0.2 + 0.6*0.6).
0.7
```

余弦相似度，便是归一化的叉积。


```erlang
6> Cross/(Norm1*Norm2).
0.4433944513137058
```

这个过程，已被纳入一个库函数中。

```erlang
7> sherlock_similar:cosine_similarity(K1, K2).
0.4433944513137058
```


两个关键词的矢量值余弦相似度，是个 0 到 1 之间的数字。1 表示两个向量完全一致。 0 表示他们没有相似之处。


### 相似度查询

我们已解释了进行相似性查询的所有准备工作。首先我们要计算语料库中所有单词的 IDF；然后我们要计算语料库中各个文档的关键字相似度的矢量值。这些计算可能耗时很长，但这并不重要，因为他们只必须执行一次。

要进行一次相似性查询，我们要取得查询文档，并使用语料库的 IDF 计算其关键字的向量。然后我们要计算语料库中，每个文档的余弦相似度，并选取有着最大余弦相似系数的那些值。


我们会列出结果，用户可以选择他们认为最感兴趣的那些文档。他们可以检查这些文档，并可能根据先前分析的结果，检索更多的文档。


## 实现概述


Sherlock 的全部代码，都存储在本书主页的 `code/sherlock` 目录中（译注：请参阅前面的译注）。这一小节提供了该实现的一种顶层概述。数据处理的主要阶段如下：


- *初始化数据存储*

    在最开始时，`sherlock_mail:ensure_mail_root/0` 会被调用。这个例程确保目录 `${HOME}/.sherlock` 存在。我们将把所有数据，存储在名为 `${HOME}/.sherlock/mails` 的目录下；在接下来的文本中，我们将把这个目录称为 `MAIL`。

- *获取邮件索引*

    第一步是获取 [The erlang-questions Archives](http://erlang.org/pipermail/erlang-questions/) 处的数据。这是使用 `inets` 这个HTTP 客户端完成的，其是 Erlang 发行版的一部分。 `sherlock_get_mails:get_index/1` 会获取那些邮件，而 `sherlock_get_mails:parse_index/2` 则会解析这些邮件。自服务器获取到的 HTML 文件，会被存储在 `MAIL/questions.html` 中，而解析结果，则是在文件 `MAIL/questions.term` 这个文件中。

- *获取原始数据*

    `sherlock_mails:fetch_all_mails/0` 会获取所有邮件。他以读取 `MAIL/questions.term` 开始；他会获取所有压缩的邮件文件，并将他们存储在目录 `MAIL/cache` 中。

- *数据分片/分区*

    `sherlock_mails:find_mail_years/0` 会分析邮件缓存中的那些文件，并返回一个已恢复邮件的年份列表。

+ *处理某一给定年份的数据*

    `sherlock_mails:process_year(Year)` 完成三件事。他会解析给定年份的所有数据；他会计算该年份中所有帖子的 TF*IDF 权重值；并会将一些合成关键字，添加到各个帖子。

    以下数据文件会被创建：

    - `MAIL/Year/parsed.bin` 包含一个二进制值 `B`，其中 `binary_to_term(B)` 包含 `#post` 的记录列表；
    - `MAIL/Year/idf.ets` 是个存储着一些 `{Word,Index,Idf}` 形式元组的 ETS 数据表，其中 `Word` 是个二进制值（某封邮件中单词之一），`Index` 是个整数的索引，`Idf` 是该单词的 IDF 权重值；
    - `MAIL/Year/mails.bin` 包含一个二进制值 `B`，其中 `binary_to_term(B)` 包含着一个 `#post` 的记录列表。这次的记录已扩充了合成关键字；
    - `MAIL/Year/mails.list` 是 `mails.bin` 中，头几个条目的一个列表。这个数据文件在开发这个程序时会用到；偶尔检查输出，并检查结果是否如预期，是很有用的。

    这一步中的工作，是在几个不同地方完成的。`sherlock_tfidf.erl` 会计算 TF*IDF 权重。`sherlock_mails:parse_mails/1` 会解析压缩的邮件文件。`text_analyzers:standard_analyzer_factory/2` 这会将某个二进制值，转换为一个单词列表。

- *执行相似度查询*

    `sherlock_mails:find_mails_similar_to_binary/2` 完成了大部分工作。他会读取 `MAIL/Year/idf.ets` 并使用该数据文件，计算该二进制值的关键字矢量值（这是经由调用 `sherlock_tfidf:keywords_in_binary/2` 完成的）。然后他会对 `MAIL/Year/mails.bin` 中的所有条目迭代，而每个条目都包含了个关键字的矢量值。`sherlock_similar:cosine_similarity/2` 会计算关键字矢量值的相似度，`sherlock_best.erl` 会留存一个那些最具显著性帖子的列表。

- *执行分面检索*

    `sherlock_mail:search_mails_regexp/4` 会迭代条目 `MAIL/Year/mails.bin`，并对 `#post` 记录中的各个元素，执行正则表达式的检索。



