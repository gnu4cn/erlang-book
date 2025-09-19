# 记录与映射


到目前为止，我们已讨论了两种数据容器，即 *元组* 和 *列表*。元组用于存储固定数量元素，而列表用于存储可变数量的元素。

本章引入了 *记录* 与 *映射*。记录其实就是变相的元组。使用记录，我们可为元组中的每个元素，关联一个名字。

映射是键值对的关联集。键可以是任何的 Erlang 项。在 Perl 和 Ruby 中，他们被称为哈希；在 C++ 和 Java 中，他们被称为映射；在 Lua 中，他们被称为表；在 Python 中，他们被称为字典。


使用记录和映射，会使编程变得更容易；无需记住复杂数据结构中，某个数据项的存储位置，我们只使用该数据项的名字，系统就能找出该数据的存储位置。记录使用一组固定的预定义名字；映射则可以动态地添加新的名字。


## 何时使用映射与记录


记录只是一些变相的元组，因此他们有着与元组同样的存储及性能特征。与元组相比，映射会使用更多存储空间，及更慢的查找属性。另一方面，映射则比元组灵活得多。


在以下情形下，应使用记录：


- 当咱们可使用固定数量的预定原子，表示咱们的数据时；
- 当记录中的元素数量及元素名字不会随时间改变时；
- 当存储是个问题，通常是当咱们有个大型元组数组，并且每个元组有着相同结构时。

映射适用于以下情况：


- 表示其中键值是事先不知道的键值数据结构；
- 表示有着大量不同键的数据；
- 作为一种其中效率并不重要，而使用方便重要的某种随处可见数据结构；
- 用于 “自文档化” 的数据结构，即用户可根据键名，猜到键值含义的那些数据结构；
- 用于表示键值对解析树，诸如 XML 或配置文件等；
- 用于与其他使用 JSON 的编程语言通信。


## 使用记录给元组项目命名


在某个小型元组中，记住单个元素表示什么几乎不会是个问题，但当元组中有大量元素时，给这些元素命名，就会变得很方便。一旦我们给元素命名后，我们将能使用名字引用他们，而不必记住他们在元组中的位置。

要给某个元组中的元素命名，我们有着如下语法的记录声明：


```erlang
-record(Name, {
                %% the next two keys have default values
                key1 = Default1,
                key2 = Default2,
                ...
                %% The next line is equivalent to
                %% key3 = undefined
                key3,
                ...
              }).
```

*警告*：`record` 不是个 shell 命令（在 shell 中要使用 `rr`；请参阅本节后面的说明）。记录声明只能在 Erlang 的源代码模组中使用，*不能* 在 shell 中使用。


在前面的示例中，`Name` 是该记录的名字。`key1`、`key2` 等等，是该记录中字段的名字；他们必须始终是一些原子。某个记录中的每个字段，都可以有个默认值，当创建该记录时，若该特定字段没有指定值，那么就会用到默认值。


例如，设想我们打算操作某个待办事项列表。我们会以定义一个 `todo` 记录开始，并将其存储在某个文件中（记录定义可包含在 Erlang 的源码文件中，也可以放在扩展名为 `.hrl` 的文件中，然后由 Erlang 源码文件包含）。


请注意，文件的包含，是确保多个 Erlang 模组使用同一记录定义的唯一方法。这类似于定义在 C 下的 `.h` 文件中，由源码文件包含的通用定义方式。有关包含指令的详情，请参阅 [8.15 节 “包含文件”](Ch08-the_rest_of_sequential_erlang.md#包含文件)。


[`records.hrl`](http://media.pragprog.com/titles/jaerlang2/code/records.hrl)


```erlang
-record(todo, {status=reminder,who=joe,text}).
```


一旦某个记录已被定义，该记录的实例即可被创建。


要在 shell 中完成此操作，在能定义记录前，我们必须将记录定义，读入到 shell 中。我们要使用 shell 函数 `rr`（*read records* 的缩写），完成这一操作。


```erlang
1> rr("records.hrl").
[todo]
```


### 创建与更新记录

现在我们以准备好定义和操作记录了。


```erlang
2> #todo{}.
#todo{status = reminder,who = joe,text = undefined}
3> X1 = #todo{status=urgent, text="Fix errata in book"}.
#todo{status = urgent,who = joe,text = "Fix errata in book"}
4> X2 = X1#todo{status=done}.
#todo{status = done,who = joe,text = "Fix errata in book"}
```


在第 2 和第 3 行中，我们 *创建了* 新记录。语法 `#todo{key1=Val1, ..., keyN=ValN}` 用于创建 `todo` 类型的新记录。键都是原子，且必须与记录定义中使用的键相同。在键被省略时，则默认值就会被假定为将来自记录定义中的值。


在第 4 行，我们 *复制了* 一条现有记录。语法 `X1#todo{status=done}` 表示创建一份 `X1`（必须是 `todo` 类型）的副本，将字段值 `status` 变更为 `done`。请记住，这会构造一份原始记录的拷贝；原始记录不会改变。


> **译注**：以下 shell 命令，反应了上述特性。


```erlang
5> X3 = #todo{status=urgent, desc="Fix errata in book"}.
* 1:27: field desc undefined in record todo
6> X1.
#todo{status = urgent,who = joe,text = "Fix errata in book"}
7> X2.
#todo{status = done,who = joe,text = "Fix errata in book"}
```


### 提取某个记录的字段


要在一次操作中，提取某个记录的多个字段，我们要用到模式匹配。



```erlang
8> #todo{who=W, text=Text} = X2.
#todo{status = done,who = joe,text = "Fix errata in book"}
9> W.
joe
10> Text.
"Fix errata in book"
```


在匹配运算符 (`=`) 左侧，我们以未绑定的变量 `W` 和 `Text`， 编写了个记录模式。在匹配成功时，这两个变量就会绑定到该记录中的相应字段。在我们只想要某个记录中的一个字段时，我们可使用 “点语法” 提取该字段。


```erlang
11> X1#todo.text.
"Fix errata in book"
```


### 函数中的记录模式匹配


我们可编写对记录字段模式匹配，并创建出新记录的函数。我们通常这样编写代码：


```erlang
clear_status(#todo{status=S, who=W} = R) ->
    %% Inside this function S and W are bound to the field
    %% values in the record
    %%
    %% R is the *entire* record
    R#todo{status=finished}
    %% ...
```


要匹配某个特定类型的记录，我们可以编写函数定义。


```erlang
do_something(X) when is_record(X, todo) ->
    %% ...
```


在 `X` 是条类型 `todo` 的记录时，这个子句就会匹配。



### 记录属于变相的元组


记录只是一些元组。


```erlang
7> X2.
#todo{status = done,who = joe,text = "Fix errata in book"}
```


现在，我们来让 shell 忘记 `todo` 的定义。


```erlang
12> rf(todo).
ok
13> X2.
{todo,done,joe,"Fix errata in book"}
```

在第 12 行，命令 `rf(todo)` 告诉了 shell 忘记 `todo` 这个记录的定义。因此，现在我们打印 `X2` 时，shell 就会将 `X2` 显示为一个元组。在内部就只有元组。记录属于一种语法上的便利，因此咱们可通过名字而不是位置，引用元组中的不同元素。



## 映射：关联的键值存储


映射是从 Erlang R17 版本开始可用的。


映射有着以下属性：


- 映射的语法与记录类似，不同之处在于省略了记录名字，而键值分隔符为 `=>` 或 `:=`；
- 映射是键值对的关联集合；
- 映射中的键，可以是任何 *完全着陆* 的 Erlang 项（完全着陆是指项中没有未绑定变量）；
- 映射中的元素，是依键排序的；
- 其中键值未变的映射更新，是种节省空间的操作；
- 查找映射中某个键的值，是种高效操作；
- 映射有着明确的顺序。


我们将在以下小节中，详细了解映射。



### 映射的语义学


映射字面值会以下语法书写：


```erlang
#{ Key1 Op Val1, Key2 Op Val2, ..., KeyN Op ValN }
```

这与记录的语法类似，但在哈希符号后并无记录名名字，且 `Op` 是 `=>` 或 `:=` 符号。


键和值可以是任何有效的 Erlang 项。例如，设想我们打算创建某个有两个键（`a` 和 `b`）的映射。


```erlang
1> F1 = #{ a => 1, b => 2 }.
#{a => 1,b => 2}
```

> **译注**：在 Erlang/OTP 28 下，只有 `=>` 才能用于映射构造。

```erlang
1> F1 = #{ a => 1, b := 2 }.
* 1:19: only association operators '=>' are allowed in map construction
2> F1 = #{ a := 1, b := 2 }.
* 1:11: only association operators '=>' are allowed in map construction
```


或者，设想我们打算创建一个不带原子键的映射。


```erlang
2> Facts = #{ {wife,fred} => "Sue", {age, fred} => 45,
   {daughter, fred} => "Mary",
   {likes, jim} => [football, swimming]}.
#{{age,fred} => 45,
  {daughter,fred} => "Mary",
  {likes,jim} => [football,swimming],
  {wife,fred} => "Sue"}
```


在内部，映射是作为有序集合存储的，无论映射表是如何创建出来，他们将始终按照键的排序被打印。下面是个示例：


```erlang
3> F2 = #{ b => 2, a => 1 }.
#{a => 1,b => 2}
4> F1 = F2.
#{a => 1,b => 2}
```


要对某个既有映射更新，我们要使用下面的语法，其中 `Op`（更新运算符）为 `=>` 或 `:=`：


```erlang
NewMap = OldMap # {K1 Op V1,...,Kn Op Vn}
```


表达式 `K => V` 用于两个目的，一个是以新值 `V` 更新某个现有键 `K` 的值，另一个是将一个全新 `K-V` 对，添加到该映射。此操作总是会成功。

而表达式 `K := V` 则用于以新值 `V` 更新某个现有键 `K` 的值。*在所更新的映射不包含键 `K` 时，此操作将会失败*。


```erlang
6> F3 = F2 # { b => 3}.
#{a => 1,b => 3}
7> F4 = F2 # { a := -10}.
#{a => -10,b => 2}
8> F5 = F2 # { c := 100 }.
** exception error: bad key: c
     in function  maps:update/3
        called as maps:update(c,100,#{a => 1,b => 2})
        *** argument 1: not present in map
     in call from erl_eval:'-expr/6-fun-0-'/2 (erl_eval.erl:470)
     in call from lists:foldl/3 (lists.erl:2466)
9> F5 = F2 # { c => 100 }.
#{c => 100,a => 1,b => 2}
```


使用 `:=` 操作符有两个很好的理由。首先，在我们拼错了新键的名字时，我们会希望发生错误。当我们创建了个映射 `Var = #{keypos => 1, ...}`，随后以 `Var #{key_pos := 2 }` 更新他，那么几乎可以肯定我们拼错了键名，我们会希望获悉这个问题。第二个原因与效率有关。当我们在某次映射更新操作中，只使用 `:=` 运算符时，那么我们就知道新旧映射有着相同键集，因此可以公用同样的键描述符。比如，在我们有个具有几百万映射的列表，且所有映射的键都相同时，那么节省的空间就会非常可观。


使用映射的最佳方式，是在某个键首次被定义时，始终使用 `Key => Val`，而在每次更改某个特定键的值时，使用 `Key := Val`。



### 映射字段的模式匹配


我们在 map 字面值中使用的 `=>` 语法，也可用作映射模式。和以前一样，某个映射模式中的键，不能包含任何未绑定变量，但值现在可包含变量，在模式匹配成功时，他们就会成为被绑定。


> **其他语言中的映射**
>
> 请注意，Erlang 中的映射工作方式，与许多其他编程语言中的等效结构有很大不同。要说明这一点，我们可以看看 JavaScript 中的情况。
>
> 设想我们要在 JavaScript 下完成下面的事情：

```javascript
var x = {status:'old', task:'feed cats'};
var y = x;
y.status = 'done';
```


> `y` 的值为对象 `{status:'done', task:'feed cats'}`。这里没有惊喜。但令人惊讶的是，`x` 已变成了 `{status:'done'，task:'feed cats'}`。这会让 Erlang 程序员大吃一惊。我们设法改变了变量 `x` 的一个字段的值，不是通过引用 `x`，而是通过给变量 `y` 的一个字段赋值。经由一个别名指针，更改 `X` 会导致许多可能很难调试的细微错误。
>
> 逻辑上等价的 Erlang 代码如下：

```erlang
D1 = {status=>old, task=>'feed cats'},
D2 = D1#{status := done},
```

> 在 Erlang 的代码中，变量 `D1` 和 `D2` 从未改变他们的初始值。`D2` 会像他是个 `D1` 的深拷贝那样行事。事实上，深拷贝并未发生；Erlang 系统只拷贝了内部结构中，维持拷贝已被创建假象的部分，因此创建某个对象的看似深度拷贝的操作，是一次非常轻量级的操作。



```erlang
1> Henry8 = #{ class => king, born => 1491, died => 1547 }.
#{died => 1547,class => king,born => 1491}
2> #{ born => B } = Henry8.
* 1:9: illegal pattern, did you mean to use `:=`?
3> #{ born := B } = Henry8.
#{died => 1547,class => king,born => 1491}
4> B.
1491
5> #{ D => 1547 }.
* 1:4: variable 'D' is unbound
```

> **译注**：可以看出，在 Erlang/OTP 28 下，映射字段的模式匹配时，只能使用 `:=` 运算符。


在第 1 行处我们创建了个包含亨利八世信息的新映射。在第 2 行，我们创建个从该映射中，提取与 `born` 键相关值的模式。该模式匹配会成功，同时 shell 会打印这整个映射的值。在第 3 行，我们打印了这个变量 `B` 的值。


在第 4 行处，我们试图找到某个值为 1547 的未知键（`D`）。但 shell 打印了个错误，因为映射中的所有键，都必须是些完全着陆项，而 `D` 是未定义的。


请注意，映射模式中键的数量，可以少于被匹配映射中键的数量。


我们可在函数头部，使用包含模式的映射，前提是该映射中的所有键都是已知的。例如，我们可以定义一个返回某个字符串中，特定字符出现次数映射的函数 `count_characters(Str)`。


```erlang
count_characters(Str) ->
    count_characters(Str, #{}).

count_characters([H|T], #{ H := N }=X) ->
    count_characters(T, X#{ H := N+1 });
count_characters([H|T], X) ->
    count_characters(T, X#{ H => 1 });
count_characters([], X) ->
    X.
```


下面是个示例：


```erlang
1> lib_misc:count_characters("hello").
#{101 => 1,104 => 1,108 => 2,111 => 1}
```


因此，字符 `h`（ASCII，101）出现了一次，以此类推。有关其中 `count_characters/2` 这个函数，有两点需要注意。在第一个子句中，该映射内部的变量 `H`，*还* 被定义在了这个映射外部，因此他是绑定的（按要求）。在第二个子句中，我们使用了 `map_extend` 为这个映射添加了一个新键。


> **译注**：
>
> 1. 其中行 `count_characters([H|T], #{ H := N }=X) ->` 处在编译时会报出错误，导致这段代码无法被编译。


```erlang
lib_misc.erl:71:28: variable 'H' is unbound
%   71| count_characters([H|T], #{ H := N }=X) ->
%     |                            ^

```

> 解决方法参考：[[Erlang] count_characters更正](https://blog.csdn.net/qq_44865780/article/details/105947621)
>
> 修订后的代码：


```erlang
count_characters(Str) ->
    count_characters(Str, #{}).

count_characters([H|T], X) ->
    case map:is_key(H, X) of
        false -> count_characters(T, X#{ H => 1 });
        true  -> #{ H := N } = X,
                 count_characters(T, X#{ H := N+1 })
    end;
count_characters([], X) ->
    X.
```


> 2. 其中 `h` 的 ASCII 代码为 104 而非 101。
>
> 参见：[Standard ASCII Table](https://www.ascii-code.com/ASCII)


### 对映射操作的 BIFs


有一些额外函数，对映射操作。他们是 `maps` 模组中的一些函数。


- `maps:new() -> #{}`

    返回一个新的空映射。

- `erlang:is_map() -> bool()`

    在 `M` 是个映射时，返回 `true`；否则返回 `false`。这可用作条件测试，或在函数体中使用。

- `maps:to_list() -> [{K1,V1},...,{Kn,Vn}]`

    将映射 `M` 中的键和值，转换为键和值的列表。结果列表中的键会按严格的升序排列。


- `maps:from_list([{K1,V1},...,{Kn,Vn}]) -> M`

    将成对元素的列表，转换为映射 `M`。若同一个键出现多次，则与列表中第一个键值关联的值将被使用，而后续值将被忽略。

- `maps:size(Map) -> NumberOfEntries`

    返回该映射中条目数量。

    > **译注**：原文此处有拼写错误，被写作了 `maps:map_size(Map) -> NumberOfEntries`。


- `maps:is_key(Key, Map) -> bool()`

    在该映射包含了某个键为 `Key` 的项目时，返回 `true`；否则返回 `false`。

- `maps:get(Key, Map) -> Val`

    返回映射表中与 `Key` 关联的值；否则会抛出一个异常。

- `maps:find(Key, Map) -> {Ok, Value}|error`

    返回映射表中与 `Key` 关联的值；否则，返回 `error`。

- `maps:keys(Map) -> [Key1,...,KeyN]`

    按升序返回该映射中的键的一个列表。


- `maps:remove(Key, M) -> M1`

    返回一个与 `M` 相同，但移除了有着键 `Key` （如果存在）项目的一个新映射 `M1`。

- `maps:without([Key1,...,KeyN], M) -> M1`

    返回一个新的，`M` 的一个副本，但移除了有着列表 `[Key1,...,KeyN]` 中键元素的新映射 `M1`。

- `maps:difference(M1, M2) -> M3`


    `M3` 等同于 `M1`，但移除了有着与 `M2` 中元素同样键的元素。


    其行为就好像他被定义如下：


```erlang
maps:difference(M1, M2) ->
    maps:without(maps:keys(M2), M1).
```


### 映射的排序


映射间的比较，会首先比较他们的大小，然后以键的排序，比较他们的键和值。


当 `A` 和 `B` 都是映射时，若 `maps:size(A) < maps:size(B)`，则 `A < B`。

若 `A` 和 `B` 是大小相等的两个映射，那么当 `maps:to_list(A) < maps:to_list(B)` 时，则 `A < B`。


因此，例如 `A = #{age => 23, person => "jim"}` 就小于 `B = #{email => "sue@somplace.com", name => "sue"}` 。这是因为 `A` 中最小的键（`age`）小于 `B` 中最小的键（`email`）。

> **译注**：在 Erlang shell 中验证如下。

```erlang
5> A = #{age => 23, person => "jim"}.
#{age => 23,person => "jim"}
6> B = #{email => "sue@somplace.com", name => "sue"}.
#{name => "sue",email => "sue@somplace.com"}
7> A < B.
true
8> A > B.
false
```



在将映射与其他 Erlang 项比较时，映射被视为要比列表或元组 “更复杂”，因此映射总是被认为大于列表或元组。

在 `~p` 选项下，映射可在 `io:format` 中输出，在 `io:read` 或 `file:consult` 中读取。



### JSON 桥

**The JSON Bridge**


熟悉 JSON 的人会注意到映射和 JSON 项之间的相似性。有两个 BIFs 可在映射和 JSON 项间转换。


- `maps:to_json(Map) -> Bin`

    会将某个映射，转换为包含该映射 JSON 表示形式的二进制值。二进制值将在 [第 7 章 “二进制及位语法 ”](Ch07-binaries_and_the_bit_syntax.md) 中讨论。请注意，并非所有映射，都能转换为 JSON 项。映射中的所有值，必须是可以用 JSON 表示的对象。因此，例如，值就不能包括 `funs`、PIDs、引用等等。在有任何键或值不能用 JSON 表示时，`maps:to_json` 会失败。

- `maps:from_json(Bin) -> Map`

    将包含 JSON 项的某个二进制值，转换为映射。

- `maps:safe_from_json(Bin) -> Map`


    将某个包含 JSON 项的二进制值，转换为映射。在这个 BIF 被调用前，`Bin` 中的任何原子都必须存在；否则将抛出一个异常。这样做是为避免创建处大量新原子。出于效率的原因，Erlang 不会对原子垃圾回收，因此持续增加新的原子，将在长时间后杀死 Erlang 虚拟机。

在前面的两个定义中，`Map` 都必须是 `json_map()` 类型的实例，其定义如下（稍后 [第 9 章 “类型”](Ch09-types.md) 中将介绍类型定义）：


```erlang
-type json_map() = [{json_key(), json_value()}].
```


其中：


```erlang
-type json_key() =
    atom() | binary() | io_list()
```

以及：


```erlang
-type json_value() =
    integer() | binary() | float() | atom() | [json_value()] | json_map()
```

JSON 对象与 Erlang 值间的映射如下：


- JSON *数字* 会表示位 Erlang 的整数或浮点数；
- JSON *字符串* 会表示为 Erlang 的二进制值；
- JSON *列表* 会表示为 Erlang 的列表；
- JSON 的 *true* 与 *false*，会表示为 Erlang 原子的 `true` 和 `false`；
- JSON *对象* 会表示位 Erlang 的映射，有映射中的键必须是原子、字符串或二进制值的限制，且值必须可表示为 JSON 项。


在我们转换 JSON 项时，我们应当心这种转换的某些限制。Erlang 提供了无限精度的整数。因此，Erlang 会很乐意将某个映射中的大数，转换为 JSON 项中的大数；而要解码这个 JSON 项的程序，则可能会理解，也可能不会理解。


在 [第 18 章 “使用 Websockets 和 Erlang 浏览”](../part-iv/Ch18-browsing_with_websockets_and_erlang.md) 中，咱们将了解如何使用结合了 JSON 项的映射，以及 websockets，提供一种与在 web 浏览器中运行应用程序通信的简单方法。


现在，我们已经介绍了在 Erlang 下创建复合数据结构的所有方法。我们知道了列表是可变数量项目的容器，而元组是固定数量项目的容器。记录用于将符号名字，添加到元组的元素，而映射则被用作关联的数组。


下一章中，我们将学习错误处理。在那之后，我们将回到顺序编程，然后再看看到目前为止我们省略了的二进制值及位语法。


## 练习


1. 配置文件可方便地表示为 JSON 项。请编写一些读取包含 JSON 项配置文件，并将其转换为 Erlang 映射的函数。编写一些对配置文件中数据执行正确性检查的代码；

2. 请编写一个函数 `map_search_pred(Map, Pred)`，返回该映射中 `Pred(Key, Value)` 为 `true` 的首个元素 `{Key,Value}`；

3. *复杂* 题目： 请查找 Ruby 哈希类的手册页面。将这个 Ruby 类中，咱们认为适合 Erlang 的方法构造为一个模组。
