# Haskell 课程作业文档

## 执行方式

在项目目录下执行 `stack run`，此时会进入一个 REPL。可用的命令如下：

- `:quit` 退出程序。
- `:env` 查看当前环境（已经绑定的全局变量的值和类型、声明的 ADT）。
- `:reset` 重设当前环境为空。
- `:run <file-path>` 读入并执行该文件。注意文件执行时的环境是运行这条指令的环境。如果文件没有问题，运行完成后的环境将会根据文件内容更新。为了实现简单，`file-path` 不能含有空格，也不能被引号引起，但是可以包括斜杠。
- `:type <expr>` 显示 `expr` 的类型。

此外，还可以在这个 REPL 中定义 ADT、进行变量绑定，以及对表达式求值。每次只能输入一行。程序的具体的语法示例见下一节。

## 实现的功能

已完成所有要求的基础功能，目前运行 `stack test` 会通过全部测试。此外还完成了：

- 代数数据类型支持以及模式匹配
- 一个简单的 REPL
- 易读性较好的文法设计和对应的 parser

下面是程序能够接受的代码示例，以及对应的说明。

**定义新的代数数据类型**：

```text
data MaybeInt = Nothing [] | Just [Int];
data EitherMaybeOrBoolFunc = Left [MaybeInt] | Right [Bool -> Bool];
```

分号是为了区分不同的编译单元，在 REPL 中不需要分号。构造子后面的类型被包括在中括号中是为了与模式匹配中的语法保持一致。

**匿名函数**、**模式匹配**、**绑定**：

```text
fakeBind =
  (a: MaybeInt) => (b: Int -> MaybeInt) => case a of
      Nothing [] -> Nothing
    | Just [x] -> b x;
```

模式匹配中 ADT 的模式需要中括号是为了将不带参数的构造子和变量绑定区分开（上面的例子中如果没有 `[]` 在 parsing 时会不知道 `Nothing` 到底是构造子还是变量），不同的模式使用 `|` 进行区分。*绑定* 指的是直接将变量加入到环境中，和 `let` 语句不同。

**函数应用** 和 Haskell 中的一样：

```text
fakeBind (Just 3) ((x: Int) => Just (x + 2));
```

**递归函数定义**：

```text
fac = letrec fac(x: Int): Int =
    if x == 0 then 1 else x * fac (x - 1) in fac;

fib = letrec fib(x: Int): Int =
    if x == 0 || x == 1 then 1 else fib (x - 1) + fib (x - 2) in fib;

isEven = letrec isEven(x: Int): Bool =
    if x == 1
    then false
    else
        if x == 0
        then true
        else isEven(x - 2)
    in isEven;
```

语法上所有的缩进都是不必要的。另外，所有的字面常量和 C 中一致；所有的操作符和 C 中一致；负整数需要用 `~` 作为负号。`let` 语句的语法是 `let x = expr in ...`，和 Haskell 中一致。

## 实现思路与亮点

### 模式匹配与类型检查

模式匹配的处理分成类型检查和求值。

对于表达式 `case e of pat_1 -> expr_1 | pat_2 -> expr2`，在类型检查时会先求出 `e` 的类型 T。然后将 `pat_1` 和 `pat_2` 与 T 进行匹配。如果某个分支不可能具有类型 T，则类型检查失败，否则，所有分支中的每个变量的类型应该都被匹配出来了。然后将这些已知类型的变量代入到 `expr_1` 和 `expr_2` 中求出各分支表达式的类型，如果一致的话就是整个模式匹配表达式的类型，否则类型检查失败。

求值过程类似，只用将类型检查中用模式去匹配类型换成了用模式去匹配值。

另外，类型检查过程都做到尽可能严格的检查，在求值过程中不再需要任何类型信息和类型检查。

### 函数闭包

为了将函数保存成为 value，同时让函数定义中引用的 free 变量在函数被调用时依然时定义时的值，需要实现函数闭包。

```haskell
type ValueMapping = StackMap String Value -- `StackMap` is an implementation of map
type Context = ([ADT], ValueMapping)

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VClosure Context String Expr
  | VData String [Value]
  | VUnit -- should only be used in REPL
  deriving (Eq)
```

函数闭包会保存函数参数的名字、定义函数时的环境、以及函数体。

当函数被应用时，求值使用的是闭包中保存的环境，而非求值时的环境。

### `LetRec` 语句实现

`LetRec` 语句会定义一个函数。问题在于，定义函数时需要创建闭包，需要传入当前环境 a；而定义的函数会被递归调用，因此环境 a 中需要包括自己对应的闭包，但是自己的闭包还没有创建出来（因为依赖 a），因此这是一个鸡与蛋的问题，二者相互依赖。

我不知道其它语言是怎么搞的，不过由于 Haskell 的求值是惰性的，这里可以直接写出来，而不会有任何问题（注意第三行）：

```haskell
ELetRec bind (arg, _) (body, _) expr -> do
    (adts, s) <- get
    let recContext = (adts, push (bind, VClosure recContext arg body) s)
    put recContext
    ret <- eval expr
    put (adts, s)
    return ret
```

---

另外实现过程中 parsing 部分有些比较麻烦的地方，不过似乎没什么技术含量。

## 参考资料与代码

Parsing 部分参考了 [https://wiki.haskell.org/Parsing_a_simple_imperative_language](https://wiki.haskell.org/Parsing_a_simple_imperative_language)。
