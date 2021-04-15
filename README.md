# K Playground

A build system based on [Shake](https://shakebuild.com/) and Docker for type-checking and running programs written in toy languages implemented using the [K Framework](https://kframework.org/).

To get started, install [Stack](https://haskellstack.org/) and Docker, then run `./pre-build.sh` to install Shake. Next, run `./build.sh` to download the Docker image provided by the K team (which could take a while), compile the provided toy language, and run the tests.

You can now modify the provided toy language's syntax in `src/syntax/lambda-syntax.k`, and its typing rules in `src/typing-rules/lambda.k`. To typecheck an example program written in that toy language, pick a name, create the following files, and run `./build.sh` again.

* `src/example/<name>.lambda`: the example program
* `src/typing-rules/<name>.expected`: what you expect the type inference algorithm to output
* `src/typing-rules/<name>.depth`: limit on how many steps to run the type inference algorithm

After having run `./build.sh`, the Shake executable now knows about your example program. It then becomes possible to run the type inference algorithm on it step by step, which is useful for debugging:

```
$ .shake/build typing-rules/nine --depth=0
2 + 3 + 4

$ .shake/build typing-rules/nine --depth=1
2 + 3 ~> hole + 4

$ .shake/build typing-rules/nine --depth=2
2 ~> hole + 3 ~> hole + 4

$ .shake/build typing-rules/nine --depth=3
type(int) ~> hole + 3 ~> hole + 4

$ .shake/build typing-rules/nine --depth=4
type(int) + 3 ~> hole + 4

$ .shake/build typing-rules/nine --depth=5
3 ~> type(int) + hole ~> hole + 4

$ .shake/build typing-rules/nine --depth=6
type(int) ~> type(int) + hole ~> hole + 4

$ .shake/build typing-rules/nine --depth=7
type(int) + type(int) ~> hole + 4

$ .shake/build typing-rules/nine --depth=8
type(int) ~> hole + 4

$ .shake/build typing-rules/nine --depth=9
type(int) + 4

$ .shake/build typing-rules/nine --depth=10
4 ~> type(int) + hole

$ .shake/build typing-rules/nine --depth=11
type(int) ~> type(int) + hole

$ .shake/build typing-rules/nine --depth=12
type(int) + type(int)

$ .shake/build typing-rules/nine --depth=13
type(int)
```

Here is a more interesting example involving variables, whose types are stored in a context `Gamma`.

```
let f : int -> int -> int
      = \ x : int -> \ y : int -> x + y in f 1

<Typecheck>
  <k> let f : int -> int -> int
            = \ x : int -> \ y : int -> x + y in f 1
  </k>
  <ctx>
    .Map
  </ctx>
</Typecheck>

<Typecheck>
  <k> \ x : int -> \ y : int -> x + y
   ~> let f : int -> int -> int
            = hole in f 1
  </k>
  <ctx>
    .Map
  </ctx>
</Typecheck>

<Typecheck>
  <k> \ y : int -> x + y
   ~> .Map ~> type( int -> hole )
   ~> let f : int -> int -> int
            = hole in f 1
  </k>
  <ctx>
    x |-> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> x + y
   ~> x |-> int
   ~> type( int -> hole )
   ~> .Map ~> type( int -> hole )
   ~> let f : int -> int -> int
            = hole in f 1
  </k>
  <ctx>
    x |-> int
    y |-> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> x
   ~> hole + y
   ~> x |-> int ~> type( int -> hole )
   ~> .Map ~> type( int -> hole )
   ~> let f : int -> int -> int
            = hole in f 1
  </k>
  <ctx>
    x |-> int
    y |-> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int )
   ~> hole + y
   ~> x |-> int ~> type( int -> hole )
   ~> .Map ~> type( int -> hole )
   ~> let f : int -> int -> int
            = hole in f 1
  </k>
  <ctx>
    x |-> int
    y |-> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int ) + y
   ~> x |-> int ~> type( int -> hole )
   ~> .Map ~> type( int -> hole )
   ~> let f : int -> int -> int
            = hole in f 1
  </k>
  <ctx>
    x |-> int
    y |-> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> y
   ~> type( int ) + hole
   ~> x |-> int ~> type( int -> hole )
   ~> .Map ~> type( int -> hole )
   ~> let f : int -> int -> int
            = hole in f 1
  </k>
  <ctx>
    x |-> int
    y |-> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int )
   ~> type( int ) + hole
   ~> x |-> int ~> type( int -> hole )
   ~> .Map ~> type( int -> hole )
   ~> let f : int -> int -> int
            = hole in f 1
  </k>
  <ctx>
    x |-> int
    y |-> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int ) + type( int )
   ~> x |-> int ~> type( int -> hole )
   ~> .Map ~> type( int -> hole )
   ~> let f : int -> int -> int
            = hole in f 1
  </k>
  <ctx>
    x |-> int
    y |-> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int )
   ~> x |-> int ~> type( int -> hole )
   ~> .Map ~> type( int -> hole )
   ~> let f : int -> int -> int
            = hole in f 1
  </k>
  <ctx>
    x |-> int
    y |-> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int -> int )
   ~> .Map ~> type( int -> hole )
   ~> let f : int -> int -> int
            = hole in f 1
  </k>
  <ctx>
    x |-> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int -> int -> int )
   ~> let f : int -> int -> int
            = hole in f 1
  </k>
  <ctx>
    .Map
  </ctx>
</Typecheck>

<Typecheck>
  <k> let f : int -> int -> int
            = type( int -> int -> int ) in f 1
  </k>
  <ctx>
    .Map
  </ctx>
</Typecheck>

<Typecheck>
  <k> f 1
  </k>
  <ctx>
    f |-> int -> int -> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> f
   ~> hole 1
  </k>
  <ctx>
    f |-> int -> int -> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int -> int -> int )
   ~> hole 1
  </k>
  <ctx>
    f |-> int -> int -> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int -> int -> int ) 1
  </k>
  <ctx>
    f |-> int -> int -> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> 1
   ~> type( int -> int -> int ) hole
  </k>
  <ctx>
    f |-> int -> int -> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int )
   ~> type( int -> int -> int ) hole
  </k>
  <ctx>
    f |-> int -> int -> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int -> int -> int ) type( int )
  </k>
  <ctx>
    f |-> int -> int -> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int -> int )
  </k>
  <ctx>
    f |-> int -> int -> int
  </ctx>
</Typecheck>
```
