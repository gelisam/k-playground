# K Playground

A build system based on [Shake](https://shakebuild.com/) and Docker for type-checking and running programs written in toy languages implemented using the [K Framework](https://kframework.org/).

To get started, install [Stack](https://haskellstack.org/) and Docker, then run `./pre-build.sh` to install Shake. Next, run `./build.sh` to download the Docker image provided by the K team (which could take a while), compile the provided toy language, and run the tests.

You can now modify the provided toy language's syntax in `src/syntax/lambda-syntax.k`, and its typing rules in `src/typing-rules/lambda.k`. To typecheck an example program written in that toy language, pick a name, create the following files, and run `./build.sh` again.

* `src/example/<name>.lambda`: the example program
* `src/typing-rules/<name>.expected`: what you expect the type inference algorithm to output
* `src/typing-rules/<name>.depth`: limit on how many steps to run the type inference algorithm

After having run `./build.sh`, the Shake executable now knows about your example program. It then becomes possible to run the type inference algorithm on it step by step, which is useful for debugging:

<details>
<summary>Click to expand</summary>

```
$ .shake/build typing-rules/nine --depth=0
<k> 2 + 3 + 4
</k>

$ .shake/build typing-rules/nine --depth=1
<k> 2 + 3
 ~> hole + 4
</k>

$ .shake/build typing-rules/nine --depth=2
<k> 2
 ~> hole + 3
 ~> hole + 4
</k>

$ .shake/build typing-rules/nine --depth=3
<k> type(int)
 ~> hole + 3
 ~> hole + 4
</k>

$ .shake/build typing-rules/nine --depth=4
<k> type(int) + 3
 ~> hole + 4
</k>

$ .shake/build typing-rules/nine --depth=5
<k> 3
 ~> type(int) + hole
 ~> hole + 4
</k>

$ .shake/build typing-rules/nine --depth=6
<k> type(int)
 ~> type(int) + hole
 ~> hole + 4
</k>

$ .shake/build typing-rules/nine --depth=7
<k> type(int) + type(int)
 ~> hole + 4
</k>

$ .shake/build typing-rules/nine --depth=8
<k> type(int)
 ~> hole + 4
</k>

$ .shake/build typing-rules/nine --depth=9
<k> type(int) + 4
</k>

$ .shake/build typing-rules/nine --depth=10
<k> 4
 ~> type(int) + hole
</k>

$ .shake/build typing-rules/nine --depth=11
<k> type(int)
 ~> type(int) + hole
</k>

$ .shake/build typing-rules/nine --depth=12
<k> type(int) + type(int)
</k>

$ .shake/build typing-rules/nine --depth=13
<k> type(int)
</k>
```
</details>

Here is a more interesting example involving variables, whose types are stored in a context `Gamma`.

<details>
<summary>Click to expand</summary>

```
let f : int -> int -> int
      = \ x : int -> \ y : int -> x + y
in f 1

<Typecheck>
  <k> let f : int -> int -> int
            = \ x : int -> \ y : int -> x + y
      in f 1
  </k>
  <ctx>
    .Map
  </ctx>
</Typecheck>

<Typecheck>
  <k> \ x : int -> \ y : int -> x + y
   ~> let f : int -> int -> int
            = hole
      in f 1
  </k>
  <ctx>
    .Map
  </ctx>
</Typecheck>

<Typecheck>
  <k> \ y : int -> x + y
   ~> .Map ~> type( int -> hole )
   ~> let f : int -> int -> int
            = hole
      in f 1
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
            = hole
      in f 1
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
            = hole
      in f 1
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
            = hole
      in f 1
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
            = hole
      in f 1
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
            = hole
      in f 1
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
            = hole
      in f 1
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
            = hole
      in f 1
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
            = hole
      in f 1
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
            = hole
      in f 1
  </k>
  <ctx>
    x |-> int
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int -> int -> int )
   ~> let f : int -> int -> int
            = hole
      in f 1
  </k>
  <ctx>
    .Map
  </ctx>
</Typecheck>

<Typecheck>
  <k> let f : int -> int -> int
            = type( int -> int -> int )
      in f 1
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
</details>

And here is an even more interesting variant with type inference:

<details>
<summary>Click to expand</summary>

```
let f = \ x -> \ y -> x + y
in f 1

<Typecheck>
  <k> let f = \ x -> \ y -> x + y
      in f 1
  </k>
  <ctx>
    .Map
  </ctx>
</Typecheck>

<Typecheck>
  <k> \ x -> \ y -> x + y
   ~> let f = hole
      in f 1
  </k>
  <ctx>
    .Map
  </ctx>
</Typecheck>

<Typecheck>
  <k> \ y -> x + y
   ~> .Map ~> type( ?T1:Type -> hole )
   ~> let f = hole
      in f 1
  </k>
  <ctx>
    x |-> ?T1:Type
  </ctx>
</Typecheck>

<Typecheck>
  <k> x + y
   ~> x |-> ?T1:Type ~> type( ?T2:Type -> hole )
   ~> .Map ~> type( ?T1:Type -> hole )
   ~> let f = hole
      in f 1
  </k>
  <ctx>
    x |-> ?T1:Type
    y |-> ?T2:Type
  </ctx>
</Typecheck>

<Typecheck>
  <k> x
   ~> hole + y
   ~> x |-> ?T1:Type ~> type( ?T2:Type -> hole )
   ~> .Map ~> type( ?T1:Type -> hole )
   ~> let f = hole
      in f 1
  </k>
  <ctx>
    x |-> ?T1:Type
    y |-> ?T2:Type
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( ?T1:Type )
   ~> hole + y
   ~> x |-> ?T1:Type ~> type( ?T2:Type -> hole )
   ~> .Map ~> type( ?T1:Type -> hole )
   ~> let f = hole
      in f 1
  </k>
  <ctx>
    x |-> ?T1:Type
    y |-> ?T2:Type
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( ?T1:Type ) + y
   ~> x |-> ?T1:Type ~> type( ?T2:Type -> hole )
   ~> .Map ~> type( ?T1:Type -> hole )
   ~> let f = hole
      in f 1
  </k>
  <ctx>
    x |-> ?T1:Type
    y |-> ?T2:Type
  </ctx>
</Typecheck>

<Typecheck>
  <k> y
   ~> type( ?T1:Type ) + hole
   ~> x |-> ?T1:Type ~> type( ?T2:Type -> hole )
   ~> .Map ~> type( ?T1:Type -> hole )
   ~> let f = hole
      in f 1
  </k>
  <ctx>
    x |-> ?T1:Type
    y |-> ?T2:Type
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( ?T2:Type )
   ~> type( ?T1:Type ) + hole
   ~> x |-> ?T1:Type ~> type( ?T2:Type -> hole )
   ~> .Map ~> type( ?T1:Type -> hole )
   ~> let f = hole
      in f 1
  </k>
  <ctx>
    x |-> ?T1:Type
    y |-> ?T2:Type
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( ?T1:Type ) + type( ?T2:Type )
   ~> x |-> ?T1:Type ~> type( ?T2:Type -> hole )
   ~> .Map ~> type( ?T1:Type -> hole )
   ~> let f = hole
      in f 1
  </k>
  <ctx>
    x |-> ?T1:Type
    y |-> ?T2:Type
  </ctx>
</Typecheck>

<Typecheck>
  <k> type( int )
   ~> x |-> int ~> type( int -> hole )
   ~> .Map ~> type( int -> hole )
   ~> let f = hole
      in f 1
  </k>
  <ctx>
    x |-> int
    y |-> int
  </ctx>
</Typecheck>

...

<Typecheck>
  <k> type( int -> int -> int ) type( int )
  </k>
  <ctx>
    f |-> int -> int -> int
  </ctx>
</Typecheck>
```
</details>
