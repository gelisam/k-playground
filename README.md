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
int ~> hole + 3 ~> hole + 4

$ .shake/build typing-rules/nine --depth=4
3 ~> int + 3 ~> hole + 4

$ .shake/build typing-rules/nine --depth=5
int ~> int + 3 ~> hole + 4

$ .shake/build typing-rules/nine --depth=6
int + int ~> hole + 4

$ .shake/build typing-rules/nine --depth=7
int ~> hole + 4

$ .shake/build typing-rules/nine --depth=8
4 ~> int + hole

$ .shake/build typing-rules/nine --depth=9
int ~> int + hole

$ .shake/build typing-rules/nine --depth=10
int + int

$ .shake/build typing-rules/nine --depth=11
int
```
