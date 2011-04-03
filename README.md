Autocompletion server
==============

This server takes about 2.6x more memory than the raw strings
themselves, uncompressed. It's very fast, and quite simple to use.

The server itself runs as an HTTP server on port 8080 (you can change
this in `Main.hs`) which serves autocompletions for words taken from
`/usr/share/dict/words`. All words have a score of 0; this score can
be any number you want to associate with a completion, such as its
likelihood.

To try it out, build and run it:

    $ cabal configure
    $ cabal build
    $ dist/build/acdaemon/acdaemon
    Serving on port 8080

Then point your web browser at http://localhost:8080/complete/lobst
and you will see something like this:

    [["lobster",0],["lobstering",0],["lobsterish",0],["lobsterlike",0],["lobsterproof",0]]

You can also get JSONP output if you include a `?callback=...`
argument, such as

http://localhost:8080/complete/lobst?callback=myCallback

How it works
----------

The strings for autocompletion are stored in a [JudySL
trie](http://judy.sourceforge.net/doc/JudySL_3x.htm), which is very
fast and space-efficient for doing prefix searches. The HTTP serving
is done with the fast and asynchronous
[Warp](http://www.yesodweb.com/blog/preliminary-warp-cross-language-benchmarks)
web server.
