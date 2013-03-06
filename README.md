# Introduction

This is the source code for [this post](http://inaka.net/blog/2013/03/05/ETS-simple-cache/)

For the original version, see the [0.1 tag](https://github.com/marcelog/simple_cache/tree/0.1)

The updated code now has a separate process to handle the expirations, which is
a regular gen_server. To start it, just do:

    simple_cache_expirer:start_link()

