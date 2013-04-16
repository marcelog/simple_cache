# Introduction

This is the source code for [this post](http://inaka.net/blog/2013/03/05/ETS-simple-cache/), a
simple ETS based cache.

For the original version, see the [0.1 tag](https://github.com/marcelog/simple_cache/tree/0.1)

# Using it

The updated code now has a separate process to handle the expirations, which is
a regular gen_server. To start it, just do:

    simple_cache_expirer:start_link()

## Create a cache

    simple_cache:init(my_cache_name).

## Getting a key

The following call will lookup **my\_key** in the cache named **my\_cache\_name**, and on
a MISS will call the given **fun**, caching its result for **3600000** milliseconds.

    simple_cache:get(my_cache_name, 3600000, my_key, fun() ->
        io:format("This fun will be called on a cache miss~n"),
        timer:sleep(5000)
        this_value_will_be_cached
    end)

## Flushing the cache

    simple_cache:flush(my_cache_name).

## Flushing a key

    simple_cache:flush(my_cache_name, my_key).
