# haskell_learning
programming in haskell &amp;&amp; learn you a haskell code repository.

## Content

* exercises from programming in haskell


## How to work with:

```bash
# in a tmux window
vim foo.hs

# do some change and press ESC:wq


# in another tmux window
ghci
> :l foo.hs

##then we could see the change~

```

## Q&A

1. what is difference between newtype,type and data in haskell?

- A newtype guarantees that your data will have exactly the same representation at runtime, as the type that you wrap.


- While data declares a brand new data structure at runtime.

So the key point here is that the construct for the newtype is guaranteed to be erased at compile time.


2. Difference between type, newtype, data, class

[click me](./docs/differences_type_newtype_data_class.md)
