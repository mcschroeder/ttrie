A contention-free STM hash map for Haskell.

"Contention-free" means that the map will never cause spurious conflicts.
A transaction operating on the map will only ever have to retry if
another transaction is operating on the same key at the same time.

This is an implementation of the *transactional trie*,
which is basically a *lock-free concurrent hash trie* lifted into STM.
For a detailed discussion, including an evaluation of its performance,
see Chapter 4 of [my master's thesis](https://github.com/mcschroeder/thesis).
