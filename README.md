# # Introduction

A concurrent banking system with 10 customer threads (Software Transactional Memory) interacting with each other by transferring money between their accounts. The processes are also done parallely on different cores of computer (`-threaded -rtsopts -O2` flags)

# Description

A customer type is made in the following manner:

```
type Name = String
type AccNo = Int
type Balance = Int
data Customer = Customer Name AccNo Balance
type Account = TVar Customer
```

1. There are 10 customers, and for each customer 10 threads are spawned describing different types of transactions. Hence, in total 100 transfers are simulated.
2. These threads, at random intervals of time, select one of the other customers (also at random), and transfers a random amount of money (between 10 and 50) into their account.
3. Account balances of any customer should never be negative
4. `mapConcurrently` is used for parallizing the whole operation.
