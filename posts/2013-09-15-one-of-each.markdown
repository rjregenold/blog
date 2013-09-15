---
title: One of Each
---

My friend, [Lance](http://lanceh.github.io/), proposed an interesting problem
which ended up having a fairly elegant solution, so I figured I\'d blog it.

### The Problem

Given an array of arrays, create every possible combination using a single
element from each array. For example, given `[[1,2],[3,4]]` produce the
solution `[[1,3],[1,4],[2,3],[2,4]]`.

<!--more-->

### My Solution

We were implementing it in Ruby and here is my solution.

``` {.ruby .numberLines}
def one_of_each(*as, &blk)
  def run(as, acc, &blk)
    head, *tail = *as
    if tail.empty?
      head.map do |x| 
        val = acc + [x]
        if blk
          blk.call(val)
        else
          val
        end
      end
    else
      head.map { |x| run(tail, acc + [x], &blk) }.flatten(1)
    end
  end
 
  run(as.size == 1 ? as.flatten(1) : as, [], &blk)
end
```

### Usage

The usage is straightforward. It accepts either a list of array arguments or a 
single argument that is an array of arrays along with an optional block. If you
don\'t provide a block, it will return an array of arrays containing all possible
combinations. If you give it a block, that block will be called with a single 
array representing a single combination.

Here\'s an IRB session showing sample usage:

``` {.ruby}
>> one_of_each([1,2,3],[4,5,6],[7,8,9])
=> [[1, 4, 7], [1, 4, 8], [1, 4, 9], [1, 5, 7], [1, 5, 8], [1, 5, 9], [1, 6, 7], 
[1, 6, 8], [1, 6, 9], [2, 4, 7], [2, 4, 8], [2, 4, 9], [2, 5, 7], [2, 5, 8], 
[2, 5, 9], [2, 6, 7], [2, 6, 8], [2, 6, 9], [3, 4, 7], [3, 4, 8], [3, 4, 9], 
[3, 5, 7], [3, 5, 8], [3, 5, 9], [3, 6, 7], [3, 6, 8], [3, 6, 9]]

>> one_of_each([1,2],[3])
=> [[1, 3], [2, 3]]

>> one_of_each([1,2,3],[4,5],[6,7,8,9],[10])
=> [[1, 4, 6, 10], [1, 4, 7, 10], [1, 4, 8, 10], [1, 4, 9, 10], [1, 5, 6, 10], 
[1, 5, 7, 10], [1, 5, 8, 10], [1, 5, 9, 10], [2, 4, 6, 10], [2, 4, 7, 10], 
[2, 4, 8, 10], [2, 4, 9, 10], [2, 5, 6, 10], [2, 5, 7, 10], [2, 5, 8, 10], 
[2, 5, 9, 10], [3, 4, 6, 10], [3, 4, 7, 10], [3, 4, 8, 10], [3, 4, 9, 10], 
[3, 5, 6, 10], [3, 5, 7, 10], [3, 5, 8, 10], [3, 5, 9, 10]]

>> def plus_one(xs)
>>   xs.map { |x| x + 1 }
>>   end
=> nil
>> one_of_each([1,2],[3,4]) { |xs| plus_one(xs) }
=> [[2, 4], [2, 5], [3, 4], [3, 5]]

>> xs = [[1,2],[3,4]]
=> [[1, 2], [3, 4]]
>> one_of_each(xs)
=> [[1, 3], [1, 4], [2, 3], [2, 4]]
```

### But Wait, There\'s More!

I implemented the problem in Haskell for good measure. It\'s not as efficient
as it could be given that it appends to the accumulator which is O(n) for List,
but it works nonetheless.

``` {.haskell .numberLines}
oneOfEach :: [[a]] -> [[a]]
oneOfEach = oneOfEach' []
  where
    append xs = (xs++) . (:[])
    oneOfEach' acc (xs : []) = map (append acc) xs
    oneOfEach' acc (xs : ys) = concat $ map (flip oneOfEach' ys . (append acc)) xs
    oneOfEach' acc []        = []
```
