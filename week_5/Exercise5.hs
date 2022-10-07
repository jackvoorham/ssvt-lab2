{--

We did not have further time for implementation...

Here are the directions we would go in:

For subset, we can create a function that takes a set of mutators and a set of properties and the function that we test.  
Then we can implement the function such that it generates mutations with the mutators and then proceeds to look at which properties still hold. 
We then need to find a way to keep track of each of the individual mutations and see which properties pass for which mutation. We then need to also find a way 
to compare the individual passing mutations between properties and then look at which ones have a high rate of correlation of passing; for example, 30 are passing 
on property x, 28 are passing on property y, and y’s passing mutations is a subset of x’s passing mutations, we can infer the subset cojecture. Implementation-wise we
can maybe look at a list of booleans for each property (where each index keeps track of whether the mutation passes the property) and then compare each of the lists of
booleans indices and then keep track of how many are in the same index.

For equivalence we take a similar approach as the subset above, however, we make the comparison stricter. Namelely that the list of booleans the indicies must
be equal, that is, each of the indices must be hold the same boolean value. This means, that if the property passes on mutation x (then index x in the lsit of the property is True), then index 
x of the other property must also be True. If the equality holds, we can infer the equivalence conjecture. 

-- Time spent: 30 minutes -- 

--}