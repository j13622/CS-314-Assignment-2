# CS-314-Assignment-2

For this assignment I wrote 10 functions in OCaml, that accomplished the following:

# cond_dup 
a recursive function that takes in a list and a predicate and duplicates all elements which satisfy the condition expressed in the predicate.

# n_times 
a recursive function such that n_times (f,n,v) applies f to v n times. If n<=0 return v.

# zipwith
a recursive function such that zipwith f l1 l2 generates a list whose ith element is obtained by applying f to the ith element of l1 and the ith element of l2 . If the lists have different lengths, the extra elements in the longer list should be ignored.

# buckets 
a function that partitions a list into equivalence classes. That is, buckets equiv lst should return a list of lists where each sublist in the result contains equivalent elements, where two elements are considered equivalent if equiv returns true. For example: buckets (=) [1;2;3;4] = [[1];[2];[3];[4]], buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]

# fib_tailrec 
a tail recursive function that computes in linear time by remembering just the current cur and the previous prev fibonacci number

# Map and Fold: The following functions use only map and fold_left or fold_right, no rec keyword in function definition.

# sum_rows 
a function that takes a list of int lists (call an internal list a "row") and returns a one-dimensional list of ints, each int equal to the sum of the corresponding row in the input.

# ap
a function s.t. ap fs args applies each function in fs to each argument in args in order. For example, ap [(fun x -> x^"?"); (fun x -> x^"!")] ["foo";"bar"] = ["foo?";"bar?";"foo!";"bar!"] where ^ is an OCaml operator for string concatenation.

# prefixes
a function s.t. prefixes l returns a list of all non-empty prefixes of an input list l, ordered from shortest to longest. There are no non-empty prefixes of an empty list.

# powerset
a function s.t. powerset l returns the powerset of the set of values in an input list l (the power set of a Set A is defined as the set of all subsets of the Set A including the Set itself). The order in the returned nested list (and each list element within) does not matter.

# assoc_list
a function that takes a list as input and returns a list of pairs where the first value of each pair is an element of the input list and the second integer of the pair is the number of occurrences of that element in the input list. This associative list should not contain duplicates. The order in the returned list does not matter.
