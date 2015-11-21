### distanceParallel, a package to calculate distance matrixes with arbitrary distance functions and parallelized with `foreach` frontend and the `doMC` backend

If you do `?dist` you'll see that there are only a few (but common) methods for calculating distances. However, if you want to compare between two samples and you need a custom distance function, well you're out of luck.

This package is designed to do just that - calculate arbitrary distances between all pairwise comparisosn of either rows or columns of a matrix. Using `foreach` and `doMC`, these calculations are parallized for faster runtimes.

### Main functions

#### `divide_cores()`

This function is used within `create_pairwise_comparisons()` to split up the total amount of calculations evenly across all cores, which reduces "overhead" time spent on spawning new processes

#### `create_pairwise_comparisons()`

This function creates all pairwise combinations with `comb`, and loops over all of these combinations and applies a custom distance function (defaults to euclidian distance). This returns a data.frame containg the two samples that were matched together and their distance.

#### `create_distance_matrix()`

Finally, this function converts the data.frame into a distance matrix using `tidyr::spread` and `as.dist()`
