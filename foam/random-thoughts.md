# Random Thoughts

## World Size

The world has a maximum size of `Extent::MAX`, *but* since resizing a `HexDiv` is basically free, it is stored in a way that it resizes similar to a `Vec`. This means, as long as the world is still relatively small, it won't have to do the full 16 splits of traversal for each single block access.

This also makes custom world size limits, for when players want to limit their world, very straight-forward even for non power-of-two sizes.
