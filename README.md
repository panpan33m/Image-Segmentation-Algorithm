# Image-Segmentation-Algorithm


To segment an image is to partition a digital image into multiple sets of pixels.
If we segment an image into disjoint, connected sets of pixels, then color each set the same color, we see a posterization effect.

The codes use Boruvka's MST algorithm to segment images in this way:
(a) construct a grid graph, where vertices are pixels and edges connect adjacent pixels,
(b) weight each edge with the difference in color of its endpoints (similar colors result in low weight while different colors result in high weight),
(c) compute the minimum spanning tree of the graph while allowing certain “high-weight” edges to
be deleted (more on this soon) such that the graph becomes disconnected.
