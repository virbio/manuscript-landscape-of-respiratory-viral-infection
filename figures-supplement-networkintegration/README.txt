There are 3 foldes in this archive. Each folder contains details of the visualization executed on different input data: 
1. including both CRISPR and Reactome. This is the basis of Figure2 in the main text.
2. CrisprOnly: same as above but the profiles miss the Reactome embeddings.
3. ReactomeOnly: same as above, but the profiels miss the Crispr p-values. 

Each folder contains:
- a table with all genes used in the visualization and their profile: the -log10 p-values and/or the mashup based graph embeddings
- tables describing the graph visualization (xy and color coordinates, edge lists etc)
- json files describing which gene has a significant signal for which pathogen
- the list of all genes in the full reactome  network
