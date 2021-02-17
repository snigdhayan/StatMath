# TopologicalDataAnalysis

The Stability Theorem ensures that "close" point cloud data have "close" persistence diagrams. In the directory `RTDA` I computed the wasserstein distance between persistence diagrams of samples drawn from the same dataset. It uses the packages

1. `TDA` - https://cran.r-project.org/web/packages/TDA/
2. `TDAstats` - https://cran.r-project.org/web/packages/TDAstats/

I have included a down-to-earth explanation and hands-on verification of the Central Limit Theorem in `CentralLimitTheorem.ipynb`.

In the directory `PythonTDA` I have used the package `Giotto-tda` - https://github.com/giotto-ai/giotto-tda to perform breast cancer analysis. More precisely, I did the following:

1. Computed `VietorisRipsPersistence` based on the features of the breast cancer dataset.
2. Used `PersistenceEntropy` to encode the features in an abstract form.
3. Used the encoded features to train a `random forest` model to predict breast cancer (achieved accuracy above 95%)

.... WORK IN PROGRESS