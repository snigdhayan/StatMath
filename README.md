# TopologicalDataAnalysis

In the directory `RTDA` I have included a down-to-earth explanation and hands-on verification of the Central Limit Theorem in `CentralLimitTheorem.ipynb`. The Stability Theorem in `Persistent Homology` roughly states that "close" point cloud data have "close" `Persistence Diagrams`. Therefore, I computed the `Wasserstein Distance` between persistence diagrams of samples drawn from the same dataset. It uses the packages

1. `TDA` - https://cran.r-project.org/web/packages/TDA/
2. `TDAstats` - https://cran.r-project.org/web/packages/TDAstats/

In the directory `PythonTDA` I have used the package `Giotto-tda` - https://github.com/giotto-ai/giotto-tda to perform breast cancer analysis. More precisely, I did the following:

1. Computed `VietorisRipsPersistence` based on the features of the breast cancer dataset.
2. Used `PersistenceEntropy` to encode the features in an abstract form.
3. Used the encoded features to train a `random forest` model to predict breast cancer (achieved accuracy above 95%)

I also used `Kepler Mapper` (consult https://kepler-mapper.scikit-tda.org/en/latest/) to visualize the breast cancer dataset in `KMapperDataVisualization.ipynb`.

.... WORK IN PROGRESS