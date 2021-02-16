# TopologicalDataAnalysis

WORK IN PROGRESS

The Stability Theorem ensures that "close" point cloud data have "close" persistence diagrams. In the directory `RTDA` I computed the wasserstein distance between persistence diagrams of samples drawn from the same dataset. It uses the packages

1. `TDA` - https://cran.r-project.org/web/packages/TDA/
2. `TDAstats` - https://cran.r-project.org/web/packages/TDAstats/

I have included a down-to-earth explanation and hands-on verification of the Central Limit Theorem in `CentralLimitTheorem.ipynb`.

In the directory `PythonTDA` there is a sample computation of VietorisRipsPersistence using `Giotto-tda` - https://github.com/giotto-ai/giotto-tda.