# HackSeq 2016 - Project 7

**VASCO: Visualization App for Single Cell explOration**

_Jean-Christophe Berube, Ogan Mancarci, Erin Marshall, Edward Mason, Celia Siu, Ben Weisburd, Shing Hei Zhan, Grace X.Y. Zheng_

## Scientific Abstract

Characterizing the transcriptome of individual cells is fundamental to understanding complex biological systems such as the nervous system, the immune system and cancer. Single cell RNA sequencing (scRNA-seq) technologies can profile transcriptomes of single cells. Recent development in scRNA-seq technologies has enabled profiling of thousands of cells at a time. However, existing analysis tools are designed for datasets with much lower cell numbers, and for people with a programming background, limiting their adoption. There is a strong need for an intuitive user interface to facilitate real-time data visualization and exploration by scientists to accelerate the discovery cycle of scRNA-seq analysis.

We have developed a visualization application, Visualization Application for Single Cell Overview (VASCO), that can be used for scRNA-seq data from thousands of cells in real time. It takes the gene-cell matrix and the cell clustering result as input. Users can visualize the cells using t-distributed stochastic neighbour embedding (t-SNE) plots. They can explore the expression pattern of specific genes. In addition, they can investigate the identity of cell clusters by examining genes that are specific to a cluster.

The application uses the plotly tool with Shiny to enable interaction over a web browser (currently tested on Firefox and Chrome). The source code is available on https://github.com/hackseq/2016_project_7. All plots can be saved and tables exported. We have tested single cell datasets consisting of thousands of cells, but the application can be extended to support the interactive analysis of tens of thousands of cells.

## Layman Abstract

Cell is a fundamental unit of biology. Understanding of complex biological systems requires knowing their individual cell types. Single cell RNA sequencing (scRNA-seq) technologies allow studies of single cell transcriptomes. However, existing scRNA-seq analysis tools are designed for datasets with low cell numbers, and for people with a programming background. There is a strong need for an intuitive user interface to facilitate real-time data visualization and exploration by scientists to accelerate the discovery cycle of scRNA-seq analysis.

We have developed a visualization application, Visualization Application for Single Cell Overview (VASCO), that can be used for scRNA-seq data from thousands of cells in real time. It takes the gene-cell information and the cell clustering analysis as input. Users can visualize the similarity among cells. They can explore the pattern of specific gene markers. In addition, they can investigate the identity of cell clusters by examining markers that are specific to a cluster.

The application is supported over a web browser, which currently support Firebox and Chrome. The source code is open on https://github.com/hackseq/2016_project_7. All plots can be saved and tables exported. We have tested single cell datasets consisting of thousands of cells, but the application can be extended to support the interactive analysis of tens of thousands of cells.
