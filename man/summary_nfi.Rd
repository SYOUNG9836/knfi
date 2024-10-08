% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_NFI.R
\name{summary_nfi}
\alias{summary_nfi}
\title{Calculate descriptive statistics for National Forest Inventory Data}
\usage{
summary_nfi(
  data,
  plotgrp = NULL,
  byplot = FALSE,
  clusterplot = FALSE,
  largetreearea = TRUE,
  stockedland = TRUE,
  talltree = TRUE
)
}
\arguments{
\item{data}{: A \code{list} containing 'plot' and 'tree' data frames, typically generated by \code{\link{read_nfi}}.}

\item{plotgrp}{: A character vector; specifies variables from 'plot' table to use for grouping. Use \code{c()} to combine multiple variables.}

\item{byplot}{: A logical flag (default FALSE); if TRUE, calculates statistics for each plot separately. If FALSE, calculates for the entire dataset.}

\item{clusterplot}{: A logical flag (default FALSE); if TRUE, treats each cluster plot as a single unit. If FALSE, calculates for each subplot separately.}

\item{largetreearea}{: A logical flag (default TRUE); if TRUE, includes large tree survey plots in the analysis. If FALSE, only uses standard tree plots.}

\item{stockedland}{: A logical flag (default TRUE); if TRUE, includes only stocked land. If FALSE, includes all land types.}

\item{talltree}{: A logical flag (default TRUE); if TRUE, includes only tall trees. If FALSE, includes both trees and shrubs.}
}
\value{
A \code{data.frame} that includes summary statistics.
The structure depends on the input parameters:
\itemize{
\item If \code{byplot = TRUE}, each row represents a plot.
\item If \code{byplot = FALSE}, each row represents the entire dataset or a group specified by \code{plotgrp}
}
}
\description{
summary_nfi() is a function that calculates comprehensive descriptive statistics for National Forest Inventory (NFI) data.
It can provide summaries for individual plots, the entire study area, or specific groups within the study area using parameters \code{byplot} or \code{plotgrp}.
Users have flexibility in specifying data inclusion criteria and analysis levels using parameters \code{clusterplot}, \code{largetreearea}, \code{stockedland}, and \code{talltree}.
These parameters determine whether to treat cluster plots as single plots, to include large tree survey plots, and to focus only on Stocked land and tall trees.
}
\details{
The function calculates the following statistics:
Plot-related statistics:
\itemize{
\item Number of cluster plots
\item Number of subplots
\item Number of subplots with large trees (\eqn{\geq} 30cm) observed
}

Tree-related statistics:
\itemize{
\item Number of individual trees
\item Number of large trees
\item Number of dominant trees
\item Number of tree species
}

Tree measurements and stand statistics (mean and standard deviation):
\itemize{
\item DBH (Diameter at Breast Height)
\item Tree height
\item Height of dominant trees
\item Number of trees per hectare
\item Basal area per hectare
\item Volume per hectare
}

The \code{largetreearea} parameter affects calculations differently:
\itemize{
\item For per-hectare statistics (trees per hectare, basal area per hectare, volume per hectare), setting \code{largetreearea = TRUE} includes data from large tree survey plots.
\item For all other statistics, trees from large tree survey plots are always excluded, regardless of the \code{largetreearea} setting.
}
}
\examples{

data("nfi_donghae")

# Basic usage
summary_stats <- summary_nfi(nfi_donghae)

# Summarize by the group, including all land types
grouped_stats <- summary_nfi(nfi_donghae, plotgrp = "OWN_CD", stockedland = FALSE)

# Summarize by individual plots, including both trees and shrubs 
plot_summaries <- summary_nfi(nfi_donghae, byplot = TRUE, talltree = FALSE)

}
