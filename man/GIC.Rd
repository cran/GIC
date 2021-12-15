\name{GIC}
\alias{GIC}
\title{A General Iterative Clustering Algorithm}
\description{
An algorithm improves the proximity matrix (PM) from a random forest (RF) and the resulting clusters from an arbitrary cluster algorithm, such as PAM, as measured by the silhouette_score. The first PM that uses unlabeled data is produced by one of many ways to provide psuedo labels for a RF. After running a cluster program on the resulting initial PM, cluster labels are obtained. These are used as labels with the same feature data to grow a new RF yielding an updated proximity matrix. This is entered into the clustering program and the process is repeated until convergence.
}
\usage{
GIC(data,cluster,initial="breiman",ntree=500,
        label=sample(1:cluster,nrow(data),replace = TRUE))
}
\arguments{
  \item{data}{ an input dataframe without label}
  \item{cluster}{ The number of clusters in the solution}
  \item{initial}{ A method to calculate initial cluters to begin the iteration (default \code{breiman}). \code{breiman}: using Breimans' unsupervised method to find initial cluters, or \code{purpose}: using Siegel and her colleagues' purposeful clustering method to find initial cluters}
  \item{ntree}{ the number of trees (default 500).}
  \item{label}{ A truth set of labels, only required if \code{purpose} is used as the method to find the initial PM}

}

\value{
  An object of class \code{GIC}, which is a list with the following components:
  \item{PAM}{output final PAM information}
  \item{randomforest}{output final randomforest information}
  \item{clustering}{A vector of integers indicating the cluster to which each point is allocated.}
  \item{silhouette_score}{A value of mean silhouette score for clusters}
  \item{plot}{A scatter plot which X-axis, y-axis, and color are first important feature, second important feature, and final clusters, respectively. }

}
\details{
  This code include Breimans' unsupervised method and Siegel and her colleagues' purposeful clustering method to calculate initial labels
  To imput user specified initial labels, please use the function \code{initial}
}
\references{
  Breiman, L. (2001), Random Forests, \emph{Machine Learning} 45(1),
  5-32.

  Siegel, C.E., Laska, E.M., Lin, Z., Xu, M., Abu-Amara, D., Jeffers, M.K., Qian, M., Milton, N., Flory, J.D., Hammamieh, R. and Daigle, B.J., (2021). Utilization of machine learning for identifying symptom severity military-related PTSD subtypes and their biological correlates. \emph{Translational psychiatry}, 11(1), pp.1-12.
}
\examples{

data(iris)
##Using breiman's method
rs=GIC(iris[,1:4],3,ntree=100)
print(rs$clustering)

}
