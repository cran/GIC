\name{iteration}
\alias{iteration}
\title{A General Iterative Clustering Algorithm}
\description{
An algorithm that improves the proximity matrix (PM) from a random forest (RF) and the resulting clusters from an arbitrary cluster algorithm as measured by the silhouette score. The initial PM, that uses unlabeled data, is produced by one of many ways to provide psuedo labels for a RF. After running a cluster program on the resulting initial PM, cluster labels are obtained. These are used as labels with the same feature data to grow a new RF yielding an updated proximity matrix. This is entered into the clustering program and the process is repeated until convergence.

}
\usage{
iteration(data,initiallabel,ntree=500)
}
\arguments{
  \item{data}{ an input dataframe without label}
  \item{initiallabel}{ a vector of label to begin with}
  \item{ntree}{ the number of trees (default 500).}

}
\value{
  An object of class \code{iteration}, which is a list with the following components:
  \item{PAM}{output final PAM information}
  \item{randomforest}{output final randomforest information}
  \item{clustering}{A vector of integers indicating the cluster to which each point is allocated.}
  \item{silhouette_score}{A value of mean silhouette score for clusters}
  \item{plot}{A scatter plot which X-axis, y-axis, and color are first important feature, second important feature, and final clusters, respectively. }
}
\details{
  This code requires initial labels as input, which can be obtained by any method of the users choice.
  As an alternative, Breimans' unsupervised method or Siegel and her colleagues' purposeful clustering method to obtain initial labels, use the function \code{GIC}


}
\references{
  Breiman, L. (2001), Random Forests, \emph{Machine Learning} 45(1),
  5-32.

  Siegel, C.E., Laska, E.M., Lin, Z., Xu, M., Abu-Amara, D., Jeffers, M.K., Qian, M., Milton, N., Flory, J.D., Hammamieh, R. and Daigle, B.J., (2021). Utilization of machine learning for identifying symptom severity military-related PTSD subtypes and their biological correlates. \emph{Translational psychiatry}, 11(1), pp.1-12.
}
\examples{

data(iris)
##Using KMEANS to find inital label
cl=kmeans(iris[,1:4],3)
###Doing GIC to find final clustering
rs=iteration(iris[,1:4],cl$cluster,ntree=100)
print(rs$clustering)

}


