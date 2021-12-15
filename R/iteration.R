iteration<-function(data,initiallabel,ntree=500){
  ct<-NULL
  data<-cbind(data,ct=initiallabel)
  cluster<-length(unique(initiallabel))
  rf<-randomForest(factor(ct)~., data=data, ntree=ntree, proximity=TRUE,importance=TRUE)
  dis<-1-rf$proximity
  pamRF <- pam(dis, k = cluster)
  data$ct<-pamRF$clustering
  s1<-summary(silhouette(data$ct,dis))$avg.width
  sc<-100
  while(sc>0.01){
    rf1<-randomForest(factor(ct)~., data=data, ntree=ntree, proximity=TRUE,importance=TRUE)
    D1 <-1-rf1$proximity
    ##Use PAM
    pamRF1 <- pam(D1, k =cluster)
    s2<-summary(silhouette(data$ct,D1))$avg.width
    sc<-100*abs(s2-s1)/s1
    s1<-s2
    data$ct<-pamRF1$clustering

  }

  cl<-data$ct

  df<-data.frame(imp1 = rf1$importance[order(rf1$importance[,ncol(rf1$importance)],decreasing=T),])

  fig<-ggplot(data, aes_string(x=rownames(df)[1], y=rownames(df)[2],color=factor(ct))) + geom_point()+theme_bw()+
    xlab(rownames(df)[1])+ylab(rownames(df)[2])+ scale_color_discrete(name = "Group")


  result<-list("PAM"=pamRF1,"randomforest"=rf1,"clustering"=cl,"silhouette_score"=s1,"plot"=fig)
  result

}



