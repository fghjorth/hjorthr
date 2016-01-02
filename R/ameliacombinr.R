#' Combine multiple imputations for a list of regression models
#'
#' This function lets you input a list of regression models and retrieve coefficients and standard errors based on a combination of imputed data sets.
#' @param modellist A list of regression models, entered as formulas
#' @param aout The output from the amelia() function
#' @param subsets A list of numeric or logical vectors, one for each model, for running the model on a subset of the data
#' @keywords amelia imputation regression
#' @examples TK!
#' ameliacombinr()

ameliacombinr<-function(modellist,aout,subsets=as.list(rep(NA,length(modellist)))){
  imputed<-list(coefs=as.list(rep(NA,length(modellist))),ses=as.list(rep(NA,length(modellist))))
  for (j in 1:length(modellist)){

    #get names of coefs in model
    modelcoefs<-tidy(lm(modellist[[j]],data=aout$imputations[[1]]))[,1]

    #matrices for storing results for each imputation
    imp_coefs<-matrix(NA,nrow=impsm,ncol=length(modelcoefs))
    imp_ses<-matrix(NA,nrow=impsm,ncol=length(modelcoefs))

    for (i in 1:aout$m){
      if (is.na(subsets[[j]])){
        imp_model<-lm(modellist[[j]],data=el.am$imputations[[i]])
      } else imp_model<-lm(modellist[[j]],data=el.am$imputations[[i]][subsets[[j]],])
      imp_coefs[i,]<-tidy(imp_model)[,2]
      imp_ses[i,]<-tidy(imp_model)[,3]
    }

    imp_combined<-mi.meld(q=imp_coefs,se=imp_ses)

    #assign names to imputed outputs so stargazer can process it
    imp_coefs<-as.numeric(imp_combined$q.mi)
    names(imp_coefs)<-modelcoefs
    imp_ses<-as.numeric(imp_combined$se.mi)
    names(imp_ses)<-modelcoefs

    #store output in a list of named vectors
    imputed$coefs[[j]]<-imp_coefs
    imputed$ses[[j]]<-imp_ses
  }
  return(imputed)
}
