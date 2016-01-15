VisualCPT <-
function(id,code,site,method="Elasticnet",alpha=0.5,testmethod="Ttest",
                     output_path = getwd(),template_path = getwd(),
                     opacity = .5,radius = 4.5,height = 450, width = 870){
  # data check
  QCrslt=QC(id,code,site)
  cpt = QCrslt$cpt; cptnonnum = QCrslt$cptnonnum; cptnum = QCrslt$cptnum
  # block classification
  rslt = grpcode(cptnonnum,cptnum)
  cpt = rslt$cpt; code.nocategory = rslt$code.nocategory; cptnonnum = rslt$cptnonnum; cptnum = rslt$cptnum
  if(length(code.nocategory)>0){print(paste0("The codes not categorized are: ", 
                                      paste(as.character(code.nocategory),collapse="; ")))}
  # rename non-numeric codes
  cpt = renameNonnumeric(cpt)
  # get sufficient stat
  dat = SufficientData(cpt)
  # construct plot data -- GLMM/PenalizedRegress
  pdata_name = PData(cpt,dat,method="Elasticnet",alpha=0.5,testmethod="Ttest")#"Ttest")
  pdata=pdata_name[[1]]; name = pdata_name[[2]]
  
  # clickme plot
  options(clickme_output_path = "/Users/xushi/",#output_path,
          clickme_template_path = "/Users/xushi/")#output_path)
  clickme(points,
            title = name,x = pdata$block, y = pdata$log2RR,
            names = pdata$code,
            extra = list("Info" = pdata$extra),
            x_title = "Block", y_title = "lob2RR",
            #xlab = "block #", ylab = "log2RR",
            palette = c("> 0.05" = "#E0E0E0", 
                        "0.05 - 0.01" = "#be29ec", 
                        "0.01 - sig." = "#3bd6c6",
                        "sig. (Bonferroni)" = "red"
            ),
            color_groups = pdata$level,
            opacity = opacity, radius = radius,
            out_height = height, 
            out_width = width,
            font = "Arial",
            padding = list(left = 70),
            file = paste(name,".html",sep=""))$iframe()
#               height = 600,
#               width = 2000,
#               relative_path = 
#                 "./temp/cache/{{domainslash_if_multitiki}}R_{{page}}")
  
  print(paste("The .html file is in ", output_path,sep=""))
}
