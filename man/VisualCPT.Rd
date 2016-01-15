\name{VisualCPT}
\alias{VisualCPT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualization of Difference in Current Procedural Terminology Code Assignments
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Determine existance and estimate difference in endorsement of CPT codes between two health systems. Use Clickme package to create an interactive visualization in the browser that desplay the estimated rate ratios, significance level, and code informations. Use the Clinical Classifications Software for Services and Procedures (CCS-S&P) to group CPT codes describing similar medical procedures.
}
\usage{
VisualCPT(id, code, site, method = "Elasticnet", alpha = 0.5, testmethod="NBExact", output_path = getwd(), template_path = getwd(), opacity = 0.6, radius = 4.5, height = 450, width = 890)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
Patient ID. A vector of integers.
}
  \item{code}{
CPT code. A vector of the same length as id. This records CPT code assigned to a patient at some visit. One CPT code can be assigned to a patient at multiple times, recorded repeatedly here.
}
  \item{site}{
The healte care center each code was assigned at.
}
  \item{method}{
Method for estimating the rate ratio, method = "GLMM", "Lasso", "Ridge", or "Elasticnet". 
}
  \item{alpha}{
For "Elasticnet" method, an alpha value need to be specified denoting weight of lasso penalty. For more details please see the documentation of the glmnet package.
}
  \item{testmethod}{
Method for hypothesis testing, choices include "NBLRT" (likelihood ratio test assuming Negative Binomial model), "NBExact" (exact test assuming Negative Binomial model), "PoisLRT" (likelihood ratio test assuming Poisson model), "PoisExact" (exact test assuming Poisson model), "BinLRT" (likelihood ratio test after dichotomizing count of code assignments, assuming Binomial model), "FishersExact" (Fisher's exact test for dichotomized count of code assignments), "Ttest" (two-sample T-test assuming equal variance).
}
  \item{output_path, template_path}{
The path of directory to store the .html file and clickme asset (.js files) for the interactive plot.
}
%  \item{template_path}{
%The path of directory to store the template and 
%}
  \item{opacity}{
Opacity of points in the plot.
}
  \item{radius}{
Radius of circles in the plot. Determines the size of each point.
}
  \item{height, width}{
The height and width of the plot region in inches.
}


}
\details{
Apply either Generalized Linear Mixed Model or Penalized Regression to estimate rate ratio comparing CPT code usage between two sites. Analyses take into account similarity in procedures described by certain CPT codes. P values are computed based on likelihood ratio tests.
}
\value{
This function generates an .html file and a binder named "clickme_assets" which scores .js codes. The .html file need to be stored in the same directory as the "clickme_assets".

Point at each dot in the plot gives some basic information about the code, the procedure group it blongs to, the rate ratio, the p value, the raw number of times the code was assigned at each site, as well as the number of patients in each site.

You could search for a certain code or hide certain codes of some significant levels.
Scrolling with three fingers moves the plot; scrolling with two fingers zooms in/out.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Xu Shi <xushi@uw.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--  or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (id, code, site, method = "Elasticnet", alpha = 0.5, 
    output_path = getwd(), template_path = getwd(), opacity = 0.6, 
    radius = 4.5, height = 450, width = 890) 
{
    cpt = QC(id, code, site)
    cpt = grpcode(cptnonnum, cptnum)
    cpt = renameNonnumeric(cpt)
    dat = SufficientData(cpt)
    pdata_name = PData(dat, method = "Elasticnet", alpha = 0.5)
    pdata = pdata_name[[1]]
    name = pdata_name[[2]]
    options(clickme_output_path = output_path, clickme_template_path = output_path)
    clickme(points, title = name, x = pdata$block, y = pdata$log2RR, 
        names = pdata$code, extra = list(Info = pdata$extra), 
        xlab = "block #", ylab = "log2RR", palette = c(`> 0.05` = "#E0E0E0", 
            `0.05 - 0.01` = "#be29ec", `0.01 - sig` = "#3bd6c6", 
            sig. = "red"), color_groups = pdata$level, opacity = opacity, 
        radius = radius, out_height = height, out_width = width, 
        font = "Arial", file = paste(name, ".html", sep = ""))$iframe()
    print(paste("The .html file is in ", output_path, sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
