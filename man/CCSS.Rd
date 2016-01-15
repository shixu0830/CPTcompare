\name{CCSS}
\alias{CCSS}
\docType{data}
\title{
Clinical Classifications Software for Services and Procedures (CCS)
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(CCSS)}
\format{
  The format is:
List of 2
 $ :'data.frame':	46615 obs. of  3 variables:
  ..$ code         : int [1:46615] 49080 49081 49082 49083 49084 59840 59841 59842 59843 59844 ...
  ..$ codegrp      : chr [1:46615] "Abdominal paracentesis" "Abdominal paracentesis" "Abdominal paracentesis" "Abdominal paracentesis" ...
  ..$ codegrp.order: int [1:46615] 88 88 88 88 88 126 126 126 126 126 ...
 $ :'data.frame':	7073 obs. of  3 variables:
  ..$ code         : chr [1:7073] "S0199" "S2260" "S2262" "S2265" ...
  ..$ codegrp      : chr [1:7073] "Abortion (termination of pregnancy)" "Abortion (termination of pregnancy)" "Abortion (termination of pregnancy)" "Abortion (termination of pregnancy)" ...
  ..$ codegrp.order: num [1:7073] 126 126 126 126 126 126 126 219 219 219 ...
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(CCSS)
## maybe str(CCSS) ; plot(CCSS) ...
}
\keyword{datasets}
