#'
#'  Function for creating data for marginsplot
#'
#'  This function lets you easily create a plot of marginal effects from a regression interaction model
#'
#'  @param model A regression model object, e.g. model1
#'  @param xterm The term for which to plot effects on y, as character
#'  @param zterm The term along which to plot effects of x on y
#'  @param zseq Sequence of values of z for which to plot the marginal effect of x
#'  @keywords margins
#'  @export
#'  @examples
#'  m1<-lm(mpg~cyl*wt,data=mtcars)
#'  require(hjorthr)
#'  mdf<-marginsplotdf(m1,"wt","cyl",seq(from=min(mtcars$cyl),to=max(mtcars$cyl),by=.5))
#'
#'  require(ggplot2)
#'  ggplot(mdf,aes(x=z,y=dydx,ymin=dydx-1.96*se,ymax=dydx+1.96*se)) +
#'    geom_line() +
#'    geom_ribbon(alpha=.2)


marginsplotdf<-function(model, xterm, zterm, zseq){
  coefs<-coef(model)
  cov<-vcov(model)
  intterm<-ifelse(is.na(coefs[paste(xterm,zterm,sep=":")]),paste(zterm,xterm,sep=":"),paste(xterm,zterm,sep=":"))
  dy.dx<-coefs[xterm]+coefs[intterm]*zseq
  se.dy.dx<-sqrt(cov[xterm,xterm]+zseq^2*cov[intterm,intterm]+zseq*2*cov[xterm,intterm])
  margins<-data.frame(z=zseq,dydx=dy.dx,se=se.dy.dx)
  return(margins)
}

# # Code example for creating marginsplot:
#
# marginsdata<-margins.twoway(model=model1,xterm="education",zterm="income",zseq=seq(from=0,to=100,by=1))
#
# t90<-1.65
# t95<-1.96
#
# ggplot(marginsdata,aes(x=z,y=dydx)) +
#   geom_hline(yintercept=0,linetype=2,color="dark gray") +
#   geom_smooth(aes(ymin=dydx-t90*se,ymax=dydx+t90*se),stat="identity",alpha=.3,color="black") +
#   geom_smooth(aes(ymin=dydx-t95*se,ymax=dydx+t95*se),stat="identity",alpha=.2,linetype=0) +
#   theme_bw()

