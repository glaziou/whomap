\name{gworld}
\alias{gworld}
\docType{data}
\title{
    Main data
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(gworld)}
\format{
  A data frame with 15243 observations on the following 7 variables.
  \describe{
    \item{\code{long}}{a numeric vector}
    \item{\code{lat}}{a numeric vector}
    \item{\code{order}}{a numeric vector}
    \item{\code{hole}}{a logical vector}
    \item{\code{piece}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{10} \code{11} \code{12} \code{13} \code{14} \code{15} \code{16} \code{17} \code{18} \code{19} \code{20}}
    \item{\code{group}}{a factor with levels \code{AFG.1} \code{AGO.1} \code{AGO.2} \code{AIA.1} \code{ALB.1} \code{AND.1} \code{AND.2} \code{ARE.1} \code{ARG.1} \code{ARG.2} \code{ARM.1} \code{ATG.1} \code{ATG.2} \code{AUS.1} \code{AUS.2} \code{AUT.1} \code{AZE.1} \code{AZE.2} \code{BDI.1} \code{BEL.1} \code{BEN.1} \code{BFA.1} \code{BGD.1} \code{BGR.1} \code{BHR.1} \code{BHS.1} \code{BIH.1} \code{BLR.1} \code{BLZ.1} \code{BOL.1} \code{BRA.1} \code{BRA.2} \code{BRB.1} \code{BRB.2} \code{BRN.1} \code{BTN.1} \code{BWA.1} \code{CAF.1} \code{CAN.1} \code{CAN.2} \code{CAN.3} \code{CAN.4} \code{CAN.5} \code{CAN.6} \code{CAN.7} \code{CAN.8} \code{CAN.9} \code{CAN.10} \code{CAN.11} \code{CAN.12} \code{CAN.13} \code{CAN.14} \code{CAN.15} \code{CAN.16} \code{CAN.17} \code{CAN.18} \code{CAN.19} \code{CAN.20} \code{CHE.1} \code{CHL.1} \code{CHL.2} \code{CHL.3} \code{CHL.4} \code{CHL.5} \code{CHL.6} \code{CHL.7} \code{CHL.8} \code{CHL.9} \code{CHL.10} \code{CHN.1} \code{CHN.2} \code{CHN.3} \code{CIV.1} \code{CMR.1} \code{COD.1} \code{COG.1} \code{COK.1} \code{COL.1} \code{COM.1} \code{CPV.1} \code{CRI.1} \code{CUB.1} \code{CUB.2} \code{CYP.1} \code{CZE.1} \code{DEU.1} \code{DJI.1} \code{DMA.1} \code{DMA.2} \code{DNK.1} \code{DNK.2} \code{DOM.1} \code{DZA.1} \code{ECU.1} \code{EGY.1} \code{ERI.1} \code{ESH.1} \code{ESP.1} \code{EST.1} \code{ETH.1} \code{FIN.1} \code{FJI.1} \code{FJI.2} \code{FLK.1} \code{FLK.2} \code{FRA.1} \code{FRA.2} \code{FSM.1} \code{GAB.1} \code{GBR.1} \code{GBR.2} \code{GEO.1} \code{GHA.1} \code{GIN.1} \code{GLP.1} \code{GMB.1} \code{GNB.1} \code{GNQ.1} \code{GRC.1} \code{GRC.2} \code{GRD.1} \code{GRD.2} \code{GRL.1} \code{GTM.1} \code{GUF.1} \code{GUM.1} \code{GUY.1} \code{HND.1} \code{HRV.1} \code{HTI.1} \code{HUN.1} \code{IDN.1} \code{IDN.2} \code{IDN.3} \code{IDN.4} \code{IDN.5} \code{IDN.6} \code{IDN.7} \code{IDN.8} \code{IDN.9} \code{IDN.10} \code{IDN.11} \code{IDN.12} \code{IDN.13} \code{IDN.14} \code{IND.1} \code{IRL.1} \code{IRN.1} \code{IRQ.1} \code{ISL.1} \code{ISR.1} \code{ITA.1} \code{ITA.2} \code{ITA.3} \code{JAM.1} \code{JOR.1} \code{JPN.1} \code{JPN.2} \code{JPN.3} \code{JPN.4} \code{KAZ.1} \code{KEN.1} \code{KGZ.1} \code{KHM.1} \code{KIR.1} \code{KNA.1} \code{KNA.2} \code{KOR.1} \code{KWT.1} \code{LAO.1} \code{LBN.1} \code{LBR.1} \code{LBY.1} \code{LCA.1} \code{LCA.2} \code{LIE.1} \code{LKA.1} \code{LSO.1} \code{LTU.1} \code{LUX.1} \code{LVA.1} \code{MAR.1} \code{MCO.1} \code{MDA.1} \code{MDG.1} \code{MDV.1} \code{MEX.1} \code{MHL.1} \code{MKD.1} \code{MLI.1} \code{MLT.1} \code{MMR.1} \code{MNE.1} \code{MNG.1} \code{MOZ.1} \code{MRT.1} \code{MSR.1} \code{MTQ.1} \code{MUS.1} \code{MWI.1} \code{MYS.1} \code{MYS.2} \code{NAM.1} \code{NCL.1} \code{NER.1} \code{NGA.1} \code{NIC.1} \code{NIU.1} \code{NLD.1} \code{NOR.1} \code{NPL.1} \code{NRU.1} \code{NZL.1} \code{NZL.2} \code{OMN.1} \code{OMN.2} \code{PAK.1} \code{PAN.1} \code{PER.1} \code{PHL.1} \code{PHL.2} \code{PHL.3} \code{PHL.4} \code{PHL.5} \code{PHL.6} \code{PHL.7} \code{PHL.8} \code{PLW.1} \code{PNG.1} \code{PNG.2} \code{PNG.3} \code{POL.1} \code{PRI.1} \code{PRI.2} \code{PRK.1} \code{PRT.1} \code{PRY.1} \code{PSE.1} \code{PSE.2} \code{QAT.1} \code{REU.1} \code{ROU.1} \code{RUS.1} \code{RUS.2} \code{RUS.3} \code{RUS.4} \code{RUS.5} \code{RUS.6} \code{RUS.7} \code{RUS.8} \code{RUS.9} \code{RUS.10} \code{RUS.11} \code{RUS.12} \code{RWA.1} \code{SAU.1} \code{SDN.1} \code{SEN.1} \code{SGP.1} \code{SJM.1} \code{SJM.2} \code{SJM.3} \code{SLB.1} \code{SLE.1} \code{SLV.1} \code{SMR.1} \code{SMR.2} \code{SOM.1} \code{SRB.1} \code{SSD.1} \code{STP.1} \code{SUR.1} \code{SVK.1} \code{SVN.1} \code{SWE.1} \code{SWZ.1} \code{SYC.1} \code{SYR.1} \code{TCA.1} \code{TCD.1} \code{TGO.1} \code{THA.1} \code{TJK.1} \code{TKL.1} \code{TKM.1} \code{TLS.1} \code{TLS.2} \code{TON.1} \code{TTO.1} \code{TUN.1} \code{TUR.1} \code{TUR.2} \code{TUV.1} \code{TZA.1} \code{UGA.1} \code{UKR.1} \code{URY.1} \code{USA.1} \code{USA.2} \code{USA.3} \code{USA.4} \code{USA.5} \code{UZB.1} \code{VCT.1} \code{VCT.2} \code{VEN.1} \code{VGB.1} \code{VIR.1} \code{VNM.1} \code{VUT.1} \code{VUT.2} \code{WSM.1} \code{YEM.1} \code{ZAF.1} \code{ZAF.2} \code{ZAF.3} \code{ZMB.1} \code{ZWE.1}}
    \item{\code{id}}{a character vector}
  }
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
data(gworld)
## maybe str(gworld) ; plot(gworld) ...
}
\keyword{datasets}
