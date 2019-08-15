
library(quantmod)


# RICs
#list.atx <- c(
#  'ANDR.VI','ATSV.VI','BAWG.VI','CAIV.VI','DOCO.VI','ERST.PR','ERST.VI','FACC.VI','IMFI.VI','LENV.VI','OMVV.VI','POST.VI','RBIV.VI','SBOE.VI','SIAG.VI','TELA.VI','UNIQ.VI','VERB.VI','VOES.VI','WBSV.VI','VIGR.VI'
#)
#list.dax <- c(
#  'ADSGn.DE','ALVG.DE','VNAn.DE','BASFn.DE','BAYGn.DE','BEIG.DE','BMWG.DE','CONG.DE','DB1Gn.DE','DBKGn.DE','DAIGn.DE','DPWGn.DE','DTEGn.DE','EONGn.DE','FMEG.DE','FREG.DE','HEIG.DE','HNKG_p.DE','IFXGn.DE','LHAG.DE','LINI.DE','MRCG.DE','MUVGn.DE','RWEG.DE','SAPG.DE','SIEGn.DE','TKAG.DE','VOWG_p.DE','WDIG.DE'
#)

# BBG + VI/DE
list.atx <- c(
  'ANDR.VI','ATS.VI','BG.VI','CAI.VI','DOC.VI','EBS.VI','FACC.VI','IIA.VI','LNZ.VI','OMV.VI','POST.VI','RBI.VI','SBO.VI','SPI.VI','TKA.VI','UQA.VI','VER.VI','VOE.VI','WIE.VI','VIG.VI'
)
list.dax <- c(
  'ADS.DE','ALV.DE','VNA.DE','BAS.DE','BAYN.DE','BEI.DE','BMW.DE','CON.DE','DB1.DE','DBK.DE','DAI.DE','DPW.DE','DTE.DE','EOAN.DE','FME.DE','FRE.DE','HEI.DE','HEN3.DE','IFX.DE','LHA.DE','LIN.DE','MRK.DE','MUV2.DE','RWE.DE','SAP.DE','SIE.DE','TKA.DE','VOW3.DE','WDI.DE'
)


getSymbols('EBS.VI',src='yahoo')
plot(OMV.VI$OMV.VI.Close)
candleChart(OMV.VI,multi.col=TRUE,theme="white")


loadyahoo <- function(sindex){
  for (i in 1:length(sindex)){
    getSymbols(sindex[i],src='yahoo')
    Sys.sleep(1.1)
  }
}
loadyahoo(list.atx)
loadyahoo(list.dax)

# load ATX
getSymbols(list.atx[1:5],src='yahoo')
getSymbols(list.atx[6:10],src='yahoo')
getSymbols(list.atx[11:15],src='yahoo')
getSymbols(list.atx[16:20],src='yahoo')

# load DAX
getSymbols(list.dax[1:5],src='yahoo')
getSymbols(list.dax[6:10],src='yahoo')
getSymbols(list.dax[11:15],src='yahoo')
getSymbols(list.dax[16:20],src='yahoo')
getSymbols(list.dax[21:25],src='yahoo')
getSymbols(list.dax[26:29],src='yahoo')


candleChart(EBS.VI,multi.col=TRUE,theme="white")
