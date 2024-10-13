source("R/setup.R")
source("R/utils.R")
# Get Spectral data
SML <- fread("data/SML.csv")
SourceID_Target <- c("dsp","fos") # We cannot MIX asd/sco together with dsp/fos.
ProjectID_Target <- "mrd"
WLmin <- 0# 750
WLmax <- Inf# 2350
Nema <- 10

# issues with:
# SID <- "LKS086" 
# SID <- "372652" 
# SID <- "83J0418"
# SID <-  "149C097A"
# SID <- "398148"
# SID <- "350673"
# SID <- "GOL077"

IDX <- SML[SourceID %in% SourceID_Target ]$SampleID |> unique()

# IDX <- c("76J0025","93D0188","92J0783")


SID <- sample(IDX,1)
DT0 <- SML[SampleID  %in%  SID & SourceID %in% SourceID_Target ][WL<=WLmax & WL>=WLmin][,.(
  WL,R,Rn=get_convex_envelope(x=WL,y=R,type="upper")-R)]

# PlotA
IMF <- gmsp::get_imf(s=DT0$R,ts=DT0$WL,method="emd",noise.amp = .5e-7)
# smooted spectra
DT1 <- IMF[!(IMF %in% c("signal","IMF1")),.(R=sum(s)),by=.(WL=t)]

# smoothed spectra with residue removed
DT2 <- IMF[!(IMF %in% c("signal","IMF1","residue")),.(Rn=-sum(s)),by=.(WL=t)]

DT3 <- IMF[IMF=="residue",.(Rn=s,WL=t)]
DT.PEAKS <- getPeaks(x=DT0,nema=10,ndiff=20,npeaks=10)

# ------------------------------------------------
SPECTRA <- list(
  # DT0[,.(ID="envelope",X=WL, Y=Rue)],
  DT0[,.(ID="raw",X=WL, Y=R)],
  DT0[,.(ID="ema",X=WL, Y=(TTR::EMA(R, n=Nema)+rev(TTR::EMA(rev(R), n=Nema)))/2)],
  DT1[,.(ID="emd",X=WL, Y=R)],
  DT3[,.(ID="res",X=WL, Y=Rn)]
  
) |> rbindlist(use.names = TRUE)
PEAKS <- DT.PEAKS[,.(ID="peaks",X=WL, Y=R)]

buildPlot.spectral(
  data.spectra=SPECTRA, # full spectral dataset
  data.points=PEAKS,  # subset of points to highlight
  plot.subtitle = "",
  plot.title = "",
  yAxis.label = "Intensity",  # Adjust label as per spectral data
  xAxis.label = "Wavelength", # Adjust label as per spectral data
  group.label = "ID",
  plot.palette = hcl.pals()[4],#[80],
  legend.layout = "vertical",
  legend.align = "right", 
  legend.valign = "top",
  legend.show = TRUE,
  plot.save = TRUE,
  group.legend.fontsize = "12px"
)

# ------------------------------------------------
SPECTRA <- list(
  # DT0[,.(ID="envelope",X=WL, Y=Rue)],
  DT0[,.(ID="raw",X=WL, Y=Rn)],
  DT0[,.(ID="ema",X=WL, Y=(TTR::EMA(Rn, n=Nema)+rev(TTR::EMA(rev(Rn), n=Nema)))/2)],
  DT2[,.(ID="res-imf1",X=WL, Y=Rn)],
  DT3[,.(ID="res",X=WL, Y=Rn)]
) |> rbindlist(use.names = TRUE)
# PEAKS <- DT0[,.(ID="peaks",X=WL[I], Y=Rn[I])]

PEAKS <- DT.PEAKS[,.(ID="peaks",X=WL, Y=Rn)]

buildPlot.spectral(
  data.spectra=SPECTRA, # full spectral dataset
  data.points=PEAKS,  # subset of points to highlight
  data.columns=PEAKS,  # subset of points to highlight
  
  plot.subtitle = "",
  plot.title = "",
  yAxis.label = "Absortion",  # Adjust label as per spectral data
  xAxis.label = "Wavelength", # Adjust label as per spectral data
  group.label = "ID",
  plot.palette = hcl.pals()[4],#[80],
  legend.layout = "vertical",
  legend.align = "right", 
  legend.valign = "top",
  legend.show = TRUE,
  plot.save = TRUE,
  group.legend.fontsize = "12px"
)

# revisar algoritmo de extraccion de features desde EMD
# comparar envolvente con residuo

