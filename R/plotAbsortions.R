source("R/setup.R")
source("R/utils.R")
# Get Spectral data
SML <- fread("data/SML.csv")
SourceID_Target <- c("dsp","fos") # We cannot MIX asd/sco together with dsp/fos.
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

IDX <- SML$SampleID |> unique()

# IDX <- c("76J0025","93D0188","92J0783")


SID <- sample(IDX,1)
DATA <- SML[SampleID  %in%  SID ][WL<=WLmax & WL>=WLmin][,.(  WL,R,Rn,Rm)]


# I <- c(get_peaks(DATA$Rm),get_peaks(DATA$Rn)) |> unique()
I <- c(get_peaks(DATA$Rm)) |> unique()

PEAKS <- data.table(ID="peaks",X=DATA$WL[I],Y=DATA$Rn[I])
# ------------------------------------------------
SPECTRA <- list(
  # DATA[,.(ID="raw",X=WL, Y=R)],
  DATA[,.(ID="Rm",X=WL, Y=Rm)],
  DATA[,.(ID="Rn",X=WL, Y=Rn)],
  DATA[,.(ID="R",X=WL, Y=R)]
  
  
) |> rbindlist(use.names = TRUE)

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
  DATA[,.(ID="raw",X=WL, Y=R)]
) |> rbindlist(use.names = TRUE)
PEAKS <- data.table(ID="peaks",X=DATA$WL[I],Y=DATA$R[I],Y0=DATA$R[I],Y1=DATA$R[I]+DATA$Rn[I])

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

