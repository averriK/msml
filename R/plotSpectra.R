source("R/setup.R")
source("R/utils.R")
SXL <- fread("data/SXL.csv") 
Yo <- fread("data/Yo.csv")
YoID <- "Ag"
SampleID_target <- Yo[ElementID==YoID & Y>0]$SampleID
DATA <- SXL[SampleID %in% SampleID_target,.(X=WL,Y=A,ID=SampleID)]
buildPlot(
  library = "highcharter",
  data = DATA, # full spectral dataset
  plot.type="spline",
  plot.subtitle = "",
  plot.title = "",
  yAxis.label = "Absortion",  # Adjust label as per spectral data
  xAxis.label = "Wavelength", # Adjust label as per spectral data
  # group.label = "ID",
  # plot.palette = hcl.pals()[4],#[80],
  legend.layout = "vertical",
  legend.align = "right", 
  legend.valign = "top",
  legend.show = FALSE,
  plot.save = TRUE,
  group.legend.fontsize = "12px"
)
