get_convex_envelope <- function(x, y, type="upper") {
  
  hull_indices <- grDevices::chull(x, y)
  AUX <- data.table(x=x, y=y)
  hull_points <- AUX[hull_indices]
  setorder(hull_points, x)
  
  # Function to check for lower convexity
  is_lower_convex <- function(p1, p2, p3) {
    # Cross product to check for convexity
    # (p2 - p1) x (p3 - p2) should be positive for the lower convex boundary
    cross_product <- (p2[1] - p1[1]) * (p3[2] - p2[2]) - (p2[2] - p1[2]) * (p3[1] - p2[1])
    return(cross_product > 0)  # Positive cross product indicates lower convex hull
  }
  
  # Function to check for upper convexity
  is_upper_convex <- function(p1, p2, p3) {
    # Cross product to check for convexity
    # (p2 - p1) x (p3 - p2) should be negative for the upper convex boundary
    cross_product <- (p2[1] - p1[1]) * (p3[2] - p2[2]) - (p2[2] - p1[2]) * (p3[1] - p2[1])
    return(cross_product < 0)  # Negative cross product indicates upper convex hull
  }
  
  # Initialize a data.table for the convex envelope
  convex_envelope <- hull_points[1]
  
  # Loop through hull points and calculate either the upper or lower envelope
  for (i in 2:(nrow(hull_points) - 1)) {
    while (nrow(convex_envelope) >= 2) {
      # Extract the last two points from the current convex_envelope
      p1 <- convex_envelope[nrow(convex_envelope) - 1, .(x, y)]
      p2 <- convex_envelope[nrow(convex_envelope), .(x, y)]
      p3 <- hull_points[i, .(x, y)]
      
      # Apply the appropriate convexity test based on the type ("upper" or "lower")
      if (tolower(type) == "upper") {
        if (!is_upper_convex(as.numeric(p1), as.numeric(p2), as.numeric(p3))) {
          convex_envelope <- convex_envelope[-nrow(convex_envelope)]
        } else {
          break
        }
      } else if (tolower(type) == "lower") {
        if (!is_lower_convex(as.numeric(p1), as.numeric(p2), as.numeric(p3))) {
          convex_envelope <- convex_envelope[-nrow(convex_envelope)]
        } else {
          break
        }
      }
    }
    # Add the new point to the convex envelope
    convex_envelope <- rbind(convex_envelope, hull_points[i])
  }
  
  # Add the last point (end of the hull) before interpolation
  DTE <- rbind(convex_envelope, hull_points[.N])
  
  # Check and handle duplicate x values in DTE
  DTE <- DTE[, .(y = mean(y)), by = x]  # Average y for duplicate x values
  
  # Interpolate to get the envelope for the full range of x-values
  Ye <- approx(DTE$x, DTE$y, xout = x)$y-y
  
  
  return(Ye)
}

get_peaks <- function(x,smooth=FALSE,nema=12,ndiff=20,npeaks=10,threshold=0.025 ){
  
  x <- pmax(0,x)
  PM <- pracma::findpeaks( x/max(x),threshold=threshold) 
  if(is.null(PM)){
    return(data.table())
  }
  RANK <- data.table(A=PM[,1]*max(x),i=PM[,2])
  if(smooth==TRUE){
    xs <- (TTR::EMA(x, n=nema)+rev(TTR::EMA(rev(x), n=nema)))/2
    i <- which(diff(sign(diff(xs))) == -2) + 1
    RANK <- rbind(RANK,data.table(A=x[i],i=i))
  }
  
  
  RANK <- unique(RANK,by="i")
  RANK <- RANK[order(i),]
  # Step 2: Create groups based on the condition that x differences are less than 10
  RANK[, G := cumsum(c(TRUE, diff(i) > ndiff))]
  # Step 3: For each group, select the row with the maximum absorption (A)
  RANK <- RANK[, .SD[which.max(A)], by = G]
  RANK <- RANK[order(-A),]
  
  # Step 4: Drop the "group" column, if not needed
  # DT <- RANK[,.(A,i)] |> head(npeaks) |> na.omit()
  RANK <- RANK[order(i),]
  DT <- RANK$i |> head(npeaks) |> na.omit()
  
  
  return(DT)
}

getPeaks <- function(x,nema=12,ndiff=20,npeaks=10 ){
  # 
  WL <- x$WL
  R <- x$R
  Rue <- get_convex_envelope(x=WL,y=R,type="upper")
  Rn <- Rue-R
  PM <- pracma::findpeaks( Rn/max(Rn),threshold=0.025) 
  if(!is.null(PM)){
    RANK <- data.table(A=PM[,1]*max(Rn),WL=WL[PM[,2]],i=PM[,2])
  } else {
    RANK <- data.table()
  }
  
  
  Re <- (TTR::EMA(Rn, n=nema)+rev(TTR::EMA(rev(Rn), n=nema)))/2
  i <- which(diff(sign(diff(Re))) == -2) + 1
  RANK <- rbind(RANK,data.table(A=Re[i],WL=WL[i],i=i))
  RANK <- unique(RANK,by="WL")
  RANK <- RANK[order(WL),]
  # Step 2: Create groups based on the condition that WL differences are less than 10
  RANK[, G := cumsum(c(TRUE, diff(WL) > ndiff))]
  # Step 3: For each group, select the row with the maximum absorption (A)
  RANK <- RANK[, .SD[which.max(A)], by = G]
  RANK <- RANK[order(-A),]
  
  # Step 4: Drop the "group" column, if not needed
  DT <- RANK[,.(A,WL,R=R[i],Rn=Rn[i],Re=Re[i])] |> head(npeaks) |> na.omit()
  
  
  return(DT)
}

get_emd_envelope <- function(x,y,method="emd",noise.amp = .5e-7){
  # browser()
  IMF <- gmsp::get_imf(s=y,ts=x,method=method,noise.amp = noise.amp)
  if(is.null(IMF) || nrow(IMF)==0 ||ncol(IMF)==0){
    return(NULL)
  }
  
  DTE <- IMF[!(IMF %in% c("signal","residue")),.(y=-sum(s)),by=.(x=t)]
  Ye <- approx(DTE$x, DTE$y, xout = x)$y
  
  return(Ye)
}


library(highcharter)
library(data.table)

buildPlot.spectral <- function(
    data.spectra=NULL, # full spectral dataset
    data.points = NULL,  # subset of points to highlight, default is NULL
    data.columns = NULL,  # subset of points to highlight, default is NULL
    
    plot.subtitle = "",
    column.width = 10,
    plot.title = "",
    yAxis.label = "Intensity",  # Adjust label as per spectral data
    xAxis.label = "Wavelength", # Adjust label as per spectral data
    group.label = "ID",
    plot.palette = hcl.pals()[4],
    plot.theme = hc_theme_hcrt(),
    legend.layout = "vertical",
    legend.align = "right", 
    legend.valign = "top",
    legend.show = TRUE,
    plot.save = TRUE,
    group.legend.fontsize = "12px"
){
  
  # Ensure the dataset is ordered by ID and X (wavelength) to maintain proper plotting
  data.spectra <- data.spectra[order(ID, X)]
  
  # Get unique IDs and assign colors
  NID <- length(unique(data.spectra$ID))
  COLORS <- hcl.colors(n=NID, palette = plot.palette)
  data.spectra[, IDC := factor(ID, levels = unique(data.spectra$ID), labels = COLORS)]
  
  # Create the highchart object with spectral data
  HC <- highchart()
  HC <- HC |>
    hc_chart(style = list(fontFamily = "Helvetica"))|> 
    hc_legend(
      enabled = legend.show,
      align = legend.align,
      verticalAlign = legend.valign,
      layout = legend.layout,
      itemStyle = list(fontSize = group.legend.fontsize)
    ) 
  if (!is.null(data.spectra)){
    HC <- HC |>
      hc_add_series(
        data = data.spectra,
        type = "spline",   # Use "line" if you want a straight line instead of spline
        hcaes(x = X, y = Y, group = ID, color = IDC)
      )
  }
  
  
  
  
  
  # Add the highlight points, only if data.points is provided
  if (!is.null(data.points)) {
    HC <- HC |>
      hc_add_series(
        data = data.points,
        type = "scatter",
        hcaes(x = X, y = Y, group = ID, color = ID),
        marker = list(radius = 4, symbol = "+")
      )
  }
  
  if (!is.null(data.columns)) {
    # Column plot for data.points with thicker, grey-shaded columns
    HC <- HC |>
      hc_add_series(
        data = data.columns, 
        type = "columnrange", 
        hcaes(x = X, low = Y0, high=Y1,group = X),  # No need to color by X here, use fixed color
        color = "lightblue",  # Set color to light grey
        pointWidth = column.width  # Adjust the width of the columns (increase for thicker columns)
      )
  }
  
  
  # Set axis configurations
  HC <- HC |> 
    hc_colors(COLORS) |>   
    hc_xAxis(
      type = "linear",
      title = list(text = xAxis.label)
    ) |>
    hc_yAxis(
      type = 'linear', 
      title = list(text = yAxis.label)
    ) |>
    hc_tooltip(
      shared = FALSE,  
      crosshairs = TRUE
    ) |>
    hc_title(text = plot.title) |>
    hc_subtitle(text = plot.subtitle) |>
    hc_add_theme(plot.theme) |> 
    hc_exporting(
      enabled = plot.save,
      type = "application/pdf",
      width = 800,
      scale = 3
    )
  
  return(HC)
}
