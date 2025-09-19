# Kilian M. Stehfest, Toby A. Patterson, Adam Barnett, Jayson M. Semmens

# Markov models and network analysis reveal sex-specific differences in the space-use of a coastal apex predator

# R code for the 'sharkov' function which computes the transition probability matrix
# or the dominant eigenvector of the transpose of the transition probability matrix from passive acoustic telemetry data

# Input data is a dataframe of hourly acoustic detections from an array of passive acoustic receivers with columns:
# id = factor vector of individual identifiers,
# time = the hourly detection time/date stamps in POSIXct format (YYYY-MM-DD hh:mm:ss)
# state = the receiver or receiver group identifier, can be numeric or character but must not be '0' as this is defined as 'no detection' i.e. the absent state in the code

# If return.matrix=TRUE, the Markov probability matrix is returned, otherwise the dominant eigenvetor of the probability matrix is returned
# niter determines the number of iterations for the power method computation of the dominant eigenvector

# Please note that this method is likely to be slow for large datasets and could be made more efficient with compiled code

sharkov<-function(data, return.matrix=T, niter=10000) {
  
  # Create regular hourly time sequence and add states (state 0 = unknown (i.e. no detection))
  makeseq<-function(d) {
    tseq<-data.frame(id=unique(d$id), time=seq.POSIXt(min(d$time), max(d$time), "hours"))
    tseq$state<-d$state[match(as.numeric(tseq$time), as.numeric(d$time))]
    tseq$state[is.na(tseq$state)]<-0
    return(tseq)
  }
  
  require(plyr)
  newdata<-ldply(split(data, as.factor(data$id)), makeseq)
  
  
  # Compute Markov transition probability matrix
  maketran<-function(X, states) {
    # Create empty transition matrix array
    tran<-array(dim=c(length(states),length(states)))
    colnames(tran)<-states
    rownames(tran)<-states
    tran[,]<-0
    
    #Fill transition matrices with transition counts
    for (i in 1:nrow(X)) {
      tran[as.character(X[i-1, "state"]), as.character(X[i, "state"])]<-tran[as.character(X[i-1, "state"]), as.character(X[i, "state"])]+1
    }
    return(tran)
  }
  
  states<-unique(newdata$state)
  tranmat<-lapply(split(newdata, newdata$id), maketran, states)
  
  # Sum number of transitions for all individuals and calculate transition probabilities by dividing each row element by the row sum
  total<-Reduce('+', tranmat)
  for (i in 1:nrow(total)) {
    total[i,]<-total[i,]/rowSums(total)[i]
  }
  
  if (return.matrix) {return(total)}
  else {
    # Compute dominant eigenvector of the transpose of the transition probability matrix using the power method
    power.method<-function(Mat, iter){
      n<-nrow(Mat)
      x <- rep(1,n)
      for (i in 1:iter) {x <- Mat%*%x}
      de<-x/sum(x)
      return(de)
    }
    eigenv<-power.method(t(total), niter)
    return(eigenv)
  }
}

# set up receivers locations 
stations <- HalaviArray %>% filter(otn_array == "QUMAN") %>%
  distinct(station_no, .keep_all = T)  %>% select(station_no, deploy_lat, deploy_long)

stations$station_no[stations$station_no == "Qu3"] <- "North"

rownames(stations) <- stations$station_no


# season function
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}


# EDMC function  -------------------------------------------------------

calculate_edmc <- function(col_name, value) {
  df <- dets %>% 
    filter(otn_array == "QUMAN") %>% 
    mutate(season  = getSeason(detection_timestamp_utc)) %>% 
    filter({{col_name}} == value) %>% 
    dplyr::select(transmitter_id,detection_timestamp_utc, station_no) %>% 
    mutate(station_agg = if_else(station_no %in% c("Qu3", "Qu2"), 'North', station_no)) %>% 
    dplyr::rename(id = transmitter_id,
                  time = detection_timestamp_utc,
                  state = station_agg) %>% 
    select(-station_no) %>%
    mutate(
      half_hour = floor_date(time, unit = "1 hour")  # bin into 30-min intervals
    ) %>%
    group_by(id, half_hour, state) %>%
    dplyr::count(state) %>%
    slice_max(order_by = n, n = 1, with_ties = FALSE) %>%  # pick most frequent state
    ungroup() %>% 
    select(-n) %>% 
    dplyr::rename(time = half_hour)
  
  matrix_values <- sharkov(df, return.matrix=T) 
  
  eig <- sharkov(df, return.matrix=F) 
  
  
  # some light clean up
  rows_to_remove <- "0"
  
  matrix_values <- matrix_values[!(row.names(matrix_values) %in% rows_to_remove), ]
  matrix_values <- matrix_values[, colnames(matrix_values) != "0"]
  
  
  eig <- eig %>% as.data.frame() %>% tibble::rownames_to_column("station")
  
  return(list(matrix_values, eig))
}


# Mapping needs -----------------------------------------------------------

library(ggspatial)
library(patchwork)
library(units)
library(grid)
library(kableExtra)
library(sf)

world <- rnaturalearth::ne_countries(returnclass = "sf", scale = 50)
shape.data <- sf::st_read("SpatialData/AlWajhIslands/AlWajhIslands.shp")

# Quman shapefile
na_quman_wgs <- st_transform(shape.data %>% filter(IslandName == "Quman"), crs = 4326)

# EDMC plot function ---------------------------------------------------------------

edmc_plot <- function(edmc_df) {
  # change depending on what you want to look at
  matrix_values <- edmc_df[[1]]
  eig <- edmc_df[[2]]
  
  tdg <- tibble::rownames_to_column(data.frame(matrix_values), "to") %>%
    pivot_longer(
      cols = !starts_with("to"),
      names_to = "from",
      values_to = "score"
    )  %>% left_join(stations, by = c("to" = "station_no")) %>% 
    left_join(stations, by = c("from" = "station_no")) %>% 
    left_join(eig, by = c("to" = "station"))
  

  ggplot() +
    geom_sf(data = na_quman_wgs) +
    geom_curve(data = tdg %>%
                 filter(from != to) %>%
                 filter(score > 0.05),
               aes(x = deploy_long.x, y = deploy_lat.x,
                   xend = deploy_long.y, yend = deploy_lat.y,
                   alpha = ifelse(score == 0, 0, 1),
                   colour = score, linewidth = score),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               curvature = 0.2) +
    geom_point(data = tdg %>%
                 filter(from == to),
               aes(deploy_long.x, deploy_lat.x,
                   color = score),
               alpha = 0.75, size = 10) +
    geom_point(data = tdg %>% 
                 filter(from == to), 
               aes(deploy_long.x, deploy_lat.x, size = V1),
               alpha = 1, shape = 23, fill = "black") +
    scale_alpha_identity() +
    scale_fill_continuous(type = "viridis", limits = c(0,1)) +
    scale_color_continuous(type = "viridis", limits = c(0,0.85)) +
    scale_size_continuous(breaks=c(0, 0.01, 0.05,0.1, 0.2, 0.31),
                          labels=c("0", "1%", "5%", "10%", "20%", "30%"),
                          limits = c(0,0.31)) +
    scale_linewidth(range = c(1,2), guide = "none") +
    labs(color = "Transition Probability", size = " Residency Probability") +
    theme_void()
  
  
}



# overall -----------------------------------------------------------------

overall <- calculate_edmc(col_name = otn_array, value = "QUMAN")

# sex ---------------------------------------------------------------------

edmc_m <- calculate_edmc(col_name = sex, value = "M")
edmc_f <- calculate_edmc(col_name = sex, value = "F")

# age ---------------------------------------------------------------------

edmc_yoy <- calculate_edmc(col_name = new_class, value = "YOY")
edmc_juv <- calculate_edmc(col_name = new_class, value = "JUV")

# season ------------------------------------------------------------------

edmc_spring <- calculate_edmc(col_name = season, value = "Spring")
edmc_summer <- calculate_edmc(col_name = season, value = "Summer")
edmc_fall <- calculate_edmc(col_name = season, value = "Fall")
edmc_winter <- calculate_edmc(col_name = season, value = "Winter")

