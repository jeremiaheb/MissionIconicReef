library(rvc)
library(tidyverse)
library(ggh4x)
library(reshape2)


binned_LenFreq = function(d, spp, bin_size, yrs = NULL, st = NULL,colName) {

  a = getDomainLengthFrequency(d, species = spp, years = yrs, status = st )
  b = merge(a, data.frame(length_class = seq(1,max(a$length_class),0.5)), all.y =T) %>%
    select(length_class, frequency) %>%
    replace(., is.na(.), 0) %>%
    mutate(bin= as.numeric(cut(length_class, seq(0,max(length_class) + 5,bin_size))))
  c <- b %>%
    group_by(bin) %>%
    summarise(freq = sum(frequency))

  colnames(c)[2] <- colName

  return(c)

}

compare_LengthFreq = function(a,b){
  library(reshape2)
  c <- merge(a,b,all = T)
  c[is.na(c)] <- 0
  d <- melt(c, id = c("bin"))
return(d)
}

melt_lenfreqs <- function(x) {
  library(reshape2)
  return(melt(x, id = c("bin")))

}

QL_plot_comparison = function(x, ttle, bin_size, vline_at_lc = NULL){

  br <- c(seq(1:max(x$bin)))
  la <- labeler(bin_num = max(x$bin), bin_size = bin_size)


  e <- ggplot(x, aes(x=bin, y=value, fill=variable)) +
    geom_bar(stat="identity", position = "dodge2", width = .9, color="black", size=.5) +
    scale_x_discrete(breaks = br,
                     labels = la[1:(max(x$bin))],
                     limits = factor(br)) +
    theme_Publication(base_size = 20)+
    theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1),
          axis.text = element_text(size = 12)) +
    scale_fill_manual(values=c('springgreen3','deepskyblue4','gold1')) +
    # scale_fill_manual(values=c('deepskyblue4','gold1')) +
    labs(title = ttle, fill = "Sampling Year") + ylab("Frequency") + xlab("Fork Length (cm)") +
    facet_wrap(~Spp_Name, ncol=2, scales = "free") +
    geom_vline(xintercept = (vline_at_lc/bin_size), linetype = "longdash", size = 1)

  # ggsave(filename = paste("SE_Florida","_LF_timeseries_1.jpg", sep = ""), plot = e, width = 9.8, height = 7.5, dpi = 300, units = "in", device = "jpg")
    return(e)
}

plot_bins = function(x, ttle, bin_size, vline_at_lc = NULL){

  br <- c(seq(1:max(x$bin)))
  la <- labeler(bin_num = max(x$bin), bin_size = bin_size)


  e <- ggplot(x, aes(x=bin, y=value, fill=variable)) +
    geom_bar(stat="identity", position = "dodge2", width = .9, color="black", size=.5) +
    scale_x_discrete(breaks = br,
                     labels = la[1:(max(x$bin))],
                     limits = factor(br)) +
    theme_Publication(base_size = 20)+
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle=45, vjust = 1, hjust = 1),
          axis.text = element_text(size = 12)) +
    scale_fill_manual(values=c('springgreen3','deepskyblue4','gold1')) +
    labs(title = ttle, fill = "Sampling Year") + ylab("Frequency") + xlab("Fork Length (cm)") +
    geom_vline(xintercept = (vline_at_lc/bin_size), linetype = "longdash", size = 1)

  return(e)
}

plot_bins_yr <- function(x, ttle, bin_size, vline_at_lc = NULL, category, spp_name) {
  br <- c(seq(1:max(x$bin)))
  la <- labeler(bin_num = max(x$bin), bin_size = bin_size)

  e <- ggplot(x, aes(x = bin, y = value, fill = factor(YEAR))) +
    geom_bar(stat = "identity", position = "dodge2", width = 0.9, color = "black", size = 0.5) +
    scale_x_discrete(breaks = br,
                     labels = la[1:(max(x$bin))],
                     limits = factor(br)) +
    theme_Publication(base_size = 20) +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.text = element_text(size = 12)) +
    scale_fill_manual(values = c('springgreen3', 'deepskyblue4', 'gold1')) +
    labs(title = ttle, fill = "Sampling Year") +  # Title with spp_name and category
    ylab("Frequency") +
    xlab("Fork Length (cm)") +
    geom_vline(xintercept = (vline_at_lc / bin_size), linetype = "longdash", size = 1)

  return(e)
}


# plot_bins <- function(dframe, bin_size, title) {
#
#   br <- c(seq(1:max(dframe$bin)))
#   la <- labeler(bin_num = max(dframe$bin), bin_size = bin_size)
#
#   ggplot(data = dframe, aes(x = bin, y = value)) +
#     geom_bar(stat = "identity") +
#     scale_x_continuous("Length(cm)", breaks = br,
#                      labels = la[1:(max(dframe$bin))],
#                      limits = br) +
#     scale_y_continuous(name = "Frequency") +
#     ggtitle(title)
#
#
# }

lenFreq_MultiYear_by_Subregion <- function(df, spp, bin_size, subreg_name) {
  # yearList = unique(df$sample_data$YEAR)
  yearList = tail(unique(df$sample_data$YEAR),3)

  dataList <- list()
  for(i in yearList){

    a <- binned_LenFreq(d = df, spp = spp, yrs = i, bin_size = bin_size, colName = i)
    dataList[[as.character(i)]] <- a
  }

  l <- Reduce( function(x, y, ...) merge(x, y, all = TRUE, ...), dataList ) %>%
    replace(., is.na(.), 0) %>%
    melt(., id = c("bin")) %>%
    mutate(REGION = subreg_name)

  return(l)

}

lenFreq_MultiYear_by_spp <- function(df, spp, bin_size, yrs = NULL, spp_name) {
  library(reshape2)
  y = tail(unique(df$sample_data$YEAR),3)
  yearList= if(is.null(yrs)) y else yrs

  dataList <- list()
  for(i in yearList){

    a <- binned_LenFreq(d = df, spp = spp, yrs = i, bin_size = bin_size, colName = i)
    dataList[[as.character(i)]] <- a
  }

  l <- Reduce( function(x, y, ...) merge(x, y, all = TRUE, ...), dataList ) %>%
    replace(., is.na(.), 0) %>%
    melt(., id = c("bin")) %>%
    mutate(Spp_Name = spp_name)

  return(l)

}
