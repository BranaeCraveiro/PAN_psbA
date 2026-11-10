library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(repr)

setwd("C:\\Local_Files\\PAN_psbA\\analysis")

sst <- read_csv('C:\\Local_Files\\PAN_psbA\\analysis\\sst.csv') %>% select(-1)
dhw <- read_csv('C:\\Local_Files\\PAN_psbA\\analysis\\crw_dhw.csv') %>% select(-1)

# drop coordinates from sst 
sst<- sst %>% select(-latitude, -longitude)
# and filter duplicate rows 
sst <- sst %>% distinct(site, date, time, .keep_all = TRUE)
dhw <- dhw %>% distinct(site, date, time, .keep_all = TRUE)

unique(dhw$site)
unique(sst$site)

# match date formats 
dhw$date <- as.Date(dhw$date, format = "%m/%d/%Y")
sst$date <- as.Date(sst$date, format = "%m/%d/%Y")

# label time cols
dhw<-dhw %>%
  rename(time_dhw = time)
sst<-sst %>%
  rename(time_sst = time)

# combine
temp<-full_join(sst,dhw,by = c('site','date'))

unique(temp$site)
head(temp,3)

# convert date to month year to match other formats 
temp <- temp %>%
  rename(full_date = date) %>%
  mutate(date = format(full_date, format = '%b %Y'))

# Satellite Temp Plots ------------------------------------------------------------
min(temp$full_date)

temp[rowSums(is.na(temp)) > 0, ]

# peak sst 
print('peak sst')
max(temp$CRW_SST, na.rm = TRUE)
temp$full_date[which.max(temp$CRW_SST)]
# peak dhw 
print('peak dhw')
max(temp$CRW_DHW, na.rm = TRUE)
temp$full_date[which.max(temp$CRW_DHW)]


options(repr.plot.width=15, repr.plot.height=8)
# plot temp 
ggplot(temp, aes(x = full_date,)) + 
  geom_line(aes(y = CRW_SST), color = 'blue', linewidth = 1) + 
  scale_x_date(
    date_labels = "%b %y", 
    date_breaks = "4 month") +
  geom_vline(
    xintercept = temp$full_date[which.max(temp$CRW_SST)],
    color = "red", linetype = "dashed", size = 1
  ) +
  theme_minimal()+
  labs(title = 'Plot 1: SST',
       y = 'Sea Surface Temp (°C)',
       x = 'Date')+
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),                                          
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)           
  )

# df for crw alert values 
hline_df <- data.frame(
  y = c(max(temp$CRW_DHW, na.rm = TRUE), 12, 16, 20),
  label = c("Peak DHW", "CRW Alert 3", 
            "CRW Alert 4", "CRW Alert 5"),
  color = c("darkblue", "coral2", "coral3", "coral4"),
  linetype = c("dashed", "dashed", "dashed", "dashed")
)

# plot dhw 
dhw_plot<-ggplot(temp, aes(x = full_date)) + 
  geom_line(aes(y = CRW_DHW), color = 'coral1', size = 1) +
  geom_hline(aes(yintercept = y, color = label, linetype = label), data = hline_df, size = 1) +
  scale_color_manual(values = setNames(hline_df$color, hline_df$label)) +
  scale_linetype_manual(values = setNames(hline_df$linetype, hline_df$label)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "4 month") +
  theme_minimal(base_size = 12) +
  labs(title = "DHW in Belize", x = "Date", y = "CRW DHW", color = "Legend", linetype = "Legend")+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),                                          
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)           
  )
dhw_plot

hline_df

temp_2023<-temp %>%
  filter(format(full_date, "%Y") == "2023")
print('max dhw in 2023:')
max(temp_2023$CRW_DHW, na.rm = TRUE)

# this data has multiple sites, so we have multiple data points on the same date - so thats where the shading is coming from 
# blue line is max dhw from our data 

summary(temp$full_date)

temp_2024<-temp %>%
  filter(full_date <= as.Date("2024-09-01"))


# graph for poster --------------------------------------------------------

# scale DHW so it fits on the same graph as SST ----- help from claude
sst_min <- min(temp_2024$CRW_SST, na.rm = TRUE)
k <- diff(range(temp_2024$CRW_SST, na.rm = TRUE)) /
  max(temp_2024$CRW_DHW, na.rm = TRUE)

ggplot(temp_2024, aes(x = full_date)) +
  geom_line(aes(y = CRW_SST, color = "Sea Surface Temperature"), linewidth = 1.2) +
  geom_line(aes(y = CRW_DHW * k + sst_min, color = "Degree Heating Weeks"), linewidth = 1.2) +
  geom_vline(xintercept = temp_2024$full_date[ which.max(replace(temp_2024$CRW_SST, is.na(temp_2024$CRW_SST), -Inf))],
    color = "#003F7F",linetype = "dashed") +
  geom_vline(xintercept = temp_2024$full_date[which.max(replace(temp_2024$CRW_DHW, is.na(temp_2024$CRW_DHW), -Inf))],
             color = "#B34700", linetype = "dashed") +
  scale_color_manual( values = c("Sea Surface Temperature" = "#56B4E9",
               "Degree Heating Weeks" = "#F5B041")) +
  scale_y_continuous(name = "Sea Surface Temperature (°C)",
    sec.axis = sec_axis(~ (. - sst_min) / k, name = "Degree Heating Weeks (DHW)")) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "4 month") +
  
  labs(title = "Sea Surface Temperature and Degree Heating Weeks", x = "Date",
    color = "Legend") +
  
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 18, hjust = 0.5), 
    axis.title.y       = element_text(color = "#003F7F"),  
    axis.title.y.right = element_text(color = "#B34700"))

ggsave("sst_dhw_poster.png", width = 9, height = 6, units = "in", dpi = 300)