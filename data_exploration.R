#setwd("C:/Users/yusufihs/Desktop/OneDrive - University at Buffalo/University/Research/Border Data Analysis Paper")

library(tidyverse)
library(scales)
library(tidytext)
library(dplyr)
library(svglite)
library(RColorBrewer)

D0 <- read.csv("nationwide-encounters-fy20-fy24-aug-aor.csv")

D0$Fiscal.Year <- gsub(" \\(FYTD\\)", "", D0$Fiscal.Year)
D0$Fiscal.Year <- as.numeric(D0$Fiscal.Year)

D0 <- D0 %>%
  mutate(Calendar.Year = ifelse(Month..abbv. %in% c("JAN", "FEB", "MAR", 
                                                    "APR", "MAY", "JUN", 
                                                    "JUL", "AUG", "SEP"),
                                Fiscal.Year,
                                Fiscal.Year - 1
                                ))

D0 <- D0 %>%
  mutate(Quarter = case_when(
    Month..abbv. %in% c("JAN", "FEB", "MAR") ~ "Q1",
    Month..abbv. %in% c("APR", "MAY", "JUN") ~ "Q2",
    Month..abbv. %in% c("JUL", "AUG", "SEP") ~ "Q3",
    Month..abbv. %in% c("OCT", "NOV", "DEC") ~ "Q4"
  ))

D0$Month..abbv. <- factor(D0$Month..abbv., levels = c("JAN", "FEB", "MAR", 
                                                      "APR", "MAY", "JUN", 
                                                      "JUL", "AUG", "SEP", 
                                                      "OCT", "NOV", "DEC"))
D0$Calendar.Year <- factor(D0$Calendar.Year, levels = c("2020", "2021", "2022",
                                                        "2023", "2024"))
D0$Title.of.Authority <- factor(D0$Title.of.Authority, levels = c("Title 8", 
                                                                  "Title 42"))
D0$Area.of.Responsibility <- as.factor(D0$Area.of.Responsibility)
D0$Demographic <- factor(D0$Demographic, levels = c("UC / Single Minors",
                                                    "FMUA", "Single Adults", 
                                                    "Accompanied Minors"))
D0$AOR..Abbv. <- as.factor(D0$AOR..Abbv.)
# Convert Calendar.Year and Month..abbv. to a single factor variable with correct levels
D0$Quarter_Year <- factor(paste(D0$Calendar.Year, D0$Quarter, sep = "-"))

D0$Quarter_Year <- factor(D0$Quarter_Year, levels = c("2020-Q1", "2020-Q2", 
                                                  "2020-Q3", "2020-Q4", 
                                                  "2021-Q1", "2021-Q2", 
                                                  "2021-Q3", "2021-Q4",
                                                  "2022-Q1", "2022-Q2", 
                                                  "2022-Q3", "2022-Q4",
                                                  "2023-Q1", "2023-Q2", 
                                                  "2023-Q3", "2023-Q4",
                                                  "2024-Q1", "2024-Q2", 
                                                  "2024-Q3"))

D0$Month_Year <- factor(paste(D0$Calendar.Year, D0$Month..abbv., sep = "-"))
D0$Month_Year <- factor(D0$Month_Year, levels = c("2020-JAN", "2020-FEB", 
                                                  "2020-MAR", "2020-APR", 
                                                  "2020-MAY", "2020-JUN", 
                                                  "2020-JUL", "2020-AUG", 
                                                  "2020-SEP", "2020-OCT", 
                                                  "2020-NOV", "2020-DEC", 
                                                  "2021-JAN", "2021-FEB", 
                                                  "2021-MAR", "2021-APR", 
                                                  "2021-MAY", "2021-JUN", 
                                                  "2021-JUL", "2021-AUG", 
                                                  "2021-SEP", "2021-OCT", 
                                                  "2021-NOV", "2021-DEC", 
                                                  "2022-JAN", "2022-FEB", 
                                                  "2022-MAR", "2022-APR", 
                                                  "2022-MAY", "2022-JUN", 
                                                  "2022-JUL", "2022-AUG", 
                                                  "2022-SEP", "2022-OCT", 
                                                  "2022-NOV", "2022-DEC", 
                                                  "2023-JAN", "2023-FEB", 
                                                  "2023-MAR", "2023-APR", 
                                                  "2023-MAY", "2023-JUN", 
                                                  "2023-JUL", "2023-AUG", 
                                                  "2023-SEP", "2023-OCT", 
                                                  "2023-NOV", "2023-DEC",
                                                  "2024-JAN", "2024-FEB",
                                                  "2024-MAR", "2024-APR",
                                                  "2024-MAY", "2024-JUN",
                                                  "2024-JUL", "2024-AUG"))

D0$Citizenship <- gsub("CHINA, PEOPLES REPUBLIC OF", "CHINA", D0$Citizenship)
D0$Citizenship <- gsub("MYANMAR \\(BURMA\\)", "MYANMAR", 
                       trimws(D0$Citizenship), ignore.case = TRUE)
figures_directory <- "figures"
options(scipen = 99)

###General Analysis

#by all countries and years
world_by_countries_yearly <- D0 %>%
  group_by(Calendar.Year, Citizenship) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Calendar.Year, desc(Encounter.Count))

world_by_countries_yearly$Citizenship <- factor(
  world_by_countries_yearly$Citizenship, 
  levels = unique(world_by_countries_yearly$Citizenship)
)

fig_world_by_countries_yearly <- ggplot(world_by_countries_yearly, aes(x = Citizenship, y = Encounter.Count, fill = Citizenship)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Calendar.Year, scales = "free_x", nrow = 5) +
  labs(
    title = "",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    strip.text = element_text(size = 14),          
    axis.text.y = element_text(size = 12),         
    axis.title.y = element_text(size = 14)
  ) +
  guides(fill = guide_legend(ncol = 1)) +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange", 
                               "yellow", "darkgreen", "darkblue", "black", 
                               "magenta", "darkorange", "cyan", "darkcyan",
                               "deeppink4", "cadetblue", "burlywood", "darkgray",
                               "darkorchid", "brown", "azure2", "cadetblue3", 
                               "darkviolet"))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "world_yearly_bycountries.pdf"), 
       plot = fig_world_by_countries_yearly, units = "px", bg="white", 
       width = 2400, height = 2400)

#aor general analysis
world_by_aors_yearly <- subset(D0, Land.Border.Region == "Southwest Land Border" 
                               & Component == "U.S. Border Patrol") %>%
  group_by(Calendar.Year, Area.of.Responsibility) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Calendar.Year, desc(Encounter.Count))

world_by_aors_yearly$Area.of.Responsibility <- gsub(" Sector", "", 
                                                    world_by_aors_yearly$
                                                      Area.of.Responsibility)

world_by_aors_yearly$Area.of.Responsibility <- factor(
  world_by_aors_yearly$Area.of.Responsibility, 
  levels = unique(world_by_aors_yearly$Area.of.Responsibility)
)



fig_world_by_aors_yearly <- ggplot(world_by_aors_yearly, 
                                   aes(x = Area.of.Responsibility, 
                                       y = Encounter.Count,
                                       fill = Area.of.Responsibility)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Calendar.Year, scales = "free_x", nrow = 5) +
  labs(
    title = "",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_comma()) +
  theme(axis.text.x = element_blank(),
        strip.text = element_text(size = 14),          
        axis.text.y = element_text(size = 12),         
        axis.title.y = element_text(size = 14)
        ) +
  guides(fill = guide_legend(ncol = 1)) +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange", 
                               "yellow", "darkgreen", "darkblue", "black"))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "world_yearly_byaors.pdf"), 
       plot = fig_world_by_aors_yearly, units = "px", bg="white", 
       width = 2400, height = 2400)

#figure for different trends
D0_trends <- subset(D0, (Citizenship == "CHINA" | Citizenship == "INDIA" |
                      Citizenship == "BRAZIL" | Citizenship == "ROMANIA" |
                      Citizenship == "MEXICO" | Citizenship == "MYANMAR") & 
                      (Calendar.Year == 2021 | Calendar.Year == 2022 |
                      Calendar.Year == 2023 | Calendar.Year == 2024))
trends_data_monthly <- D0_trends %>%
  group_by(Month_Year, Citizenship) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

trends_data_monthly$Citizenship <- factor(trends_data_monthly$Citizenship, 
                                             levels = c('CHINA', 'INDIA', 
                                                        'BRAZIL', 'ROMANIA', 
                                                        'MEXICO', 'MYANMAR'))

fig_trends_by_countries <- ggplot(trends_data_monthly, aes(x = Month_Year, y = Encounter.Count, color = Citizenship, group = Citizenship)) +
  geom_line(size = 1) +  # Draw lines for encounters over time
  geom_point(size = 3) + # Add points for better visibility
  facet_wrap(~ Citizenship, nrow = 3, ncol = 2, scales = "free_y") +  # 3 rows, 2 columns
  labs(
    title = "",
    x = "",
    y = "",
    color = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 16),
    legend.position = "none",  
  )

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "trends_bycountries.pdf"), 
       plot = fig_trends_by_countries, units = "px", bg="white", 
       width = 4800, height = 3200)

#by land border region
world_byregion_monthly <-D0 %>%
  group_by(Month_Year, Land.Border.Region) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))


fig_world_byregion <- ggplot(world_byregion_monthly, aes(x = Month_Year, y = Encounter.Count, 
                                                                 fill = Land.Border.Region)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Encounter Count by Month and Region",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "world_monthly_byregion.svg"), 
       plot = fig_world_byregion, units = "px", bg="white", 
       width = 3200, height = 1600)

#by fields of operations in southwest border
D0_southwest <- subset(D0, Land.Border.Region == "Southwest Land Border")

world_bycomponent_monthly <-D0_southwest %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: "Total Encounters by Month and Component in Southwest Border"
fig_world_bycomponent <- ggplot(world_bycomponent_monthly, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))


ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "world_monthly_bycomponent_southwest.png"), 
       plot = fig_world_bycomponent, units = "px", bg="white", 
       width = 3200, height = 1600)

###China

D0_china <- subset(D0, Citizenship == "CHINA")

#monthly encounters by border category in china titled as "Encounter Count by Month and Region for Chinese Nationals"

china_byregion_monthly_summary <- D0_china  %>%
  group_by(Month_Year, Land.Border.Region) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

fig_china_byregion <- ggplot(china_byregion_monthly_summary, aes(x = Month_Year, y = Encounter.Count, 
                                           fill = Land.Border.Region)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))  # Center the title

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "china_monthly_byregion.pdf"), 
       plot = fig_china_byregion, units = "px", bg="white", 
       width = 2400, height = 800)

#monthly encounters by demographics in china

china_demmonthly_summary <- D0_china %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

fig_china_bydemographics <- ggplot(china_demmonthly_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Encounter Count by Month and Demographics for Chinese Nationals (USBP & OFO combined)",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "china_monthly_bydemographics.png"), 
       plot = fig_china_bydemographics, units = "px", bg="white", 
       width = 3200, height = 1600)

#monthly encounters in southwest border by fields of operations in china

china_monthlysw_summary <- subset(D0_china, Land.Border.Region == "Southwest Land Border")  %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

fig_china_bycomponent <- ggplot(china_monthlysw_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Encounter Count by Month and Component in Southwest Border for Chinese Nationals",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))


ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "china_monthly_bycomponent_southwest.png"), 
       plot = fig_china_bycomponent, units = "px", bg="white", 
       width = 3200, height = 1600)

#monthly encounters in northern border by fields of operations in china

china_monthlyn_summary <- subset(D0_china, Land.Border.Region == "Northern Land Border")  %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

fig_china_bycomponentn <- ggplot(china_monthlyn_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Encounter Count by Month and Component in Northern Border for Chinese Nationals",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "china_monthly_bycomponent_north.png"), 
       plot = fig_china_bycomponentn, units = "px", bg="white", 
       width = 3200, height = 1600)

#monthly encounters in southwest border by demographics in china

china_demmonthlysw_summary <- subset(D0_china, Land.Border.Region 
                                     == "Southwest Land Border")  %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

fig_china_bydemographicssw <- ggplot(china_demmonthlysw_summary, 
                                   aes(x = Month_Year, 
                                       y = Encounter.Count,
                                       fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Encounter Count by Month and Demographics in Southwest Border for Chinese Nationals",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "china_monthly_bydemographicssw.png"), 
       plot = fig_china_bydemographicssw, units = "px", bg="white", 
       width = 3200, height = 1600)



#monthly encounters by sectors
#2022-2024 is chosen because prior to these years the data was negligible
#only the active 4 sectors have chosen to show the details,
#other 5 sectors have either no or minimum activity after 2020

china_monthlysw_bysector <- subset(D0_china, (Land.Border.Region == "Southwest Land Border" 
                                              & Component == "U.S. Border Patrol"
                                              & (Calendar.Year == 2022 | Calendar.Year == 2023 | Calendar.Year == 2024)
                                              & (Area.of.Responsibility == "El Centro Sector" | 
                                                   Area.of.Responsibility == "Rio Grande Valley Sector" | 
                                                   Area.of.Responsibility == "San Diego Sector" | 
                                                   Area.of.Responsibility == "Yuma Sector"))) %>%
  group_by(Month_Year, Area.of.Responsibility) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Month_Year, Encounter.Count)

fig_china_byaor <- ggplot(china_monthlysw_bysector, aes(x = Month_Year, y = Encounter.Count,color = Area.of.Responsibility)) +
  geom_point() +
  geom_line(linewidth = 1.2, aes(group = Area.of.Responsibility)) +
  labs(title = "Total Number of Chinese Encounters per Month by AOR ",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for better readability
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  facet_wrap(~Area.of.Responsibility, ncol = 2) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  guides(color = FALSE)  # Remove the legend for color

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "china_monthly_byaor.png"), 
       plot = fig_china_byaor, units = "px", bg="white", 
       width = 3200, height = 1600)


###India

D0_india <- subset(D0, Citizenship == "INDIA")

#monthly encounters by border category in india

india_byregion_monthly_summary <- D0_india  %>%
  group_by(Month_Year, Land.Border.Region) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

fig_india_byregion <- ggplot(india_byregion_monthly_summary, 
                             aes(x = Month_Year, y = Encounter.Count, 
                                           fill = Land.Border.Region)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "india_monthly_byregion.pdf"), 
       plot = fig_india_byregion, units = "px", bg="white", 
       width = 2400, height = 800)

#monthly encounters in southwest border by fields of operations in india

india_monthlysw_summary <- subset(D0_india, Land.Border.Region == "Southwest Land Border")  %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Component in Southwest Border for Indian Nationals
fig_indiasw_bycomponent <- ggplot(india_monthlysw_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "indiasw_monthly_bycomponent.pdf"), 
       plot = fig_indiasw_bycomponent, units = "px", bg="white", 
       width = 2400, height = 800)

#monthly encounters in northern border by fields of operations in india

india_monthlyn_summary <- subset(D0_india, Land.Border.Region == "Northern Land Border")  %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Component in Northern Border for Indian Nationals
fig_indian_bycomponent <- ggplot(india_monthlyn_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "indian_monthly_bycomponent.pdf"), 
       plot = fig_indian_bycomponent, units = "px", bg="white", 
       width = 2400, height = 800)

#monthly encounters in southwest border by demographics in india

india_demmonthlysw_summary <- subset(D0_india, Land.Border.Region == "Southwest Land Border")  %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Demographics in Southwest Border for Indian Nationals
fig_indiasw_bydemographics <- ggplot(india_demmonthlysw_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "indiasw_monthly_bydemographics.pdf"), 
       plot = fig_indiasw_bydemographics, units = "px", bg="white", 
       width = 2400, height = 800)

#monthly encounters by demographics in india

india_demmonthly_summary <- D0_india %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

fig_india_bydemographics <- ggplot(india_demmonthly_summary,
                                   aes(x = Month_Year, y = Encounter.Count, 
                                       fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "india_monthly_bydemographics.pdf"), 
       plot = fig_india_bydemographics, units = "px", bg="white", 
       width = 2400, height = 800)

#monthly encounters by sectors in SW border

india_monthlysw_bysector <- subset(D0_india, (Land.Border.Region == "Southwest Land Border" 
                                              & Component == "U.S. Border Patrol"
                                              & (Calendar.Year == 2021 | Calendar.Year == 2022 |
                                                Calendar.Year == 2023 | Calendar.Year == 2024)
                                              & (Area.of.Responsibility == "El Centro Sector" |
                                                   Area.of.Responsibility == "San Diego Sector" |
                                                   Area.of.Responsibility == "Tucson Sector" |
                                                   Area.of.Responsibility == "Yuma Sector")
)) %>%
  group_by(Month_Year, Area.of.Responsibility) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Month_Year, Encounter.Count)



fig_indiasw_byaor <- ggplot(india_monthlysw_bysector, 
                            aes(x = Month_Year, y = Encounter.Count,
                                color = Area.of.Responsibility)) +
  geom_point() +
  geom_line(linewidth = 1.2, aes(group = Area.of.Responsibility)) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) +
  facet_wrap(~Area.of.Responsibility, ncol = 2) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  guides(color = FALSE)  # Remove the legend for color

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "indiasw_monthly_byaor.pdf"), 
       plot = fig_indiasw_byaor, units = "px", bg="white", 
       width = 2400, height = 1200)

#monthly encounters by sectors in all borders 

india_monthly_bysector <- subset(D0_india, (Component == "U.S. Border Patrol"
                                              & (Calendar.Year == 2022 |
                                                   Calendar.Year == 2023 | Calendar.Year == 2024)
                                            & (Area.of.Responsibility == "San Diego Sector" |
                                                 Area.of.Responsibility == "Tucson Sector" |
                                                 Area.of.Responsibility == "Yuma Sector" |
                                                 Area.of.Responsibility == "Swanton Sector" )
)) %>%
  group_by(Month_Year, Area.of.Responsibility) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Month_Year, Encounter.Count)



fig_india_byaor <- ggplot(india_monthly_bysector, 
                            aes(x = Month_Year, y = Encounter.Count,
                                color = Area.of.Responsibility)) +
  geom_point() +
  geom_line(linewidth = 1.2, aes(group = Area.of.Responsibility)) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) +
  facet_wrap(~Area.of.Responsibility, ncol = 2) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  guides(color = FALSE)  # Remove the legend for color

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "india_monthly_byaor.pdf"), 
       plot = fig_india_byaor, units = "px", bg="white", 
       width = 2400, height = 1200)



###Burma

D0_burma <- subset(D0, Citizenship == "MYANMAR")

#monthly encounters by border category in burma

burma_byregion_monthly_summary <- D0_burma  %>%
  group_by(Month_Year, Land.Border.Region) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Region for Burmese Nationals
fig_burma_byregion <- ggplot(burma_byregion_monthly_summary, aes(x = Month_Year, y = Encounter.Count, 
                                           fill = Land.Border.Region)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "burma_monthly_byregion.pdf"), 
       plot = fig_burma_byregion, units = "px", bg="white", 
       width = 2400, height = 800)

#monthly encounters by demographics in burma

burma_demmonthly_summary <- D0_burma %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: "Encounter Count by Month and Demographics for Burmese Nationals"
fig_burma_bydemog <- ggplot(burma_demmonthly_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "burma_monthly_bydemography.pdf"), 
       plot = fig_burma_bydemog, units = "px", bg="white", 
       width = 2400, height = 800)

###Philippines

D0_philippines <- subset(D0, Citizenship == "PHILIPPINES")

#monthly encounters by border category in Philippines

philippines_byregion_monthly_summary <- D0_philippines  %>%
  group_by(Month_Year, Land.Border.Region) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Region for Philippines Nationals
fig_philippines_byregion <- ggplot(philippines_byregion_monthly_summary, aes(x = Month_Year, y = Encounter.Count, 
                                           fill = Land.Border.Region)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "philippines_monthly_byregion.pdf"), 
       plot = fig_philippines_byregion, units = "px", bg="white", 
       width = 2400, height = 800)

#monthly encounters in northern border by fields of operations in philippines

philippines_monthlyn_summary <- subset(D0_philippines, Land.Border.Region == "Northern Land Border")  %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Component in Northern Border for Filipino Nationals
fig_philippines_northern <- ggplot(philippines_monthlyn_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "philippines_monthly_northern.pdf"), 
       plot = fig_philippines_northern, units = "px", bg="white", 
       width = 2400, height = 800)

#monthly encounters by demographics in philippines

philippines_demmonthly_summary <- D0_philippines %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Demographics for Philippines Nationals (USBP & OFO combined)
fig_philippines_demmonthly <- ggplot(philippines_demmonthly_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "philippines_monthly_bydemographics.pdf"), 
       plot = fig_philippines_demmonthly, units = "px", bg="white", 
       width = 2400, height = 800)



###Romania

D0_romania <- subset(D0, Citizenship == "ROMANIA")

#monthly encounters by border category romania

romania_byregion_monthly_summary <- D0_romania  %>%
  group_by(Month_Year, Land.Border.Region) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Region for Romanian Nationals
fig_romania_byregion <- ggplot(romania_byregion_monthly_summary, aes(x = Month_Year, y = Encounter.Count, 
                                            fill = Land.Border.Region)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "romania_monthly_byregion.pdf"), 
       plot = fig_romania_byregion, units = "px", bg="white", 
       width = 2400, height = 800)

#monthly encounters by fields of operations romania

romania_monthlyfoo_summary <- D0_romania %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Component in Southwest Border for Romanian Nationals
fig_romania_bycomponents <- ggplot(romania_monthlyfoo_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "romania_monthly_bycomponent.pdf"), 
       plot = fig_romania_bycomponents, units = "px", bg="white", 
       width = 2400, height = 800)

#monthly encounters by demographics - romania

romania_demmonthly_summary <- D0_romania %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Demographics for Romanian Nationals (USBP & OFO combined)
fig_romania_bydemographics <- ggplot(romania_demmonthly_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "romania_monthly_bydemographics.pdf"), 
       plot = fig_romania_bydemographics, units = "px", bg="white", 
       width = 2400, height = 800)

#monthly encounters in southwest border by demographics in romania

romania_demmonthlysw_summary <- subset(D0_romania, Land.Border.Region == "Southwest Land Border")  %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Demographics in Southwest Border for Romanian Nationals
fig_romania_bydemographics_sw <- ggplot(romania_demmonthlysw_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "romania_monthly_southwest_bydemographics.pdf"), 
      plot = fig_romania_bydemographics_sw, units = "px", bg="white", 
      width = 2400, height = 800)

#monthly encounters in southwest border by fields of operations in romania

romania_monthlysw_summary <- subset(D0_romania, Land.Border.Region == "Southwest Land Border")  %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Component in Southwest Border for Romanian Nationals
fig_romania_bycomponents_sw <- ggplot(romania_monthlysw_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "romania_monthly_southwest_bycomponents.pdf"), 
      plot = fig_romania_bycomponents_sw, units = "px", bg="white",
      width = 2400, height = 800)

#monthly encounters in northern border by fields of operations in romania

romania_monthlynt_summary <- subset(D0_romania, Land.Border.Region == "Northern Land Border")  %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Component in Northern Border for Romanian Nationals
fig_romania_bycomponents_nt <- ggplot(romania_monthlynt_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "romania_monthly_north_bycomponents.pdf"), 
       plot = fig_romania_bycomponents_nt, units = "px", bg="white",
       width = 2400, height = 800)

#monthly encounters by all sectors

romania_monthlysw_bysector <- subset(D0_romania, (Land.Border.Region == "Southwest Land Border" 
                                                & Component == "U.S. Border Patrol"
                                                & (Area.of.Responsibility == "Rio Grande Valley Sector" |
                                                     Area.of.Responsibility == "San Diego Sector" |
                                                     Area.of.Responsibility == "Yuma Sector")
)) %>%
  group_by(Month_Year, Area.of.Responsibility) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Month_Year, Encounter.Count)

#title: Total Number of Romanian Encounters per Month by AOR
fig_romania_byaor <- ggplot(romania_monthlysw_bysector, aes(x = Month_Year, y = Encounter.Count,color = Area.of.Responsibility)) +
  geom_point() +
  geom_line(linewidth = 1.2, aes(group = Area.of.Responsibility)) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) +  # Center the title
  facet_wrap(~Area.of.Responsibility, ncol = 1) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  guides(color = FALSE)  # Remove the legend for color

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "romania_monthly_byaor.pdf"), 
      plot = fig_romania_byaor, units = "px", bg="white",
      width = 2400, height = 1200)

###Russia

D0_russia <- subset(D0, Citizenship == "RUSSIA")

#monthly encounters by border category Russia

russia_byregion_monthly_summary <- D0_russia  %>%
  group_by(Month_Year, Land.Border.Region) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#titel: Encounter Count by Month and Region for Russian Nationals
fig_russia_byregion <- ggplot(russia_byregion_monthly_summary, aes(x = Month_Year, y = Encounter.Count, 
                                                 fill = Land.Border.Region)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "russia_monthly_byregion.pdf"), 
      plot = fig_russia_byregion, units = "px", bg="white",
      width = 2400, height = 800)

#monthly encounters in southwest border by demographics in russia

russia_demmonthlysw_summary <- subset(D0_russia, Land.Border.Region == "Southwest Land Border")  %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Demographics in Southwest Border for Russian Nationals
fig_russia_bydemographics_sw <- ggplot(russia_demmonthlysw_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "russia_monthly_bydemographics_sw.pdf"), 
      plot = fig_russia_bydemographics_sw, units = "px", bg="white",
      width = 2400, height = 800)

#monthly encounters in southwest border by fields of operations russia

russia_monthlysw_summary <- subset(D0_russia, Land.Border.Region == "Southwest Land Border")  %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Component in Southwest Borderfor Russian Nationals
fig_russia_bycomponents_sw <- ggplot(russia_monthlysw_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "russia_monthly_bycomponents_sw.pdf"), 
      plot = fig_russia_bycomponents_sw, units = "px", bg="white",
      width = 2400, height = 800)

#monthly encounters by demographics - russia

russia_demmonthly_summary <- D0_russia %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Demographics for Russian Nationals (USBP & OFO combined)
fig_russia_bydemographics <- ggplot(russia_demmonthly_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "russia_monthly_demographics.pdf"), 
      plot = fig_russia_bydemographics, units = "px", bg="white",
      width = 2400, height = 800)

#monthly encounters by sectors
#2023-2024 is chosen because prior to these years the data was negligible

russia_monthlysw_bysector <- subset(D0_russia, (Land.Border.Region == "Southwest Land Border" 
                                              & Component == "U.S. Border Patrol"
                                              & (Calendar.Year == 2022 | 
                                                   Calendar.Year == 2023 | 
                                                   Calendar.Year == 2024)
                                              & (Area.of.Responsibility == "El Paso Sector" | 
                                                   Area.of.Responsibility == "San Diego Sector" | 
                                                   Area.of.Responsibility == "Yuma Sector")
                                              )) %>%
  group_by(Month_Year, Area.of.Responsibility) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Month_Year, Encounter.Count)

#title: Total Number of Russian Encounters per Month by AOR 
russia_monthlysw_byaor <- ggplot(russia_monthlysw_bysector, aes(x = Month_Year, y = Encounter.Count,color = Area.of.Responsibility)) +
  geom_point() +
  geom_line(linewidth = 1.2, aes(group = Area.of.Responsibility)) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) +
  facet_wrap(~Area.of.Responsibility, ncol = 1) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  guides(color = FALSE)  # Remove the legend for color

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "russia_monthly_byaor.pdf"), 
      plot = russia_monthlysw_byaor, units = "px", bg="white",
      width = 2400, height = 1200)

###Turkey

D0_turkey <- subset(D0, Citizenship == "TURKEY")

#monthly encounters by region - turkey

turkey_byregion_monthly_summary <- D0_turkey  %>%
  group_by(Month_Year, Land.Border.Region) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title - Encounters per Month by Region for Turkish Nationals
turkey_monthly_byregion <- ggplot(turkey_byregion_monthly_summary, aes(x = Month_Year, y = Encounter.Count, 
                                             fill = Land.Border.Region)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "turkey_monthly_byregion.png"), 
      plot = turkey_monthly_byregion, units = "px", bg="white",
      width = 3200, height = 1600)

#monthly encounters by fields of operations - turkey

turkey_monthlyfoo_summary <- D0_turkey %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title - Encounter Count by Month and Component for Turkish Nationals
turkey_monthly_bycomponent <- ggplot(turkey_monthlyfoo_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "turkey_monthly_bycomponent.png"), 
      plot = turkey_monthly_bycomponent, units = "px", bg="white",
      width = 3200, height = 1600)

#monthly encounters by demographics - turkey

turkey_demmonthly_summary <- D0_turkey %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title - Encounter Count by Month and Demographics for Romanian Nationals (USBP & OFO combined)
turkey_monthly_bydemographics <- ggplot(turkey_demmonthly_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "turkey_monthly_bydemographics.png"), 
      plot = turkey_monthly_bydemographics, units = "px", bg="white",
      width = 3200, height = 1600)

#monthly encounters in southwest border by demographics in turkey

turkey_demmonthlysw_summary <- subset(D0_turkey, Land.Border.Region == "Southwest Land Border")  %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title - Encounter Count by Month and Demographics in Southwest Border for Turkish Nationals
turkey_monthly_bydemographics_sw <- ggplot(turkey_demmonthlysw_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "turkey_monthly_bydemographics_sw.png"), 
      plot = turkey_monthly_bydemographics_sw, units = "px", bg="white",
      width = 3200, height = 1600)

#monthly encounters in southwest border by fields of operations in turkey

turkey_monthlysw_summary <- subset(D0_turkey, Land.Border.Region == "Southwest Land Border")  %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title - Encounter Count by Month and Component in Southwest Border for Turkish Nationals
turkey_monthly_bycomponents_sw <- ggplot(turkey_monthlysw_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "turkey_monthly_bycomponents_sw.png"), 
      plot = turkey_monthly_bycomponents_sw, units = "px", bg="white",
      width = 3200, height = 1600)

#monthly encounters by sectors - turkey

turkey_monthlysw_bysector <- subset(D0_turkey, (Land.Border.Region == "Southwest Land Border" 
                                                  & Component == "U.S. Border Patrol"
                                                  & (Calendar.Year == 2021 | Calendar.Year == 2022 | 
                                                     Calendar.Year == 2023 | Calendar.Year == 2024)
                                                  & (Area.of.Responsibility == "El Paso Sector" |
                                                     Area.of.Responsibility == "San Diego Sector")
                                                     
)) %>%
  group_by(Month_Year, Area.of.Responsibility) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Month_Year, Encounter.Count)

#title - Total Number of Turkish Encounters per Month by AOR
turkey_monthly_byaor <- ggplot(turkey_monthlysw_bysector, aes(x = Month_Year, y = Encounter.Count,color = Area.of.Responsibility)) +
  geom_point() +
  geom_line(linewidth = 1.2, aes(group = Area.of.Responsibility)) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for better readability
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  facet_wrap(~Area.of.Responsibility, ncol = 1) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  guides(color = FALSE)  # Remove the legend for color

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "turkey_monthly_byaor.png"), 
      plot = turkey_monthly_byaor, units = "px", bg="white",
      width = 3200, height = 1600)

###Ukraine

D0_ukraine <- subset(D0, Citizenship == "UKRAINE")


ukraine_byregion_monthly_summary <- D0_ukraine  %>%
  group_by(Month_Year, Land.Border.Region) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Region for Ukranian Nationals
ukraine_monthly_byregion <- ggplot(ukraine_byregion_monthly_summary, aes(x = Month_Year, y = Encounter.Count, 
                                                 fill = Land.Border.Region)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "ukraine_monthly_byregion.pdf"), 
      plot = ukraine_monthly_byregion, units = "px", bg="white",
      width = 2400, height = 800)

#monthly encounters by fields of operations -ukraine

ukraine_monthlyfoo_summary <- D0_ukraine %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Component for Ukranian Nationals
ukraine_monthly_bycomponents <- ggplot(ukraine_monthlyfoo_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "ukraine_monthly_bycomponents.pdf"), 
      plot = ukraine_monthly_bycomponents, units = "px", bg="white",
      width = 2400, height = 800)

#monthly encounters by demographics - ukraine

ukraine_demmonthly_summary <- D0_ukraine %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#title: Encounter Count by Month and Demographics for Ukranian Nationals
ukraine_monthly_bydemographics <- ggplot(ukraine_demmonthly_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                          figures_directory), "ukraine_monthly_bydemographics.pdf"), 
      plot = ukraine_monthly_bydemographics, units = "px", bg="white",
      width = 2400, height = 800)

#monthly encounters by fields of operations in countries outside of Americas
#excluding OTHER category where it also includes the following countries
#in South-Central America:Costa Rica, Panama, Guyana, Suriname, French Guiana, 
#Bolivia, Paraguay, Uruguay, Argentina, Chile, Jamaica, Dominican Republic, and
#Puerto Rico

###combined non-american countries

D0_others <- subset(D0, (Citizenship == "CHINA" 
                   | Citizenship == "INDIA" | Citizenship == "MYANMAR"
                   | Citizenship == "PHILIPPINES"
                   | Citizenship == "ROMANIA" | Citizenship == "RUSSIA"
                   | Citizenship == "TURKEY" | Citizenship == "UKRAINE"))

others_monthly_summary <- D0_others  %>%
  group_by(Month_Year, Citizenship) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

others_yearly_summary <- D0_others  %>%
  group_by(Calendar.Year, Citizenship) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

others_general_summary <- D0_others %>%
  group_by(Citizenship) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Encounter.Count)

others_general_summary$Citizenship <- factor(others_general_summary$Citizenship, 
                                             levels = others_general_summary$Citizenship)

others_general_summary <- others_general_summary %>%
  mutate(Percentage = Encounter.Count / sum(Encounter.Count) * 100)

#treemap

#to be titled on the document as "Number of Encounters Nationwide from Countries Outside of the Americas Between 2020-2024"
library(treemapify)
fig_others_treemap <- ggplot(others_general_summary, aes(area = Encounter.Count, fill = Encounter.Count, label = Citizenship)) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  geom_treemap(color = "black", size = 2) +
  geom_treemap_text(color = "white", 
                    size = 24, 
                    place = "centre",
                    reflow = TRUE,
                    grow = FALSE) +
  labs(fill = "Number of 
Encounters") +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 5))
        )

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "nonamerican_countries_treemap_2020_2024.png"), 
       plot = fig_others_treemap, units = "px", bg="white",
       width = 2400, height = 1600)

#lollipop chart

# title: "Total Number of Encounters Nationwide from Countries Outside of the Americas Between 2020-2024"
fig_others_lollipop <- ggplot(others_general_summary, aes(y = Citizenship, x = Encounter.Count)) +
  geom_segment(aes(y = Citizenship, yend = Citizenship, x = 0, xend = Encounter.Count),
               size = 1.2) +
  geom_point(color = "blue", size = 4) + 
  geom_text(aes(label = format(Encounter.Count, big.mark = ",")), vjust = -0.5, hjust =0.8, size = 5) +
  theme_light() +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", margin = margin(r = 1)),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  ) +
  xlab("") +
  ylab("")

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "nonamerican_countries_lollipop_2020_2024.svg"), 
       plot = fig_others_lollipop, units = "px", bg="white",
       width = 2400, height = 1200)

ggplot(others_yearly_summary, aes(x = Encounter.Count, y = Citizenship, color = Calendar.Year, group = Citizenship)) +
  geom_line(size = 2) +    # Line for each country
  geom_point(size = 3) +     # Points at each year
  scale_color_brewer(palette = "Dark2") +  # Use a color palette for distinct lines
  theme_minimal() +
  labs(title = "Encounter Counts per Year by Country",
       x = "",
       y = "",
       color = "Country") +
  theme(legend.position = "right")

#monthly encounters by region - the countries outside of the Americas 

others_byregion_monthly_summary <- D0_others  %>%
  group_by(Month_Year, Land.Border.Region) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#to be titled on the document as "Encounter Distribution by Month and Region from the Countries Outside of the Americas"
fig_others_byregion <- ggplot(others_byregion_monthly_summary, aes(x = Month_Year, y = Encounter.Count, 
                                            fill = Land.Border.Region)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "nonamerican_countries_byregion.png"), 
       plot = fig_others_byregion, units = "px", bg="white",
       width = 3200, height = 1600)

#monthly encounters by cbp components - the countries outside of the Americas

others_monthlyfoo_summary <- D0_others %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#to be titled as "Encounter Count by Month and Component from the Countries Outside of the Americas"
fig_others_bycomponents <- ggplot(others_monthlyfoo_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "nonamerican_countries_bycomponents.png"), 
       plot = fig_others_bycomponents, units = "px", bg="white",
       width = 3200, height = 1600)

#monthly encounters by demographics - the countries outside of the Americas 

others_demmonthly_summary <- D0_others %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#to be titled as "Encounter Count by Month and Demographics from the Countries Outside of the Americas"
fig_others_bydemographics <- ggplot(others_demmonthly_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "nonamerican_countries_bydemographics.png"), 
       plot = fig_others_bydemographics, units = "px", bg="white",
       width = 3200, height = 1600)

#monthly encounters in southwest border by demographics - the countries outside of the Americas 

others_demmonthlysw_summary <- subset(D0_others, Land.Border.Region == "Southwest Land Border")  %>%
  group_by(Month_Year, Demographic) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

#to be titled as "Encounter Count by Month and Demographics in Southwest Border from the Countries Outside of the Americas"
fig_others_bydemographics_sw <- ggplot(others_demmonthlysw_summary, aes(x = Month_Year, y = Encounter.Count, fill = Demographic)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "nonamerican_countries_bydemographics_sw.png"), 
       plot = fig_others_bydemographics_sw, units = "px", bg="white",
       width = 3200, height = 1600)

#monthly encounters in southwest border by fields of operations - the countries outside of the Americas 

others_monthlysw_summary <- subset(D0_others, Land.Border.Region == "Southwest Land Border")  %>%
  group_by(Month_Year, Component) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

fig_others_bycomponents_sw <- ggplot(others_monthlysw_summary, aes(x = Month_Year, y = Encounter.Count, fill = Component)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Encounter Count by Month and Component in Southwest Border from the Countries Outside of the Americas",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "nonamerican_countries_bycomponents_sw.png"), 
       plot = fig_others_bycomponents_sw, units = "px", bg="white",
       width = 3200, height = 1600)

#monthly encounters by sectors - the countries outside of the Americas 

others_monthlysw_bysector <- subset(D0_others, (Land.Border.Region == "Southwest Land Border" 
                                                & Component == "U.S. Border Patrol"
                                                & (Calendar.Year == 2022 | 
                                                   Calendar.Year == 2023 | Calendar.Year == 2024)
                                                & (Area.of.Responsibility == "El Centro Sector" |
                                                     Area.of.Responsibility == "El Paso Sector" |
                                                     Area.of.Responsibility == "Rio Grande Valley Sector" |
                                                     Area.of.Responsibility == "San Diego Sector" |
                                                     Area.of.Responsibility == "Tucson Sector" |
                                                     Area.of.Responsibility == "Yuma Sector")
)) %>%
  group_by(Month_Year, Area.of.Responsibility) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Month_Year, Encounter.Count)

fig_others_byaor_sw <- ggplot(others_monthlysw_bysector, aes(x = Month_Year, y = Encounter.Count,color = Area.of.Responsibility)) +
  geom_point() +
  geom_line(linewidth = 1.2, aes(group = Area.of.Responsibility)) +
  labs(title = "Total Number of Encounters from the Countries Outside of the Americas per Month by AOR",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for better readability
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  facet_wrap(~Area.of.Responsibility, ncol = 2) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  guides(color = FALSE)  # Remove the legend for color

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "nonamerican_countries_byaor_sw.png"), 
       plot = fig_others_byaor_sw, units = "px", bg="white",
       width = 3200, height = 1600)

###continents comparison for 2020-2024

others_monthly_summary <- D0_others  %>%
  group_by(Month_Year, Citizenship) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

D0_americavsworld <- D0 %>%
  group_by(Calendar.Year, Citizenship) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Calendar.Year,Encounter.Count)
D0_americavsworld <- D0_americavsworld[D0_americavsworld$Citizenship != "OTHER",] #exclude the OTHER category
D0_americavsworld <- transform(
  D0_americavsworld, is_in_america = ifelse((Citizenship == "CHINA" 
                                            | Citizenship == "INDIA" | Citizenship == "MYANMAR"
                                            | Citizenship == "PHILIPPINES"
                                            | Citizenship == "ROMANIA" | Citizenship == "RUSSIA"
                                            | Citizenship == "TURKEY" | Citizenship == "UKRAINE"), 
                                            "FALSE",
                                            "TRUE")
)

americavsworld_yearly <- D0_americavsworld %>%
  group_by(Calendar.Year, is_in_america) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

americavsworld_yearly <- americavsworld_yearly %>%
  group_by(Calendar.Year) %>%
  mutate(Proportion = Encounter.Count / sum(Encounter.Count) * 100)

americavsworld_yearly <- americavsworld_yearly %>%
  mutate(Category = "Nationwide")

#to be titled as "Americas versus Non-Americas Encounters Nationwide"
fig_othersvsworld_nationwide <- ggplot(data = americavsworld_yearly, aes(x=" ", y = Proportion, 
                                         group = is_in_america, 
                                         fill=is_in_america)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  facet_grid(.~ Calendar.Year) + theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10), size = 12),
        legend.box.margin = margin(-30, 0, 0, 0),
        legend.position = "none") +
  labs(title = "(a) Americas versus Non-Americas Encounters - Nationwide")
  #fill = "Is the Origin in
#America Continent?     ")

#ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
#                           figures_directory), "nonamerican_vs_american_nationwide.png"), 
#       plot = fig_othersvsworld_nationwide, units = "px", bg="white",
#       width = 3200, height = 800)

#america_vs_world in southwest border - USBP & OFO combined
D0_americavsworldsw <- subset(D0, (Land.Border.Region == "Southwest Land Border"))  %>%
  group_by(Calendar.Year, Citizenship) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Calendar.Year,Encounter.Count)
D0_americavsworldsw <- D0_americavsworldsw[D0_americavsworldsw$Citizenship != "OTHER",] #exclude the OTHER category
D0_americavsworldsw <- transform(
  D0_americavsworldsw, is_in_america = ifelse((Citizenship == "CHINA" 
                                               | Citizenship == "INDIA" | Citizenship == "MYANMAR"
                                               | Citizenship == "PHILIPPINES"
                                               | Citizenship == "ROMANIA" | Citizenship == "RUSSIA"
                                               | Citizenship == "TURKEY" | Citizenship == "UKRAINE"), 
                                              "FALSE",
                                              "TRUE")
)
americavsworldsw_yearly <- D0_americavsworldsw %>%
  group_by(Calendar.Year, is_in_america) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

americavsworldsw_yearly <- americavsworldsw_yearly %>%
  group_by(Calendar.Year) %>%
  mutate(Proportion = Encounter.Count / sum(Encounter.Count) * 100)

americavsworldsw_yearly <- americavsworldsw_yearly %>%
  mutate(Category = "Southwest")

#to be titled as "Americas versus Non-Americas Encounters in Southwest Border - OFO & USBP Combined"
fig_othersvsworld_southwest <- ggplot(data = americavsworldsw_yearly, aes(x=" ", y = Proportion, 
                                           group = is_in_america, 
                                           fill=is_in_america)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  facet_grid(.~ Calendar.Year) + theme_void() +
  labs(title = "(b) Americas versus Non-Americas Encounters in Southwest Border - OFO & USBP Combined") + 
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10), size = 12),
        legend.box.margin = margin(-30, 0, 0, 0),
        legend.position = "none") 

#ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
#                           figures_directory), "nonamerican_vs_american_southwest.png"), 
#       plot = fig_othersvsworld_southwest, units = "px", bg="white",
#       width = 3200, height = 800)

#america_vs_world in southwest border - USBP only (Calendar Year)
D0_americavsworld_swusbp <- subset(D0, (Land.Border.Region == "Southwest Land Border"
                                   & Component == "U.S. Border Patrol"))  %>%
  group_by(Calendar.Year, Citizenship) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Calendar.Year,Encounter.Count)
D0_americavsworld_swusbp <- D0_americavsworld_swusbp[D0_americavsworld_swusbp$Citizenship != "OTHER",] #exclude the OTHER category
D0_americavsworld_swusbp <- transform(
  D0_americavsworld_swusbp, is_in_america = ifelse((Citizenship == "CHINA" 
                                               | Citizenship == "INDIA" | Citizenship == "MYANMAR"
                                               | Citizenship == "PHILIPPINES"
                                               | Citizenship == "ROMANIA" | Citizenship == "RUSSIA"
                                               | Citizenship == "TURKEY" | Citizenship == "UKRAINE"), 
                                              "FALSE",
                                              "TRUE")
)

americavsworldswusbp_yearly <- D0_americavsworld_swusbp %>%
  group_by(Calendar.Year, is_in_america) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE))

americavsworldswusbp_yearly <- americavsworldswusbp_yearly %>%
  group_by(Calendar.Year) %>%
  mutate(Proportion = Encounter.Count / sum(Encounter.Count) * 100)

americavsworldswusbp_yearly <- americavsworldswusbp_yearly %>%
  mutate(Category = "USBP")

fig_othersvsworld_southwestusbp <- ggplot(data = americavsworldswusbp_yearly, aes(x=" ", y = Proportion, 
                                           group = is_in_america, 
                                           fill=is_in_america)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  facet_grid(.~ Calendar.Year) + theme_void() +
  labs(title = "(c) Americas versus Non-Americas Encounters in Southwest Border - USBP Only",
       fill = "Is the Origin in America Continent?") +
  theme(legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.spacing.x = unit(0, "mm"),
      legend.spacing.y = unit(0, "mm"),
      plot.title = element_text(hjust = 0.5, margin = margin(b = 10), size = 12))

#ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
#                           figures_directory), "nonamerican_vs_american_southwest_usbp.png"), 
#       plot = fig_othersvsworld_southwestusbp, units = "px", bg="white",
#       width = 3200, height = 800)

library(gridExtra)
fig_othersvsworld_all <- grid.arrange(fig_othersvsworld_nationwide, 
                                      fig_othersvsworld_southwest,
                                      fig_othersvsworld_southwestusbp, nrow =3)

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "nonamerican_vs_american_all.pdf"), 
       plot = fig_othersvsworld_all, units = "px", bg="white",
       width = 2200, height = 2200)

#combine the three dataframes to generate a bar chart

americavsworld_yearly_combined <- rbind(americavsworld_yearly, americavsworldsw_yearly, americavsworldswusbp_yearly)

americavsworld_yearly_combined <- americavsworld_yearly_combined %>%
  arrange(Calendar.Year,Category,is_in_america)

#filter out only the countries outside of the america continents
americavsworld_yearly_combined_worldonly <- subset(americavsworld_yearly_combined,is_in_america == "FALSE")

fig_americavsworld_yearly_combined_proportion <- ggplot(americavsworld_yearly_combined_worldonly, 
                                                       aes(x = Category, y = Proportion, 
                                                           fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Calendar.Year, nrow = 1, strip.position = "bottom") +
  scale_y_continuous(
    limits = c(0, 16),  # Keep your original limits
    breaks = seq(0, 16, by = 2)  # Adjust the breaks here for more granularity
  ) +
  labs(
    title = "",
    x = "",
    y = "Proportion (%)",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing.x = unit(0, "mm"),
    legend.spacing.y = unit(0, "mm"),
    legend.box.margin = margin(-30, 0, 0, 0)
  )

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "nonamerican_countries_ratio_overtheyears.svg"), 
       plot = fig_americavsworld_yearly_combined_proportion, units = "px", bg="white",
       width = 2400, height = 1200)

fig_americavsworld_yearly_combined_encounters <- ggplot(americavsworld_yearly_combined_worldonly, 
                                                       aes(x = Category, y = Encounter.Count, 
                                                           fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Calendar.Year, nrow = 1, strip.position = "bottom") +
  #scale_y_continuous(
  #  limits = c(0, 20),  # Keep your original limits
  #  breaks = seq(0, 20, by = 2)  # Adjust the breaks here for more granularity
  #) +
  labs(
    title = "",
    x = "",
    y = "Total Encounters",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 24),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing.x = unit(0, "mm"),
    legend.spacing.y = unit(0, "mm"),
    legend.box.margin = margin(-30, 0, 0, 0)
  )

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "nonamerican_countries_total_overtheyears.svg"), 
       plot = fig_americavsworld_yearly_combined_encounters, units = "px", bg="white",
       width = 2400, height = 1800)


 ###continents comparison for FY 2007-2019 only in southwest usbp

#2007-2019 data
#CAUTION: only available date variable is FISCAL YEAR
df <- read.csv("2007-2019.csv")
colnames(df) <- gsub("\\.", " ", colnames(df))


df <- df %>%
  mutate(across(starts_with("Big Bend Sector"):starts_with("Yuma Sector"), ~ gsub(",", "", .)))

df <- df %>%
  mutate(across(starts_with("Big Bend Sector"):starts_with("Yuma Sector"), as.numeric))

# Reshape the dataset to long format
df_long <- df %>%
  pivot_longer(
    cols = c("Big Bend Sector", "Del Rio Sector", "El Centro Sector", "El Paso Sector",
             "Laredo Sector", "Rio Grande Valley Sector", "San Diego Sector", 
             "Tucson Sector", "Yuma Sector"),
    names_to = "Sector",
    values_to = "Encounters"
  )

df_long <- subset(df_long, df_long$Encounters != "")

colnames(df_long) <- gsub(" ", "\\.",colnames(df_long))

write.csv(df_long, '2007-2019_encounters_bysector.csv', row.names = FALSE)

D0_americavsworld_old <- df_long %>%
  group_by(Fiscal.Year, Citizenship) %>%
  summarise(Encounters = sum(Encounters, na.rm = TRUE)) %>%
  arrange(Fiscal.Year,Encounters)
D0_americavsworld_old <- D0_americavsworld_old[D0_americavsworld_old$Citizenship != "STATELESS",] #exclude the STATELESS category
D0_americavsworld_old <- D0_americavsworld_old[D0_americavsworld_old$Citizenship != "UNKNOWN",] #exclude the STATELESS category
D0_americavsworld_old <- transform(
  D0_americavsworld_old, is_in_america = ifelse((Citizenship == "ANTIGUA-BARBUDA" 
                                             | Citizenship == "ARGENTINA" | Citizenship == "ARUBA"
                                             | Citizenship == "BAHAMAS" | Citizenship == "BARBADOS"
                                             | Citizenship == "BELIZE" | Citizenship == "BERMUDA"
                                             | Citizenship == "BOLIVIA" | Citizenship == "BRAZIL"
                                             | Citizenship == "BRITISH VIRGIN ISLANDS" | Citizenship == "CANADA"
                                             | Citizenship == "CAYMAN ISLANDS" | Citizenship == "CHILE"
                                             | Citizenship == "COLOMBIA" | Citizenship == "COSTA RICA"
                                             | Citizenship == "CUBA" | Citizenship == "DOMINICA"
                                             | Citizenship == "DOMINICAN REPUBLIC" | Citizenship == "ECUADOR"
                                             | Citizenship == "EL SALVADOR" | Citizenship == "GRENADA"
                                             | Citizenship == "GUADELOUPE" | Citizenship == "GUATEMALA"
                                             | Citizenship == "GUYANA" | Citizenship == "HAITI"
                                             | Citizenship == "HONDURAS" | Citizenship == "JAMAICA"
                                             | Citizenship == "GUADELOUPE" | Citizenship == "GUATEMALA"
                                             | Citizenship == "MARTINIQUE" | Citizenship == "MEXICO"
                                             | Citizenship == "MONTSERRAT" | Citizenship == "NICARAGUA"
                                             | Citizenship == "PANAMA" | Citizenship == "PARAGUAY"
                                             | Citizenship == "PERU" | Citizenship == "PUERTO RICO"
                                             | Citizenship == "ST. KITTS-NEVIS" | Citizenship == "ST. LUCIA"
                                             | Citizenship == "ST. VINCENT-GRENADINES" | Citizenship == "SURINAME"
                                             | Citizenship == "TRINIDAD AND TOBAGO" | Citizenship == "TURKS AND CAICOS ISLANDS"
                                             | Citizenship == "URUGUAY" | Citizenship == "VENEZUELA"), 
                                            "TRUE",
                                            "FALSE")
)


americavsworld_old_yearly <- D0_americavsworld_old %>%
  group_by(Fiscal.Year, is_in_america) %>%
  summarise(Encounters = sum(Encounters, na.rm = TRUE))

americavsworld_old_yearly <- americavsworld_old_yearly %>%
  group_by(Fiscal.Year) %>%
  mutate(Proportion = Encounters / sum(Encounters) * 100)

ggplot(data = americavsworld_old_yearly, aes(x=" ", y = Proportion, 
                                         group = is_in_america, 
                                         fill=is_in_america)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  facet_wrap(.~ Fiscal.Year, nrow = 3) + theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) +
  labs(title = "Americas versus Non-Americas Encounters Nationwide", 
       fill = "Is the Origin in
America Continent?     ")

##to compare 2020 and late data with the early, remove the countries in OTHER 
##category and keep the following countries
americavsworld_old_adjusted <- subset(D0_americavsworld_old, 
                                  Citizenship == "BRAZIL" |
                                    Citizenship == "CANADA" |
                                    Citizenship == "CHINA" |
                                    Citizenship == "COLOMBIA" |
                                    Citizenship == "CUBA" |
                                    Citizenship == "ECUADOR" |
                                    Citizenship == "EL SALVADOR" |
                                    Citizenship == "GUATEMALA" |
                                    Citizenship == "HAITI" |
                                    Citizenship == "HONDURAS" |
                                    Citizenship == "INDIA" |
                                    Citizenship == "MEXICO" |
                                    Citizenship == "MYANMAR" |
                                    Citizenship == "NICARAGUA" |
                                    Citizenship == "PERU" |
                                    Citizenship == "PHILIPPINES" |
                                    Citizenship == "ROMANIA" |
                                    Citizenship == "RUSSSIA" |
                                    Citizenship == "TURKEY"  |
                                    Citizenship == "UKRAINE" |
                                    Citizenship == "VENEZUELA"
                                    )


americavsworld_old_adjusted <- transform(
  americavsworld_old_adjusted, is_in_america = ifelse((Citizenship == "CHINA" 
                                               | Citizenship == "INDIA" | Citizenship == "MYANMAR"
                                               | Citizenship == "PHILIPPINES"
                                               | Citizenship == "ROMANIA" | Citizenship == "RUSSIA"
                                               | Citizenship == "TURKEY" | Citizenship == "UKRAINE"), 
                                              "FALSE",
                                              "TRUE"))

americavsworld_old_adjusted_yearly <- americavsworld_old_adjusted %>%
  group_by(Fiscal.Year, is_in_america) %>%
  summarise(Encounters = sum(Encounters, na.rm = TRUE))

americavsworld_old_adjusted_yearly <- americavsworld_old_adjusted_yearly %>%
  group_by(Fiscal.Year) %>%
  mutate(Proportion = Encounters / sum(Encounters) * 100)

ggplot(data = americavsworld_old_adjusted_yearly, aes(x=" ", y = Proportion, 
                                           group = is_in_america, 
                                           fill=is_in_america)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  facet_wrap(.~ Fiscal.Year, nrow = 3) + theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) +
  labs(title = "Americas versus Non-Americas Encounters in Southwest Border",
       fill = "Is the Origin in 
America Continent?     ")

##combine 2007-2018 with 2020-2024(may) data

americavsworld_old_adjusted_yearly2 <- americavsworld_old_adjusted_yearly

colnames(americavsworld_old_adjusted_yearly2)[which(names(
  americavsworld_old_adjusted_yearly2) == "Encounters")] <- "Encounter.Count"

americavsworld_fiscalyear_2007_2024 <- rbind(americavsworld_old_adjusted_yearly2,
                                             americavsworldswusbp_yearly)

ggplot(data = americavsworld_fiscalyear_2007_2024, aes(x=" ", y = Proportion, 
                                                      group = is_in_america, 
                                                      fill=is_in_america)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  facet_wrap(.~ Fiscal.Year, nrow = 3) + theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) +
  #labs(title = "Americas versus Non-Americas Encounters in Southwest Border - USBP",
  #     fill = "Is the Origin in 
#America Continent?     ")
  labs(title = " ",
            fill = "Is the Origin in 
       America Continent?     ")

### India, China, Turkey, Romania, Russia on the same graph
D0_ictrr <- subset(D0, Citizenship == "CHINA" | 
                   Citizenship == "INDIA" | Citizenship == "TURKEY" |
                   Citizenship == "ROMANIA" | Citizenship == "RUSSIA")

ictrr_monthly_bysector <- subset(D0_ictrr, (Component == "U.S. Border Patrol"
                                          & (Area.of.Responsibility == "Swanton Sector" | 
                                             Area.of.Responsibility == "El Paso Sector" |
                                             Area.of.Responsibility == "Rio Grande Valley Sector" | 
                                             Area.of.Responsibility == "San Diego Sector" | 
                                             Area.of.Responsibility == "Tucson Sector" |
                                             Area.of.Responsibility == "Yuma Sector")
                                          & (Calendar.Year == "2021" | Calendar.Year == "2022" |
                                             Calendar.Year == "2023" | Calendar.Year == "2024")
)) %>%
  group_by(Quarter_Year, Citizenship, Area.of.Responsibility) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  arrange(Quarter_Year, Area.of.Responsibility, Citizenship)

ictrr_monthly_bysector$Area.of.Responsibility <- gsub(" Sector", "", ictrr_monthly_bysector$Area.of.Responsibility)

# title: Total Number of Encounters per Month by AOR
fig_inchrorutu_byaor <- ggplot(ictrr_monthly_bysector, aes(x = Quarter_Year, y = Encounter.Count,color = Citizenship)) +
  geom_point() +
  geom_line(linewidth = 1.2, aes(group = Citizenship)) +
  labs(title = "",
       fill = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # Rotate x-axis labels for better readability
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.box.margin = margin(-25, 0, 0, 0)) +
  facet_wrap(~Area.of.Responsibility, ncol = 3) +
  scale_y_continuous(breaks = pretty_breaks(n = 5))

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "inchrorutu_byaor.pdf"), 
       plot = fig_inchrorutu_byaor, units = "px", bg="white",
       width = 2400, height = 1800)

# pie charts to compare mexicon-nonmexicon through US SW border

mexicovstheworld_fiscalyear_2007_2019 <- df_long %>%
  group_by(Fiscal.Year, Citizenship) %>%
  summarise(Encounters = sum(Encounters, na.rm = TRUE)) %>%
  arrange(Fiscal.Year,Encounters)
mexicovstheworld_fiscalyear_2020_2024 <- D0 %>%
  group_by(Fiscal.Year, Citizenship) %>%
  summarise(Encounter.Count = sum(Encounter.Count, na.rm = TRUE)) %>%
  rename(Encounters = Encounter.Count) %>%
  arrange(Fiscal.Year, Encounters)

mexicovstheworld_fiscalyear_2007_2019 <- transform(
  mexicovstheworld_fiscalyear_2007_2019, is_in_mexico = ifelse(Citizenship == "MEXICO", 
                                                      "TRUE",
                                                      "FALSE"))
mexicovstheworld_fiscalyear_2020_2024 <- transform(
  mexicovstheworld_fiscalyear_2020_2024, is_in_mexico = ifelse(Citizenship == "MEXICO", 
                                                               "TRUE",
                                                               "FALSE"))

mexicovsworld_old_yearly_2007_2019 <- mexicovstheworld_fiscalyear_2007_2019 %>%
  group_by(Fiscal.Year, is_in_mexico) %>%
  summarise(Encounters = sum(Encounters, na.rm = TRUE))

mexicovsworld_old_yearly_2020_2024 <- mexicovstheworld_fiscalyear_2020_2024 %>%
  group_by(Fiscal.Year, is_in_mexico) %>%
  summarise(Encounters = sum(Encounters, na.rm = TRUE))

mexicovsworld_fiscalyear_2007_2024 <- rbind(mexicovsworld_old_yearly_2007_2019,
                                            mexicovsworld_old_yearly_2020_2024)
mexicovsworld_fiscalyear_2007_2024 <- mexicovsworld_fiscalyear_2007_2024 %>%
  mutate(Proportion = Encounters / sum(Encounters) * 100)

#plot - mexico vs the world
ggplot(data = mexicovsworld_fiscalyear_2007_2024, aes(x=" ", y = Proportion, 
                                                       group = is_in_mexico, 
                                                       fill=is_in_mexico)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  facet_wrap(.~ Fiscal.Year, nrow = 3) + theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) +
  labs(title = "Mexican versus Non-Mexican Encounters in Southwest Border - USBP",
       fill = "Is the Individual is 
Mexican National?     ")

#plot - mexico vs the world2

fig_mexicovstheworld <- ggplot(data = mexicovsworld_fiscalyear_2007_2024, aes(x=Fiscal.Year, y = Encounters, 
                                                      color = as.factor(is_in_mexico))) +
  geom_line(size=1) +
  geom_point(size=2) +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10))) +
  theme_minimal(base_size = 12) +
  scale_y_continuous(labels = scales::label_comma(),breaks = pretty_breaks(n = 5)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  theme(legend.position = "bottom",
        legend.margin = margin(-20, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm")) +
  labs(title = " ",
       x = " ",
       y = " ",
       color = "Mexican National"
       )

ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "mexican-nonmexican.pdf"), 
       plot = fig_mexicovstheworld, units = "px", bg="white",
       width = 2400, height = 800)

#####

#for yearly 1925-2023 encounters data 
###
yearly_encounters <-read.csv("yearly_encounters_1925_2024.csv")

fig_usbp_trends_1925_2024 <- ggplot(yearly_encounters, aes(x = Year, y = Apprehensions)) +
  geom_line(size = 1) +  # Draw lines for encounters over time
  geom_point(size = 1) + # Add points for better visibility
  theme_minimal() +
  scale_x_continuous(breaks = c(seq(1925, 2020, by = 5), 2024)) +
  scale_y_continuous(labels = scales::label_comma(),breaks = pretty_breaks(n = 5)) +
  labs(title = "",
       fill = "",
       x = "",
       y = "") +
  theme(
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 12),
    #panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave(file.path(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), 
                           figures_directory), "usbp_1925_2024.pdf"), 
       plot = fig_usbp_trends_1925_2024, units = "px", bg="white",
       width = 2400, height = 1200)

