library(dplyr)
library(openair)
library(lubridate)
library(purrr)

# data from Frank, my electricity supplier
frank_csvs <- list.files("frank_data/", pattern = ".csv", full.names = TRUE)

# function to read csv and make some changes
rd_csv <- function(x){read.csv(x,sep=";")}

frank <- purrr::map_dfr(frank_csvs, rd_csv) |> 
  transmute(Date = Datum,
         From = Van,
         consump_kWh = as.numeric(gsub(",", ".", Verbruik.in.kWh)),
         market_price = as.numeric(gsub(",", ".", Marktprijs..excl.btw..in.EUR.kWh)),
         cost_exc = as.numeric(gsub(",", ".", Kosten.in.EUR..marktprijs...verbruik...excl.btw.)),
         cost_exc_pp = as.numeric(gsub(",", ".", Kosten.in.EUR...Marktprijs...inkoopvergoeding....verbruik...excl.btw.)),
         cost_inc = as.numeric(gsub(",", ".", Kosten.in.EUR...marktprijs...inkoopvergoeding....verbruik...incl.btw.))) |> 
  mutate(date = ymd_hm(paste(Date,From))) |> 
  select(-Date,-From) |> 
  mutate(fixed_rate_cost = consump_kWh*0.22,
         cont_inc_me = consump_kWh*(market_price*1.21))

sum(frank$fixed_rate_cost)

sum(frank$cost_inc)

# names(frank) <- c("EAN","Date","From","To","VAT percentage","Consumption: Initial position","Consumption: Final position",
#                "Consumption in kWh","Market price (excl. VAT) in EUR/kWh","Consumption: Purchase price (excl. VAT) in EUR/kWh",
#                "Costs in EUR (market price * consumption) (excl. VAT)","Costs in EUR ((Market price + purchase price) * consumption) (excl. VAT)",
#                "Costs in EUR ((market price + purchase price) * consumption) (incl. VAT)","Return: Initial position","Return: Final position",
#                "Return in kWh;Return: Purchase price (excl. VAT) in EUR/kWh","Return: Revenue in EUR (market price * return) (excl. VAT)",
#                "Return: Revenue in EUR ((market price – purchase price) * return) (excl. VAT)","Return: Revenue in EUR ((market price – purchase price) * return) (incl. VAT))")

typical_frank <- frank |> 
  filter(date < "2025-05-11") |> 
  mutate(day = lubridate::wday(date),
         hour = hour(date)) |> 
  filter(day == 1) |> 
  group_by(hour) |> 
  summarise(typical_cost = mean(cost_inc, na.rm = TRUE))

frank_daily <- timeAverage(frank, "day", statistic = "sum")


nrg_csvs <- list.files("energy_monitor", pattern = ".csv", full.names = TRUE)


nrg <- map_dfr(nrg_csvs, read.csv) |> 
  mutate(realtime_1 = c(Import.T1.kWh[1], diff(Import.T1.kWh)),
         realtime_2 = c(Import.T2.kWh[1], diff(Import.T2.kWh)),
         date = ymd_hm(time)) |> 
  transmute(date, realtime = realtime_1+realtime_2) |> 
  distinct(date, .keep_all = TRUE)

nrg <- nrg[-1,] |> 
  timeAverage("hour", statistic = "sum") |> 
  filter(realtime < 4)

nrg_annual <- timeAverage(nrg, "year", statistic = "sum")

easy_nrg <- nrg |> 
  filter(date > "2023-07-03") |> 
  mutate(day = lubridate::wday(date),
         hour = hour(date)) |> 
  filter(day == 1) |> 
  group_by(hour) |> 
  summarise(typical = mean(realtime, na.rm = TRUE))

easy_nrg_weekend <- nrg |> 
  filter(date > "2023-07-03") |> 
  mutate(day = lubridate::wday(date),
         hour = hour(date)) |> 
  filter(day == 1 | day == 7) |> 
  group_by(hour) |> 
  summarise(typical = mean(realtime, na.rm = TRUE))

easy_nrg_this_weekend <- nrg |> 
  filter(date > "2025-05-08") |> 
  mutate(day = lubridate::wday(date),
         hour = hour(date)) |> 
  filter(day == 1 | day == 7)  
  group_by(hour) |> 
  summarise(typical = mean(realtime, na.rm = TRUE))

sum(easy_nrg_weekend$typical)

prices <- read.csv("market_data/prices_11052025.csv")

nrg_may_11 <- prices |> 
  mutate(date = dmy_hm(date)) |> 
  left_join(nrg, "date") |> 
  mutate(price_vat = price) |> 
  mutate(cost = price_vat*realtime,
         hour = hour(date)) |> 
  select(hour, price,consumption = realtime, cost) |> 
  left_join(typical_frank, by = "hour")



sum(nrg_may_11$typical_cost)

frank_nrg <- left_join(frank,nrg, by = "date")

timeVariation(frank_nrg, c("consump_kWh", "realtime"))

library(bbplot)
library(ggplot2)


#Make plot
ggplot(nrg_may_11, aes(x = hour, y = cost)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_col(aes(y = typical_cost), fill = "#FAAB18", alpha = 0.5) +  # semi-transparent bars
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_x_continuous(breaks = seq(0, 24, by = 2))+
  scale_y_continuous(limits = c(-1.2,0.3), n.breaks = 8)+
  geom_curve(aes(x = 12.5, y = 0.15, xend = 10, yend = 0.06), 
               colour = "#555555", 
               size=0.5, 
               curvature = 0.2,
               arrow = arrow(length = unit(0.03, "npc")))+
  geom_label(aes(x = 12.5, y = 0.17, label = "Typical Sunday energy\nconsumption in yellow"), 
               hjust = 0, 
               vjust = 0.5, 
               colour = "#555555", 
               fill = "white", 
               label.size = NA, 
               family="Helvetica", 
               size = 3) +
  geom_curve(aes(x = 9, y = -1.1, xend = 12, yend = -1.06), 
             colour = "#555555", 
             size=0.5, 
             curvature = 0.05,
             arrow = arrow(length = unit(0.03, "npc")))+
  geom_label(aes(x = 1.5, y = -1.1, label = "Peak negative pricing 1-2pm -42c/kWh"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 3) +
  labs(title="The great Dutch grid giveaway",
       subtitle = "My hourly electricity bill yesterday (€)", 
       caption = "plotted using ggplot2")+
  bbc_style()
  

ggsave("bbc_graph.png")
