## necessary packages 

# library(readxl)
# library(tidyverse)
# library(ggtext)
# library(cowplot)





# load data
data <- read_excel("data/data.xlsx", na = "na")



d <- data |> 
  
  #select variables of intrest 
  select(id, sex, timepoint, test, weight, vo2) |>
  
  #filter test: max  
  filter(test == "max") |>
  
  #calculate vo2/mL/kg
  mutate(vo2.kg = vo2/weight) |>
  
  #remove test, weight and vo2
  select(-test : -vo2) |>
  
  pivot_wider(names_from = timepoint,
              values_from = vo2.kg) |>
  
  #select and and change names for the variables  
  select(id, sex, "0" = `1`, "8" = `2`, "22" = `3`, "32" = `4`, "40" = `5`, "48" = `6`) |>
  
  #make lang format 
  pivot_longer(cols = c("0":"48"),
               names_to = "timepoint",
               values_to = "vo2.kg") |>
  
  #set timepoint to numeric 
  mutate(timepoint = as.numeric(timepoint))




mean <-  d |>
  
  #summarize mean and sd values for both sex at each timepoint
  group_by(sex, timepoint) |>
  summarise(mean = mean(vo2.kg, na.rm = T),
            sd = sd(vo2.kg, na.rm = T))
  
  #write caption text
  caption_text <- "**Figure 1** illustrates maximal oxygen uptake indexed to body mass for females (red) and males (blue) before, during, and after a 1-year endurance training period. Individual values are represented by transparent points, while the mean and standard deviation are shown with solid points and connected whiskers." 

  
  #make ggplot 
  plot <- ggplot(aes(timepoint, vo2.kg, colour = sex, group = sex), data = d) +
  
  #include individual values at each timepoint an make each point transparent
  geom_point(alpha = 0.2) +
  
  #include mean and sd values for both sex at each timepoint and use position_doge to seperate de points and lines  
  geom_line(data = mean, aes(timepoint, mean, group = sex, color = sex),
            position = position_dodge(width = 1.5)) +
  geom_point(data = mean, aes(timepoint, mean, group = sex, color = sex),
             position = position_dodge(width = 1.5),
             size = 2.5) +
  geom_linerange(data = mean, aes(timepoint, mean, ymin = mean - sd, ymax = mean + sd),
                 position = position_dodge(width = 1.5),
                 size = 1) +
  
  #set theme_classic because its nice 
  theme_classic() +
  
  #use labs to adjust y-axis text 
  labs(y = expression("V\u0307"*O[2*max]~(mL %.% min^{-1} %.% kg^{-1}))) + 
    
  labs(caption = caption_text) +  
  
  #adjust y-axis breaks   
  scale_y_continuous(limits = c(15, 55),
                     breaks = c(20, 25, 30, 35, 40, 45, 50, 55),
                     labels = c(20, 25, 30, 35, 40, 45, 50, 55),
                     expand = c(0,0)) +
  
  #adjust x-axis brakes and lables 
  scale_x_continuous(breaks = c(0, 8, 22, 32, 40, 48),
                     labels = c("week 0",
                                "week 8 ",
                                "week 22",
                                "week 32",
                                "week 40",
                                "week 48"),
                     expand = c(0.05, 0.05)) +
    
    #change colors and legend text 
  scale_color_manual(values = c("f" = "red", "m" = "blue"),
                     name = "Sex", 
                     labels = c("Female", "Male")) +

  #adjust text sizes     
  theme(axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 15,  color = "black"),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "top",
        plot.caption = element_textbox_simple(padding = margin(20, 10, 5, 0), size = 18)) 
  

 
  
  ggsave2("figure1.png", plot, path = "figures", width = 30, height = 20,
          dpi = 600,
          units = "cm", device= "png")
         