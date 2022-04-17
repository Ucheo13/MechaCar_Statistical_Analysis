library(readr)
library(tidyverse)
MechaCar_mpg <- read_csv("MechaCar_mpg.csv")
Suspension_Coil <- read_csv("Suspension_Coil.csv")

#Deliverable 1
reg <- lm(mpg ~ vehicle_weight + spoiler_angle + ground_clearance + AWD + vehicle_length,data=MechaCar_mpg) #generate multiple linear regression model
summary <- summary(reg)

#Deliverable 2
total_summary <-Suspension_Coil %>% summarise(Mean=mean(PSI), Median= median(PSI),Variance=var(PSI),SD=sd(PSI))

total_summary_lot <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarise(Mean=mean(PSI), Median= median(PSI),Variance=var(PSI),SD=sd(PSI))

#Deliverable 3
sample_table_1 <- Suspension_Coil[Suspension_Coil$Manufacturing_Lot== "Lot1",] %>% sample_n(50)

sample_table_2 <- Suspension_Coil[Suspension_Coil$Manufacturing_Lot== "Lot2",] %>% sample_n(50)

sample_table_3 <- Suspension_Coil[Suspension_Coil$Manufacturing_Lot== "Lot3",] %>% sample_n(50)

t_test1 <- t.test(log10(sample_table_1$PSI),mu=mean(log10(Suspension_Coil$PSI))) #compare sample versus population means

t_test2 <- t.test(log10(sample_table_2$PSI),mu=mean(log10(Suspension_Coil$PSI)))

t_test3 <- t.test(log10(sample_table_3$PSI),mu=mean(log10(Suspension_Coil$PSI)))
