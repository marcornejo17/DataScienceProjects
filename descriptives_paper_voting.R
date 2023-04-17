## Women voting for women ##
## Descriptives: Access
## SVV and DD and SH ##

rm(list=ls(all=TRUE))
options(scipen = 100) #don't want scientific notation

library(tidyverse)
library(tidylog)
library(readstata13)
library(wesanderson)

#QUOTA
#Por año
## Get data ----
setwd("/Users/marianacornejo/Desktop/Estancia/")
load("candidate_leg.Rdata")

#setwd("/Users/sebastian/OneDrive - University Of Houston/Papers and Chapters/Women Voting for Women/Descriptives/Descriptives Paper")

## 1. Number of candidates running by gender:
candidate_leg_prov$type <- "PROVINCIAL"
candidate_leg_nac$type <- "NACIONAL"

candidate_leg <- rbind.data.frame(candidate_leg_prov,candidate_leg_nac)

#Percentage of legislative candidates by gender
candidate_leg %>%
  filter(year_election!=2021) %>%
  group_by(year_election,type) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,year_election,type) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,type,.keep_all=T) %>%
  ggplot(aes(x=year_election,y=pct_cand_gender,color = CANDIDATO_SEXO)) +
  geom_point() +
  geom_line() +
  facet_wrap(~type) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "black", alpha = .8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="Year", y = "% of Candidates", color = "",
       title = "Percentage of legislative candidates by gender")

#Intentos de gráficas por partido
table(candidate_leg$OP_SIGLAS)
candidate_leg$OP_unif <- ifelse(str_detect(candidate_leg$OP_SIGLAS,"MPAIS"),"MPAIS",NA)
candidate_leg$OP_unif <- ifelse(str_detect(candidate_leg$OP_SIGLAS,"PSC"),"PSC",candidate_leg$OP_unif)
candidate_leg$OP_unif <- ifelse(str_detect(candidate_leg$OP_SIGLAS,"PRE"),"PRE",candidate_leg$OP_unif)
candidate_leg$OP_unif <- ifelse(str_detect(candidate_leg$OP_SIGLAS,"PSP"),"PSP",candidate_leg$OP_unif)
candidate_leg$OP_unif <- ifelse(str_detect(candidate_leg$OP_SIGLAS,"MUPP"),"MUPP",candidate_leg$OP_unif)
candidate_leg$OP_unif <- ifelse(str_detect(candidate_leg$OP_SIGLAS,"ID"),"ID",candidate_leg$OP_unif)
candidate_leg$OP_unif <- ifelse(str_detect(candidate_leg$OP_SIGLAS,"DP"),"DP",candidate_leg$OP_unif)

table(candidate_leg$OP_unif)

#GRÁFICAS DESCRIPTIVAS

#PROVINCIALES Y NACIONALES
#Gráfica para contabilizar la cantidad de candidatos nacionales y provinciales

#PROVINCIALES Y NACIONALES PERIODO
candidate_leg %>%
  filter(year_election!=2021) %>%
  group_by(year_election, type) %>%
  mutate(tot_candidatos = n()) %>%
  ungroup() %>%
  distinct(year_election, tot_candidatos, type) %>%
  ggplot(aes(x=factor(year_election),y=tot_candidatos,color = type, fill = type)) +
  geom_col(alpha = 0.5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Election Year", y = "Number of Candidates", color = "", fill = "",
       title = "Number of Candidates Over Time")

ggsave(filename = "nombre.pdf", w = 10, h=8)

#PROVINCIALES Y NACIONALES GÉNERO
candidate_leg %>%
  filter(year_election!=2021) %>%
  group_by(year_election, CANDIDATO_SEXO) %>%
  mutate(tot_candidatos = n()) %>%
  ungroup() %>%
  distinct(year_election, tot_candidatos, CANDIDATO_SEXO) %>%
  ggplot(aes(x=factor(year_election),y=tot_candidatos,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = 0.5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Election Year", y = "Number of Candidates", color = "", fill = "",
       title = "Gender of Candidates Over Time")

ggsave(filename = "grafgenero.pdf", w = 10, h=8)

#Percentage of legislative candidates by gender by party
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  group_by(year_election,OP_unif) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(OP_unif,year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  distinct(OP_unif,year_election,CANDIDATO_SEXO,.keep_all=T) %>%
  ggplot(aes(x=year_election,y=pct_cand_gender,color = CANDIDATO_SEXO)) +
  geom_point() +
  geom_line() +
  facet_wrap(~OP_unif) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "black", alpha = .8) +
  geom_vline(aes(xintercept=2009), colour="#ED1C24", linetype="dashed") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="Year", y = "% of Candidates", color = "",
       title = "Percentage of legislative candidates by gender")

#Candidatos por partido nacional
table(candidate_leg$type)

table(candidate_leg$year_election)

#Percentage of national legislative candidates by gender
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  filter(type == 'NACIONAL') %>%
  group_by(year_election) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  distinct(year_election,CANDIDATO_SEXO,.keep_all=T) %>%
  ggplot(aes(x=year_election,y=pct_cand_gender,color = CANDIDATO_SEXO)) +
  geom_point() +
  geom_line() +
  # facet_wrap(~OP_unif) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "black", alpha = .8) +
  geom_vline(aes(xintercept=2009), colour="#ED1C24", linetype="dashed") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  ylim(c(0.25,0.75)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="Year", y = "% of Candidates", color = "",
       title = "Percentage of national legislative candidates by gender")

#Candidatos por partido nacional
table(candidate_leg$type)

table(candidate_leg$year_election)

#Percentage of provincial legislative candidates by gender
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  filter(type == 'PROVINCIAL') %>%
  group_by(year_election) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  distinct(year_election,CANDIDATO_SEXO,.keep_all=T) %>%
  ggplot(aes(x=year_election,y=pct_cand_gender,color = CANDIDATO_SEXO)) +
  geom_point() +
  geom_line() +
  # facet_wrap(~OP_unif) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "black", alpha = .8) +
  geom_vline(aes(xintercept=2009), colour="#ED1C24", linetype="dashed") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  ylim(c(0.25,0.75)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="Year", y = "% of Candidates", color = "",
       title = "Percentage of provincial legislative candidates by gender")


### ELECTOS O NO ELECTOS
table(candidate_leg$elected_dum)

#Percentage of elected legislative candidates by gender
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  filter(elected_dum == 'ELECTOS') %>%
  group_by(year_election) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  distinct(year_election,CANDIDATO_SEXO,.keep_all=T) %>%
  ggplot(aes(x=year_election,y=pct_cand_gender,color = CANDIDATO_SEXO)) +
  geom_point() +
  geom_line() +
  # facet_wrap(~OP_unif) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "black", alpha = .8) +
  geom_vline(aes(xintercept=2009), colour="#ED1C24", linetype="dashed") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  # ylim(c(0.00,0.52)) +
  coord_cartesian(x=c(2007,2011)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="Year", y = "% of Candidates", color = "",
       title = "Percentage of elected legislative candidates by gender")

#Percentage of non-elected legislative candidates by gender
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  filter(elected_dum == 'NO ELECTOS') %>%
  group_by(year_election,OP_unif) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(OP_unif,year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  distinct(OP_unif,year_election,CANDIDATO_SEXO,.keep_all=T) %>%
  ggplot(aes(x=year_election,y=pct_cand_gender,color = CANDIDATO_SEXO)) +
  geom_point() +
  geom_line() +
  facet_wrap(~OP_unif) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "black", alpha = .8) +
  geom_vline(aes(xintercept=2009), colour="#ED1C24", linetype="dashed") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  #ylim(c(0.45,0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="Year", y = "% of Candidates", color = "",
       title = "Percentage of non-elected legislative candidates by gender")

#Percentage of national elected legislative candidates by gender
#ELECTOS NACIONALES
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  filter(type == 'NACIONAL') %>%
  filter(elected_dum == 'ELECTOS') %>%
  group_by(year_election,OP_unif) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(OP_unif,year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  distinct(OP_unif,year_election,CANDIDATO_SEXO,.keep_all=T) %>%
  ggplot(aes(x=year_election,y=pct_cand_gender,color = CANDIDATO_SEXO)) +
  geom_point() +
  geom_line() +
  facet_wrap(~OP_unif) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "black", alpha = .8) +
  geom_vline(aes(xintercept=2009), colour="#ED1C24", linetype="dashed") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  #ylim(c(0.45,0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="Year", y = "% of Candidates", color = "",
       title = "Percentage of national elected legislative candidates by gender")

#Percentage of national non-elected legislative candidates by gender
#NO ELECTOS NACIONALES
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  filter(type == 'NACIONAL') %>%
  filter(elected_dum == 'NO ELECTOS') %>%
  group_by(year_election,OP_unif) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(OP_unif,year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  distinct(OP_unif,year_election,CANDIDATO_SEXO,.keep_all=T) %>%
  ggplot(aes(x=year_election,y=pct_cand_gender,color = CANDIDATO_SEXO)) +
  geom_point() +
  geom_line() +
  facet_wrap(~OP_unif) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "black", alpha = .8) +
  geom_vline(aes(xintercept=2009), colour="#ED1C24", linetype="dashed") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  #ylim(c(0.45,0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="Year", y = "% of Candidates", color = "",
       title = "Percentage of national non-elected legislative candidates by gender")

#Percentage of provincial elected legislative candidates by gender
#ELECTOS PROVINCIALES
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  filter(type == 'PROVINCIAL') %>%
  filter(elected_dum == 'ELECTOS') %>%
  group_by(year_election,OP_unif) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(OP_unif,year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  distinct(OP_unif,year_election,CANDIDATO_SEXO,.keep_all=T) %>%
  ggplot(aes(x=year_election,y=pct_cand_gender,color = CANDIDATO_SEXO)) +
  geom_point() +
  geom_line() +
  facet_wrap(~OP_unif) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "black", alpha = .8) +
  geom_vline(aes(xintercept=2009), colour="#ED1C24", linetype="dashed") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  #ylim(c(0.45,0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="Year", y = "% of Candidates", color = "",
       title = "Percentage of provincial elected legislative candidates by gender")

#Percentage of provincial non-elected legislative candidates by gender
#NO ELECTOS PROVINCIALES
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  filter(type == 'PROVINCIAL') %>%
  filter(elected_dum == 'NO ELECTOS') %>%
  group_by(year_election,OP_unif) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(OP_unif,year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  distinct(OP_unif,year_election,CANDIDATO_SEXO,.keep_all=T) %>%
  ggplot(aes(x=year_election,y=pct_cand_gender,color = CANDIDATO_SEXO)) +
  geom_point() +
  geom_line() +
  facet_wrap(~OP_unif) +
  geom_hline(yintercept = .5, linetype = "dashed", color = "black", alpha = .8) +
  geom_vline(aes(xintercept=2009), colour="#ED1C24", linetype="dashed") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  #ylim(c(0.45,0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="Year", y = "% of Candidates", color = "",
       title = "Percentage of provincial non-elected legislative candidates by gender")

#Ratio of candidates to elected by gender and organization
#RATIO OF CANDIDATES
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  group_by(year_election,OP_unif) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(OP_unif,year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  filter(elected_dum=="ELECTOS") %>%
  group_by(OP_unif,year_election) %>%
  mutate(total_elec = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,year_election,OP_unif) %>%
  mutate(total_elec_gender = n(),
         pct_elec_gender = total_elec_gender/total_elec) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,OP_unif,.keep_all=T) %>%
  ggplot(aes(x=pct_cand_gender,y=pct_elec_gender,color = CANDIDATO_SEXO, shape = OP_unif)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1, linetype = "dashed", color = "black", alpha = .8) +
  #scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  xlim(c(.15,.85))+
  ylim(c(.15,.85)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="% of Candidates", y = "% Elected", color = "", shape = "",
       title = "Ratio of candidates to elected by gender and organization",
       subtitle = "Note: Above the dashed line is overperfomance and below is underperformance")

#Ratio of national candidates to elected by gender and organization
#NATIONAL
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  filter(type == 'NACIONAL') %>%
  group_by(year_election,OP_unif) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(OP_unif,year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  filter(elected_dum=="ELECTOS") %>%
  group_by(OP_unif,year_election) %>%
  mutate(total_elec = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,year_election,OP_unif) %>%
  mutate(total_elec_gender = n(),
         pct_elec_gender = total_elec_gender/total_elec) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,OP_unif,.keep_all=T) %>%
  ggplot(aes(x=pct_cand_gender,y=pct_elec_gender,color = CANDIDATO_SEXO, shape = OP_unif)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1, linetype = "dashed", color = "black", alpha = .8) +
  #scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  coord_cartesian(x=c(0.4,0.6)) +
  coord_cartesian(y=c(0.4,0.6)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="% of Candidates", y = "% Elected", color = "", shape = "",
       title = "Ratio of national candidates to elected by gender and organization",
       subtitle = "Note: Above the dashed line is overperfomance and below is underperformance")

#Ratio of provintial candidates to elected by gender and organization
#SUMAR CUOTAS
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  group_by(year_election) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  filter(elected_dum=="ELECTOS") %>%
  group_by(year_election) %>%
  mutate(total_elec = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,year_election) %>%
  mutate(total_elec_gender = n(),
         pct_elec_gender = total_elec_gender/total_elec) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,.keep_all=T) %>%
  ggplot(aes(x=pct_cand_gender,y=pct_elec_gender,color = CANDIDATO_SEXO)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1, linetype = "dashed", color = "black", alpha = .8) +
  #scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  xlim(c(.15,.85))+
  ylim(c(.15,.85)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="% of Candidates", y = "% Elected", color = "", shape = "",
       title = "Ratio of candidates to elected by gender",
       subtitle = "Note: Above the dashed line is overperfomance and below is underperformance")


candidate_leg %>%
  mutate(after.quotas = ifelse(year_election > 2009,1,0)) %>%
  filter(year_election!=2021) %>%
  filter(!is.na(OP_unif)) %>%
  group_by(year_election) %>%
  mutate(total_cand = n()) %>%
  ungroup() %>%
  group_by(year_election,CANDIDATO_SEXO) %>%
  mutate(total_cand_gender = n(),
         pct_cand_gender = total_cand_gender/total_cand) %>%
  ungroup() %>%
  filter(elected_dum=="ELECTOS") %>%
  group_by(year_election) %>%
  mutate(total_elec = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,year_election) %>%
  mutate(total_elec_gender = n(),
         pct_elec_gender = total_elec_gender/total_elec) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,.keep_all=T) %>%
  ggplot(aes(x=pct_cand_gender,y=pct_elec_gender,color = CANDIDATO_SEXO)) +
  geom_point() +
  geom_abline(intercept = 0,slope = 1, linetype = "dashed", color = "black", alpha = .8) +
  #scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  facet_wrap(~after.quotas) +
  xlim(c(.15,.85))+
  ylim(c(.15,.85)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(x="% of Candidates", y = "% Elected", color = "", shape = "",
       title = "Ratio of candidates to elected by gender",
       subtitle = "Note: 0 is before quotas, 1 is after quotas")


## 2. Relative Position

#Distribution of *relative position* in list order of legislative candidates by gender
candidate_leg %>%
  filter(type == 'PROVINCIAL') %>%
  ggplot(aes(y=rel_position,x = CANDIDATO_SEXO, color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_violin(alpha = 0.5) + 
  geom_boxplot(width=0.1,alpha = 0.5 ) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  labs(x="", y = "Relaative Position", color = "", shape = "",
       title = "Distribution of *relative position* in list order of legislative candidates by gender",
       subtitle = "Note: Relative position weighs the order in the list by the district magnitude.")

#Distribution of *relative position* in list order of *elected* legislators by gender
#ELECTED
candidate_leg %>%
  filter(elected_dum=="ELECTOS") %>%
  ggplot(aes(y=rel_position,x = CANDIDATO_SEXO, color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_violin(alpha = 0.5) + 
  geom_boxplot(width=0.1,alpha = 0.5 ) +
  facet_wrap(~OP_unif) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  labs(x="", y = "Relaative Position", color = "", shape = "",
       title = "Distribution of *relative position* in list order of *elected* legislators by gender",
       subtitle = "Note: Relative position weighs the order in the list by the district magnitude.")

#Distribution of *relative position* in list order of *non-elected* legislators by gender
#NON-ELECTED
candidate_leg %>%
  filter(elected_dum=="NO ELECTOS") %>%
  ggplot(aes(y=rel_position,x = CANDIDATO_SEXO, color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_violin(alpha = 0.5) + 
  geom_boxplot(width=0.1,alpha = 0.5 ) +
  facet_wrap(~OP_unif) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  labs(x="", y = "Relaative Position", color = "", shape = "",
       title = "Distribution of *relative position* in list order of *non-elected* legislators by gender",
       subtitle = "Note: Relative position weighs the order in the list by the district magnitude.")


# 3. Tenure

#Distribution of tenure of legislative candidates by gender by party
candidate_leg %>%
  filter(year_election != 2021) %>% 
  filter(year_election != 2002) %>% # It starts in 2002
  filter(!is.na(OP_unif)) %>%
  group_by(year_election,tenure_all) %>%
  mutate(count_tenure = n()) %>%
  ungroup() %>%
  group_by(year_election,tenure_all,CANDIDATO_SEXO) %>%
  mutate(count_tenure_gender = n(),
         pct_tenure_gender = count_tenure_gender/count_tenure) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,tenure_all,.keep_all=T) %>%
  ggplot(aes(x=tenure_all,y=pct_tenure_gender,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO )) +
  geom_col(alpha = 0.5) +
  facet_wrap(~OP_unif) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  labs(x="Tenure (Candidacies)", y = "%", color = "", fill = "",
       title = "Distribution of tenure of legislative candidates by gender by party",
       subtitle = "Note: Each column is the % of candidates that had that number of candidacies during that election year by gender.")

#Distribution of tenure of legislative candidates by gender
candidate_leg %>%
  filter(year_election != 2021) %>% 
  filter(year_election != 2002) %>% # It starts in 2002
  group_by(year_election,tenure_all) %>%
  mutate(count_tenure = n()) %>%
  ungroup() %>%
  group_by(year_election,tenure_all,CANDIDATO_SEXO) %>%
  mutate(count_tenure_gender = n(),
         pct_tenure_gender = count_tenure_gender/count_tenure) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,tenure_all,.keep_all=T) %>%
  ggplot(aes(x=tenure_all,y=pct_tenure_gender,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO )) +
  geom_col(alpha = 0.5) +
  facet_wrap(~year_election) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  labs(x="Tenure (Candidacies)", y = "%", color = "", fill = "",
       title = "Distribution of tenure of legislative candidates by gender",
       subtitle = "Note: Each column is the % of candidates that had that number of candidacies during that election year by gender.")

#% of elected candidates by tenure and gender
candidate_leg %>%
  filter(year_election != 2021) %>% 
  filter(year_election != 2002) %>% # It starts in 2002
  group_by(year_election,tenure_all,CANDIDATO_SEXO) %>%
  mutate(count_tenure_gender = n()) %>%
  ungroup() %>%
  group_by(year_election,tenure_all,CANDIDATO_SEXO,elected_dum) %>%
  mutate(count_tenure_gender_elec = n(),
         pct_tenure_gender_elec = count_tenure_gender_elec/count_tenure_gender) %>%
  ungroup() %>%
  filter(elected_dum == "ELECTOS")%>%
  distinct(CANDIDATO_SEXO,year_election,tenure_all,elected_dum,.keep_all=T) %>%
  ggplot(aes(x=tenure_all,y=pct_tenure_gender_elec,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO )) +
  geom_col(alpha = 0.5, position = "dodge") +
  facet_wrap(~year_election) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  labs(x="Tenure (Candidacies)", y = "% Elected", color = "", fill = "",
       title = "% of elected candidates by tenure and gender")


# 4. Province 

#Number of candidates by province and gender
candidate_leg_prov %>%
  filter(year_election != 2021) %>% 
  filter(PROVINCIA_NOMBRE !="AMERICA LATINA EL CARIBE Y AFRICA") %>%
  filter(PROVINCIA_NOMBRE !="EEUU Y CANADA") %>%
  filter(PROVINCIA_NOMBRE !="EUROPA ASIA Y OCEANIA") %>%
  group_by(CANDIDATO_SEXO) %>%
  mutate(count_gender = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,PROVINCIA_NOMBRE) %>%
  mutate(count_gender_prov = n(),
         pct_gender_prov = count_gender_prov/count_gender_prov) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,PROVINCIA_NOMBRE,.keep_all=T) %>%
  ggplot(aes(x=(PROVINCIA_NOMBRE),y=count_gender_prov,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = 0.5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="", y = "", color = "", fill = "",
       title = "Number of candidates by province and gender",
       subtitle = "Note: Only provincial candidates")

#Number of elected candidates by province and gender
#ELECTED
candidate_leg_prov %>%
  filter(elected_dum=="ELECTOS") %>%
  filter(year_election != 2021) %>% 
  filter(PROVINCIA_NOMBRE !="AMERICA LATINA EL CARIBE Y AFRICA") %>%
  filter(PROVINCIA_NOMBRE !="EEUU Y CANADA") %>%
  filter(PROVINCIA_NOMBRE !="EUROPA ASIA Y OCEANIA") %>%
  group_by(CANDIDATO_SEXO) %>%
  mutate(count_gender = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,PROVINCIA_NOMBRE) %>%
  mutate(count_gender_prov = n(),
         pct_gender_prov = count_gender_prov/count_gender_prov) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,PROVINCIA_NOMBRE,.keep_all=T) %>%
  ggplot(aes(x=(PROVINCIA_NOMBRE),y=count_gender_prov,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = 0.5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="", y = "", color = "", fill = "",
       title = "Number of elected candidates by province and gender",
       subtitle = "Note: Only provincial candidates")

#Number of non-elected candidates by province and gender
#NON-ELECTED
candidate_leg_prov %>%
  filter(elected_dum=="NO ELECTOS") %>%
  filter(year_election != 2021) %>% 
  filter(PROVINCIA_NOMBRE !="AMERICA LATINA EL CARIBE Y AFRICA") %>%
  filter(PROVINCIA_NOMBRE !="EEUU Y CANADA") %>%
  filter(PROVINCIA_NOMBRE !="EUROPA ASIA Y OCEANIA") %>%
  group_by(CANDIDATO_SEXO) %>%
  mutate(count_gender = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,PROVINCIA_NOMBRE) %>%
  mutate(count_gender_prov = n(),
         pct_gender_prov = count_gender_prov/count_gender_prov) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,PROVINCIA_NOMBRE,.keep_all=T) %>%
  ggplot(aes(x=(PROVINCIA_NOMBRE),y=count_gender_prov,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = 0.5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="", y = "", color = "", fill = "",
       title = "Number of non-elected candidates by province and gender",
       subtitle = "Note: Only provincial candidates")

#Only Provincial candidates
candidate_leg_prov %>%
  filter(year_election != 2021) %>% 
  filter(PROVINCIA_NOMBRE !="AMERICA LATINA EL CARIBE Y AFRICA") %>%
  filter(PROVINCIA_NOMBRE !="EEUU Y CANADA") %>%
  filter(PROVINCIA_NOMBRE !="EUROPA ASIA Y OCEANIA") %>%
  group_by(CANDIDATO_SEXO) %>%
  mutate(count_gender = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,PROVINCIA_NOMBRE) %>%
  mutate(count_gender_prov = n(),
         pct_gender_prov = count_gender_prov/count_gender) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,PROVINCIA_NOMBRE,.keep_all=T) %>%
  ggplot(aes(x=(PROVINCIA_NOMBRE),y=pct_gender_prov,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = 0.5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="", y = "", color = "", fill = "",
       title = "Number of candidates by province and gender",
       subtitle = "Note: Only provincial candidates")


# 5. District Mag 

#Number of candidates by district magnitude and gender
candidate_leg %>%
  filter(year_election != 2021) %>% 
  # filter(PROVINCIA_NOMBRE !="AMERICA LATINA EL CARIBE Y AFRICA") %>%
  # filter(PROVINCIA_NOMBRE !="EEUU Y CANADA") %>%
  # filter(PROVINCIA_NOMBRE !="EUROPA ASIA Y OCEANIA") %>%
  group_by(CANDIDATO_SEXO) %>%
  mutate(count_gender = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,district_mag) %>%
  mutate(count_gender_prov = n(),
         pct_gender_prov = count_gender_prov/count_gender_prov) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,district_mag,.keep_all=T) %>%
  ggplot(aes(x=factor(district_mag),y=count_gender_prov,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = 0.5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="District Magnitude", y = "", color = "", fill = "",
       title = "Number of candidates by district magnitude and gender",
       subtitle = "Note: Some provinces were redistricted after 2013")

#Number of elected candidates by district magnitude and gender
#ELECTED
candidate_leg %>%
  filter(year_election != 2021) %>% 
  filter(elected_dum=="ELECTOS") %>%
  # filter(PROVINCIA_NOMBRE !="AMERICA LATINA EL CARIBE Y AFRICA") %>%
  # filter(PROVINCIA_NOMBRE !="EEUU Y CANADA") %>%
  # filter(PROVINCIA_NOMBRE !="EUROPA ASIA Y OCEANIA") %>%
  group_by(CANDIDATO_SEXO) %>%
  mutate(count_gender = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,district_mag) %>%
  mutate(count_gender_prov = n(),
         pct_gender_prov = count_gender_prov/count_gender_prov) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,district_mag,.keep_all=T) %>%
  ggplot(aes(x=factor(district_mag),y=count_gender_prov,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = 0.5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="District Magnitude", y = "", color = "", fill = "",
       title = "Number of elected candidates by district magnitude and gender",
       subtitle = "Note: Some provinces were redistricted after 2013")

#Number of Non-Elected candidates by district magnitude and gender
#NON-ELECTED
candidate_leg %>%
  filter(year_election != 2021) %>%
  filter(elected_dum=="NO ELECTOS") %>%
  # filter(PROVINCIA_NOMBRE !="AMERICA LATINA EL CARIBE Y AFRICA") %>%
  # filter(PROVINCIA_NOMBRE !="EEUU Y CANADA") %>%
  # filter(PROVINCIA_NOMBRE !="EUROPA ASIA Y OCEANIA") %>%
  group_by(CANDIDATO_SEXO) %>%
  mutate(count_gender = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,district_mag) %>%
  mutate(count_gender_prov = n(),
         pct_gender_prov = count_gender_prov/count_gender_prov) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,district_mag,.keep_all=T) %>%
  ggplot(aes(x=factor(district_mag),y=count_gender_prov,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = 0.5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="District Magnitude", y = "", color = "", fill = "",
       title = "Number of Non-Elected candidates by district magnitude and gender",
       subtitle = "Note: Some provinces were redistricted after 2013")

#Number of candidates by district magnitude and gender by year
#FOR YEAR
candidate_leg %>%
  filter(year_election != 2021) %>% 
  # filter(PROVINCIA_NOMBRE !="AMERICA LATINA EL CARIBE Y AFRICA") %>%
  # filter(PROVINCIA_NOMBRE !="EEUU Y CANADA") %>%
  # filter(PROVINCIA_NOMBRE !="EUROPA ASIA Y OCEANIA") %>%
  group_by(CANDIDATO_SEXO,year_election) %>%
  mutate(count_gender = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,district_mag,year_election) %>%
  mutate(count_gender_prov = n(),
         pct_gender_prov = count_gender_prov/count_gender) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,district_mag,year_election,.keep_all=T) %>%
  ggplot(aes(x=factor(district_mag),y=pct_gender_prov,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = 0.5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  facet_wrap(~year_election, scales = "free_x") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="District Magnitude", y = "", color = "", fill = "",
       title = "Number of candidates by district magnitude and gender",
       subtitle = "Note: Some provinces were redistricted after 2013")

#Number of elected candidates by district magnitude and gender by year
#ELECTED
candidate_leg %>%
  filter(year_election != 2021) %>% 
  filter(elected_dum=="ELECTOS") %>%
  # filter(PROVINCIA_NOMBRE !="AMERICA LATINA EL CARIBE Y AFRICA") %>%
  # filter(PROVINCIA_NOMBRE !="EEUU Y CANADA") %>%
  # filter(PROVINCIA_NOMBRE !="EUROPA ASIA Y OCEANIA") %>%
  group_by(CANDIDATO_SEXO,year_election) %>%
  mutate(count_gender = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,district_mag,year_election) %>%
  mutate(count_gender_prov = n(),
         pct_gender_prov = count_gender_prov/count_gender) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,district_mag,year_election,.keep_all=T) %>%
  ggplot(aes(x=factor(district_mag),y=pct_gender_prov,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = 0.5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  facet_wrap(~year_election, scales = "free_x") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="District Magnitude", y = "", color = "", fill = "",
       title = "Number of elected candidates by district magnitude and gender by year",
       subtitle = "Note: Some provinces were redistricted after 2013")

#Number of non-elected candidates by district magnitude and gender by year
#NON-ELECTED
candidate_leg %>%
  filter(year_election != 2021) %>% 
  filter(elected_dum=="NO ELECTOS") %>%
  # filter(PROVINCIA_NOMBRE !="AMERICA LATINA EL CARIBE Y AFRICA") %>%
  # filter(PROVINCIA_NOMBRE !="EEUU Y CANADA") %>%
  # filter(PROVINCIA_NOMBRE !="EUROPA ASIA Y OCEANIA") %>%
  group_by(CANDIDATO_SEXO,year_election) %>%
  mutate(count_gender = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,district_mag,year_election) %>%
  mutate(count_gender_prov = n(),
         pct_gender_prov = count_gender_prov/count_gender) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,district_mag,year_election,.keep_all=T) %>%
  ggplot(aes(x=factor(district_mag),y=pct_gender_prov,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = 0.5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  facet_wrap(~year_election, scales = "free_x") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="District Magnitude", y = "", color = "", fill = "",
       title = "Number of non-elected candidates by district magnitude and gender",
       subtitle = "Note: Some provinces were redistricted after 2013")


# 6. Head of list

#Heads of lists by gender
candidate_leg %>%
  filter(year_election != 2021) %>% 
  filter(CANDIDATO_ORDEN_LISTA == 1) %>%
  group_by(year_election) %>%
  mutate(total_head = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,year_election) %>%
  mutate(total_gender_head = n(),
         pct_gender_head = total_gender_head/total_head) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,.keep_all=T) %>%
  ggplot(aes(x=factor(year_election),y=pct_gender_head,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = .5) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Year", y = "%", color = "", fill = "",
       title = "Heads of lists by gender")

#Heads of lists by gender and party
candidate_leg %>%
  filter(year_election != 2021) %>% 
  filter(CANDIDATO_ORDEN_LISTA == 1) %>%
  filter(!is.na(OP_unif)) %>%
  group_by(year_election) %>%
  mutate(total_head = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,year_election) %>%
  mutate(total_gender_head = n(),
         pct_gender_head = total_gender_head/total_head) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,.keep_all=T) %>%
  ggplot(aes(x=factor(year_election),y=pct_gender_head,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = .5) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  facet_wrap(~OP_unif) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Year", y = "%", color = "", fill = "",
       title = "Heads of lists by gender and party")

#% of heads of lists that were elected by gender
candidate_leg %>%
  filter(year_election != 2021) %>% 
  filter(CANDIDATO_ORDEN_LISTA == 1) %>%
  group_by(year_election,CANDIDATO_SEXO) %>%
  mutate(total_head_gender = n()) %>%
  ungroup() %>%
  filter(elected_dum == "ELECTOS") %>%
  group_by(CANDIDATO_SEXO,year_election) %>%
  mutate(total_head_elec = n(),
         pct_gender_head_elec = total_head_elec/total_head_gender) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,.keep_all=T) %>%
  ggplot(aes(x=factor(year_election),y=pct_gender_head_elec,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = .5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Year", y = "%", color = "", fill = "",
       title = "% of heads of lists that were elected by gender")

#% of heads of lists national and provincial that were elected by gender
#NACIONAL
candidate_leg %>%
  filter(year_election != 2021) %>% 
  filter(CANDIDATO_ORDEN_LISTA == 1) %>%
  group_by(year_election,CANDIDATO_SEXO,type) %>%
  mutate(total_head_gender = n()) %>%
  ungroup() %>%
  filter(elected_dum == "ELECTOS") %>%
  group_by(CANDIDATO_SEXO,year_election,type) %>%
  mutate(total_head_elec = n(),
         pct_gender_head_elec = total_head_elec/total_head_gender) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,type,.keep_all=T) %>%
  ggplot(aes(x=factor(year_election),y=pct_gender_head_elec,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_col(alpha = .5, position = "dodge") +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  geom_vline(aes(xintercept=2009), colour="#ED1C24", linetype="dashed") +
  facet_wrap(~type) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="Year", y = "%", color = "", fill = "",
       title = "% of heads of lists national and provincial that were elected by gender") +
  geom_vline(aes(xintercept=2009), colour="#ED1C24", linetype="dashed")

#Ratio of candidates heads of lists to elected heads of lists by gender
candidate_leg %>%
  filter(year_election != 2021) %>% 
  filter(CANDIDATO_ORDEN_LISTA == 1) %>%
  group_by(year_election) %>%
  mutate(total_head = n()) %>%
  ungroup() %>%
  group_by(year_election,CANDIDATO_SEXO) %>%
  mutate(total_head_gender = n(),
         pct_head_gender = total_head_gender/total_head) %>%
  ungroup() %>%
  filter(elected_dum == "ELECTOS") %>%
  group_by(year_election) %>%
  mutate(total_elec = n()) %>%
  ungroup() %>%
  group_by(CANDIDATO_SEXO,year_election) %>%
  mutate(total_head_elec = n(),
         pct_gender_head_elec = total_head_elec/total_elec) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,.keep_all=T) %>%
  ggplot(aes(x=pct_head_gender,y=pct_gender_head_elec,color = CANDIDATO_SEXO, fill = CANDIDATO_SEXO)) +
  geom_point(alpha = .5) +
  geom_abline(intercept = 0,slope = 1, linetype = "dashed", color = "black", alpha = .8) +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  scale_fill_manual(values= wes_palette("Royal1", n = 2)) +
  xlim(c(.05,.95))+
  ylim(c(.05,.95)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="% Candidates", y = "% Elected", color = "", fill = "",
       title = "Ratio of candidates heads of lists to elected heads of lists by gender",
       subtitle = "Note: Above the dashed line is overperfomance and below is underperformance")


# 7. Ran next 

#Percentage of legislative candidates that ran again by gender
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(year_election!=2017) %>%
  group_by(year_election,CANDIDATO_SEXO) %>%
  mutate(total_gender = n(),
         total_gender_rannext = sum(ran_next),
         pct_gender_rannext = total_gender_rannext/total_gender) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,.keep_all=T) %>%
  ggplot(aes(x=factor(year_election),y=pct_gender_rannext,color = CANDIDATO_SEXO, group = CANDIDATO_SEXO)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x="Year", y = "% of Candidates", color = "",
       title = "Percentage of legislative candidates that ran again by gender")

#Percentage of legislative national and provincial candidates that ran again by gender
#ELECTED
candidate_leg %>%
  filter(year_election!=2021) %>%
  filter(year_election!=2017) %>%
  group_by(year_election,CANDIDATO_SEXO) %>%
  mutate(total_gender = n(),
         total_gender_rannext = sum(ran_next),
         pct_gender_rannext = total_gender_rannext/total_gender) %>%
  ungroup() %>%
  distinct(CANDIDATO_SEXO,year_election,.keep_all=T) %>%
  ggplot(aes(x=factor(year_election),y=pct_gender_rannext,color = CANDIDATO_SEXO, group = CANDIDATO_SEXO)) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values= wes_palette("Royal1", n = 2)) +
  theme_minimal() +
  facet_wrap(~elected_dum) +
  theme(legend.position = "bottom") +
  labs(x="Year", y = "% of Candidates", color = "",
       title = "Percentage of legislative national and provincial candidates that ran again by gender")

  
