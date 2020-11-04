# test
setwd("~/Google Drive File Stream/My Drive/Collaborations/COVID19/USC.University.SEIR.Model/USC.model")

source("USC_dependencies.R")
source("USC_model_func.R")
source("USC_parm_init_control.R")

# 
param <- param.dcm(latent = latent,
                   infectious = infectious,
                   isolation = isolation,
                   R0_student_to_student = R0_student_to_student,
                   R0_on_to_on = R0_on_to_on,
                   R0_saf = R0_saf,
                   beta_student_to_student = beta_student_to_student,
                   beta_on_to_on = beta_on_to_on,
                   beta_saf = beta_saf,
                   community = community,
                   p_asympt_stu = p_asympt_stu,
                   p_asympt_saf = p_asympt_saf,
                   p_hosp_stu = p_hosp_stu,
                   p_hosp_saf = p_hosp_saf,
                   p_death_stu = p_death_stu,
                   p_death_saf = p_death_saf,
                   contacts = contacts,
                   p_contacts_reached = p_contacts_reached,
                   ili = ili,
                   sensitivity = sensitivity,
                   sensitivity_on = sensitivity_on,
                   sensitivity_saf = sensitivity_saf,
                   eff_npi = eff_npi,
                   testing = 1/testing,
                   screening_on = 1/screening_on,
                   screening=1/screening,
                   screening_saf=1/screening_saf)

mod <- dcm(param, init, control)
mod <- mutate_epi(mod, I_stu = Isym_on + Isym_off,
                           Icum_stu = Icum_on + Icum_off,
                           P_stu = P_on + P_off,
                            Q_stu = Q_on + Q_off,
                           Pcum_stu = Pcum_on + Pcum_off,
                           Qcum_stu = Qcum_on + Qcum_off,
                           Hcum_stu = Hcum_on + Hcum_off,
                           Dcum_stu = Dcum_on + Dcum_off)
mod <- as.data.frame(mod)
mod.m <- reshape::melt(mod, id.var="time")

par(mfrow=c(1,3))
pdf("SingleRunPlot.pdf")

#Infections
ggplot(data=mod,aes(x=time))+
  geom_line(aes(y=I_stu, colour = "Students"), linetype = 1, size=2)+
  geom_line(aes(y=I_saf, colour = "Staff"), linetype = 1, size=2)+
  ggtitle("Infections Over Time")+xlab("Days")+ylab("Infections")+
  theme_classic()+
  scale_colour_manual("Group", values=c("Students"="red", "Staff"="blue"))


#Isolation
ggplot(data=mod,aes(x=time))+
  geom_line(aes(y=P_stu, colour = "Students"), linetype = 1, size=2)+
  geom_line(aes(y=P_saf, colour = "Staff"), linetype = 1, size=2)+
  ggtitle("Isolation Over Time")+xlab("Days")+ylab("Isolation")+
  theme_classic()+
  scale_colour_manual("Group", values=c("Students"="red", "Staff"="blue"))

# Quarantine
ggplot(data=mod,aes(x=time))+
  geom_line(aes(y=Q_stu, colour = "Students"), linetype = 1, size=2)+
  geom_line(aes(y=Q_saf, colour = "Staff"), linetype = 1, size=2)+
  ggtitle("Quarantine Over Time")+xlab("Days")+ylab("Quarantine")+
  theme_classic()+
  scale_colour_manual("Group", values=c("Students"="red", "Staff"="blue"))

dev.off()


# summary table
df_cum<-mod %>%
  filter(time == max(time)) %>%
  summarize(
    student_n = S_on + E_on + I_on + R_on + P_on + Q_on - Dcum_on +
      S_off + E_off + I_off + R_off + P_off + Q_off - Dcum_off,
    student_cases = Icum_on + Icum_off,
    student_hosps = Hcum_on + Hcum_off,
    student_isos = Pcum_on + Pcum_off,
    student_quas = Qcum_on + Qcum_off,
    student_deaths = Dcum_on + Dcum_off,
    saf_n = S_saf + E_saf + I_saf + R_saf + P_saf + Q_saf - Dcum_saf,
    saf_cases = Icum_saf,
    saf_hosps = Hcum_saf,
    saf_deaths = Dcum_saf,
    tests = Test
  )

tests_pc = df_cum$tests / (df_cum$student_n + df_cum$saf_n)
df_cum <- data.frame(df_cum, tests_pc)

df_peak <- mod %>%
  summarize(
    student_cases_peak = max(Isym_on + Isym_off, na.rm = TRUE),
    student_case_peak_day = match(student_cases_peak, Isym_on + Isym_off),
    student_isos_peak = max(P_on + P_off, na.rm = TRUE),
    student_isos_peak_day = match(student_isos_peak, P_on + P_off),
    student_isos_days = sum(P_on + P_off, na.rm = TRUE),
    student_quas_peak = max(Q_on + Q_off, na.rm = TRUE),
    student_quas_peak_day = match(student_quas_peak, Q_on + Q_off),
    student_quas_days = sum(Q_on + Q_off, na.rm = TRUE),
    saf_cases_peak = max(Isym_saf, na.rm = TRUE),
    saf_case_peak_day = match(saf_cases_peak, Isym_saf),
  )

df_out <- data.frame(df_cum, df_peak)

var_order <- c("student_n", "student_cases","student_cases_peak","student_case_peak_day",
               "student_isos","student_isos_peak","student_isos_peak_day","student_isos_days",
               "student_quas","student_quas_peak","student_quas_peak_day", "student_quas_days",
               "saf_n", "saf_cases","saf_cases_peak","saf_case_peak_day",
               "tests","tests_pc")
df_out <- df_out[,var_order]

round(t(df_out),0)


