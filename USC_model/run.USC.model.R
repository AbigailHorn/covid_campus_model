# test
setwd("~/Google Drive File Stream/My Drive/Collaborations/COVID19/USC.University.SEIR.Model/USC.model")

source("USC_dependencies.R")
source("USC_model_func.R")
source("USC_parm_init_control.R")
source("USC_psa_optimizedistr.R")
source("USC_psa_parm.R")   #Note this overwrites initial parameters from parm_init_control

# 
param <- param.dcm(latent = latent.int,
                   infectious = infectious.int,
                   isolation = isolation,
                   beta_student_to_student = beta_student_to_student.int,
                   beta_on_to_on = beta_on_to_on.int,
                   beta_saf = beta_saf.int,
                   community = community.int,
                   p_asympt_stu = p_asympt_stu.int,
                   p_asympt_saf = p_asympt_saf.int,
                   p_hosp_stu = p_hosp_stu.int,
                   p_hosp_saf = p_hosp_saf.int,
                   p_death_stu = p_death_stu.int,
                   p_death_saf = p_death_saf.int,
                   contacts = contacts.int,
                   p_contacts_reached = p_contacts_reached.int,
                   ili = ili.int,
                   sensitivity = sensitivity.int,
                   eff_npi = eff_npi.int,
                   testing = 1/testing,
                   screening_on = 1/screening_on,
                   screening=0)
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

probs=c(0.25,0.5,0.75)

par(mfrow=c(1,3))
pdf("plot.pdf")

#Infections
all.I <- active<-mod %>% select(run,time,I_stu, I_saf, Icum_stu, Icum_saf) %>% group_by(time)

stu.I <- all.I %>% do(data.frame(t(quantile(.$I_stu, probs))))
colnames(stu.I)<-c("time","lower","mean","upper")
stu.I$var<-"student"

saf.I <- all.I %>% do(data.frame(t(quantile(.$I_saf, probs))))
colnames(saf.I)<-c("time","lower","mean","upper")
saf.I$var<-"staff"

n.all.I <-rbind(stu.I,saf.I)

ggplot(data=n.all.I,aes(x=time,y=mean,fill=var))+geom_line(aes(colour = var), linetype = 2)+
  scale_color_manual(values=c("darkred","darkblue"))+
  geom_ribbon(aes(ymin=lower,ymax=upper),linetype=2,alpha=0.2)+
  scale_fill_manual(values=c("darkred","#0066CC"))+
  ggtitle("Infections Over Time")+xlab("Days")+ylab("Infections")+
  theme_classic()+theme(legend.title=element_blank(),plot.title = element_text(size=10))


#Isolation
all.P <- active<-mod %>% select(run,time,P_stu, P_saf, Pcum_stu, Pcum_saf) %>% group_by(time)

stu.P <- all.P %>% do(data.frame(t(quantile(.$P_stu, probs))))
colnames(stu.P)<-c("time","lower","mean","upper")
stu.P$var<-"student"

saf.P <- all.P %>% do(data.frame(t(quantile(.$P_saf, probs))))
colnames(saf.P)<-c("time","lower","mean","upper")
saf.P$var<-"staff"

n.all.P <-rbind(stu.P,saf.P)

ggplot(data=n.all.P,aes(x=time,y=mean,fill=var))+geom_line(aes(colour = var), linetype = 2)+
  scale_color_manual(values=c("darkred","darkblue"))+
  geom_ribbon(aes(ymin=lower,ymax=upper),linetype=2,alpha=0.2)+
  scale_fill_manual(values=c("darkred","#0066CC"))+
  ggtitle("Isolation Over Time")+xlab("Days")+ylab("Isolation")+
  theme_classic()+theme(legend.title=element_blank(),plot.title = element_text(size=10))


# Quarantine
all.Q <- active<-mod %>% select(run,time,Q_stu, Qcum_stu) %>% group_by(time)

stu.Q <- all.Q %>% do(data.frame(t(quantile(.$Q_stu, probs))))
colnames(stu.Q)<-c("time","lower","mean","upper")
stu.Q$var<-"student"

ggplot(data=stu.Q,aes(x=time,y=mean,fill=var))+geom_line(aes(colour = var), linetype = 2)+
  scale_color_manual(values=c("darkred","darkblue"))+
  geom_ribbon(aes(ymin=lower,ymax=upper),linetype=2,alpha=0.2)+
  scale_fill_manual(values=c("darkred","#0066CC"))+
  ggtitle("Quarantine Over Time")+xlab("Days")+ylab("Quarantine")+
  theme_classic()+theme(legend.title=element_blank(),plot.title = element_text(size=10))

dev.off()

