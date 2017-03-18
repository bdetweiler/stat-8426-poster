library(MASS)
library(car)

dat = read.csv("sortedData_0315.csv")
dat$Response = dat$Response+2
dat$Patient = as.factor(dat$Patient)

mod.null = lm(Response~1, data = dat)
mod.full = lm(Response~(.+.-DIS_Ans-SOC_Ans-X-Patient)^2, data = dat)
step(mod.full, scope = list(lower = mod.null,upper = mod.full), direction = "both")


mod.new2 = lm(formula = Response ~ Age + Gender + PreIns + Site + Stay.Length + 
                SOC_Tine + SOC_Vt + SOC_Vc + DIS_Tine + DIS_Vt + DIS_Vc + 
                Dsc + Tine + Video.Time + Age:Gender + Age:Site + Age:Tine + 
                Gender:Site + Gender:SOC_Vt + Gender:SOC_Vc + Gender:Dsc + 
                Gender:Tine + PreIns:Site + PreIns:Stay.Length + PreIns:SOC_Vt + 
                PreIns:SOC_Vc + PreIns:DIS_Vt + PreIns:DIS_Vc + PreIns:Dsc + 
                PreIns:Tine + Site:Stay.Length + Site:SOC_Vt + Site:SOC_Vc + 
                Site:DIS_Vt + Site:DIS_Vc + Site:Tine + Stay.Length:SOC_Vt + 
                Stay.Length:SOC_Vc + Stay.Length:DIS_Vt + Stay.Length:DIS_Vc + 
                Stay.Length:Tine + SOC_Tine:DIS_Vt + SOC_Tine:DIS_Vc + SOC_Tine:Dsc + 
                SOC_Vt:SOC_Vc + SOC_Vt:DIS_Tine + SOC_Vt:DIS_Vt + SOC_Vt:DIS_Vc + 
                SOC_Vt:Video.Time + SOC_Vc:DIS_Tine + SOC_Vc:DIS_Vt + SOC_Vc:DIS_Vc + 
                SOC_Vc:Dsc + DIS_Tine:Dsc + DIS_Vc:Dsc, data = dat)

summary(mod.new2)
Anova(mod.new2)
plot(rstudent(mod.new2))
qqnorm(rstudent(mod.new2))
qqplot(rstudent(mod.new2))
qqline(rstudent(mod.new2))
