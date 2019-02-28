library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

fishData <- read_excel("YPD_NCSU_nd0.xlsx", sheet="LR data-nd0",
                       range = "A4:BH135")

fishData2 <- rename(fishData, 
                    "length" = "L, mm", 
                    "weight" = "Wt, g", 
                    "HRMS_33_LR" = "HRMS (33-LR)", 
                    "LRMS_33" = "LRMS (33)", 
                    "LRMSMS_33" = "LRMSMS (33)", 
                    "lipid" = "%Lipid", 
                    "totalPCBperGramLipid" = "mg Total PCB / g lipid", 
                    "totalPCBperGramWeight" = "total PCB / weight")
#change variables to factors:
fishData2$Waterbody <- as.factor(fishData2$Waterbody)
fishData2$species <- as.factor(fishData2$species)
fishData2$TL <- as.factor(fishData2$TL)
fishData2$guild <- as.factor(fishData2$guild)

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
#for the information tab content (tab 1)
output$info <- renderUI({
    h4("Polychlorinated Biphenyl Congeners (PCBs) are organic chlorinated industrial chemicals. they are man-made and very stable. There are 209 distinct PCB compounds (known as congeners), which generally occur as mixtures of congeners. Since PCBs affect human and environmental health, they were prohibited from use in the United States since 1979.")
    br()
    h4("The primary exposure to PCBs is through fish consumption. In purpose of evaluating human health risk, the researchers quantified PCBs presence in fish that been collected from different rivers. This project aims to evaluate PCBs presence in fish in regarding to fish species and body of water in which the fish live. This project also aims to create a model for predicting PCBs contamination level by using other environmental factors, such as trophic level, fish weight, lipid content, etc., together with fish species and body of water. To evaluate PCBs presence, this study calculated total PCBs presence per unit of lipid as well as per unit of fish body weight. Additionally, to compare different methods of quantifying PCBs presence, researches also applied 3 ways to quantify the presence of a subset of 33 PCB congeners. The results will be used to compare these methods.")
    br()
    h4("The significance of this project is that by creating a model to predict fish PCBs toxicity, there will be a more cost effective way to accurately estimate PCBs for evaluating human health risk in regarding to PCBs.")
})
  
# for 2nd tab: ANOVA
#according to linear regression assumption analysis and compare to models without interaction, we finally use model with interaction and sqrt(PCB content) as response variable.
o.weight2 <- lm(I(sqrt(totalPCBperGramWeight)) ~ species + Waterbody + species:Waterbody, data=fishData2)

#model without species (so as interaction)=species not important in predict PCBs
o.weight_species <- lm(I(sqrt(totalPCBperGramWeight)) ~ Waterbody, data=fishData2)

#compare models
output$species_PCB <- renderPrint({
  anova(o.weight2, o.weight_species)
})

#model without Waterbody (so as interaction)=Waterbody not important in predict PCBs
o.weight_Waterbody <- lm(I(sqrt(totalPCBperGramWeight)) ~ species, data=fishData2)

#compare models
output$water_PCB <- renderPrint({
  anova(o.weight2, o.weight_Waterbody)
})

output$info7 <- renderUI({
  h4("If the p-value < 0.05, this factor does affect PCB content.")
})

#scatter plot
#plot1 <- ggplot(fishData2, aes(x = species, y = totalPCBperGramWeight)) + geom_point(aes(color = factor(waterbody))) +
 #       scale_color_discrete(name = "waterbody")
  
output$plots <- renderPlot({
  ggplot(fishData2, aes(x = species, y = totalPCBperGramWeight)) + geom_point(aes(color = factor(Waterbody))) +
    scale_color_discrete(name = "Waterbody")
  })

# for 3rd tab: building pridiction model for PCB
modelFit <- lm(I(sqrt(totalPCBperGramWeight)) ~ species + Waterbody +TL + guild + length + weight + lipid, data=fishData2)

output$fitting <- renderPrint({summary(modelFit)})
output$info2 <- renderUI({
  h4("Here are parameters for each variable in the model. To fit the equal variance and normality assumption fo using linear regression, the response variable in the model is transformed as square root of the PCB content.")
})

output$info3 <- renderUI({
  h4("The interaction terms are not included in building the model because not each pair of interaction have repeat observations for this analysis.")
})

#calculate PCB predict value:
#totalPCB <- input$speciesmodel*Fit$coefficients[1]...


# plot for 4th tab: compare quantification methods

output$info4 <- renderUI({
  h4("In the previous analysis, we’ve used values that have been calculated using the standard method by including all 209 PCB congeners. In addition to this standard quantifying method, researchers also applied two different methods as well as the standard method for quantifying a subset of 33 PCB congeners.")
  br()
  h4("To compare results from these two alternative methods to the standard method (HRMS vs LRMS, and HRMS vs LRMSMS), we applied hypothesis test for testing paired difference since each sample has values of PCB content that been quantified by each method.  The null hypothesis are “there is no difference of PCB content that been quantified using HRMS and LRMS: mu=0”, and “there is no difference of PCB content that been quantified using HRMS and LRMSMS: mu=0.")
})

#for std method HRMS & LRMS
#create new variable as the mean of the difference for each sample
HRMS_LRMS_bar <- sum(fishData2$LRMS_33-fishData2$HRMS_33_LR)/nrow(fishData2)

#get sd
HRMS_LRMS_sd <- sd(fishData2$LRMS_33-fishData2$HRMS_33_LR)

#t value
HRMS_LRMS_t <- HRMS_LRMS_bar/(HRMS_LRMS_sd/sqrt(nrow(fishData2)))

#for std method HRMS & LRMSMS
#create new variable as the mean of the difference for each sample
HRMS_LRMSMS_bar <- sum(fishData2$LRMSMS_33-fishData2$HRMS_33_LR)/nrow(fishData2)

#get sd
HRMS_LRMSMS_sd <- sd(fishData2$LRMSMS_33-fishData2$HRMS_33_LR)

#t value
HRMS_LRMSMS_t <- HRMS_LRMSMS_bar/(HRMS_LRMSMS_sd/sqrt(nrow(fishData2)))

t <- reactive({
  # Render a scatter plot
    if(input$vs=="HRMS vs LRMS"){ #1st dynamic UI
      HRMS_LRMS_t
    } else {
      HRMS_LRMSMS_t
    }
})

output$tvalue <- renderPrint({
  t
})

output$info5 <- renderUI({
  h4("By testing the paired difference, we get t value =")
})

output$info6 <- renderUI({
  h4("By comparing these t values using the table 'percentage points of Students’ t distribution', for t values =< 1.98, it represents p-value is greater than 0.05, thus, result in conclusion that there is no difference between the two methods. For t values > 1.98, it represents p-value is less than 0.05, thus, conclusion is that there is difference between the two methods.")
})
})