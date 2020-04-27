---
title: "Capstone 2020"
author: "Pai Liu"
date: "4/28/2929"
output: html_document
---

## Question1

1) Provide a brief background and significance about a specific research problem that interests you. It could be project you’re involved with now, or a rotation project, or something you’d like to work on. The reader will need to understand enough background to make sense of the experiment you propose below. Keep it brief. In one short paragraph.

**Hypoglycemia is a condition when blood sugar level drops lower than normal. It can be dangerous as individuals with hypoglycemia can show symptoms such as visual distrubances, seizures, and loss of consciousness if medical care is not seeked immediately. APOE is a gene that not only mediates amyloid-β (Aβ) degradation in Alzheimer's disease but also be implicated in glucose metabolism. It is proposed that both ApoE3 and ApoE4 brains exhibited decreased levels of  insulin receptor substrates, and glucose transporter 4 (Glut4), meaning that there is decreased glucose uptake. It is not known if deleting APOE gene would lead to decrease in glucose.**

## Question2

1) Briefly state something that is unknown about this system that can be discovered through, and leads to, an experiment.  For example, "It is not known whether....."

**Deletion of APOE gene will lead to hypoglycemia.**

## Question3

3) Make an “if” “then” prediction that is related to item #2. It should be of the general form, “if X is true, then Y should happen”.

**If the mice have APOE knock-out, they will show lower blood sugar level compared with their age-matched, gender-matched wildtype mice.**

## Question4

4) What dependent variable will be observed to test this prediction in item #3? What predictor variable will be used to manipulate the system experimentally? Define the inherent properties of these variables (eg, are they sorted, ordered or measured).

**The dependent variable will be glucose level. Each subject is the experimental unit. The predictor variable is different mice strains, which are APOE KO (knock-out) mice and WT (wild-type) mice. Different mice strain is a discrete, factoral variable that has two levels, KO and WT.**

## Question5

5) Write a statistical hypothesis.  There should be a null and alternate. These should be explicitly consistent with the prediction in item #3 and the response variable in #4. In other words, make sure the statistical hypotheses that you write here serves as a test of the prediction made in item #3.

**The null hypothesis is that the glucose level of KO mice does not differ from or higher than the glucose level of the WT mice. The alternate hypothesis is that the glucose level of KO mice is less than from the glucose level of the WT mice.**

## Question6

6) What is the statistical test you would use to test the hypothesis in item #5? Briefly defend what makes this appropriate for the hypothesis and the experimental variables. If there are alternatives, why is this approach chosen instead? Points will not be awarded if the justification involves something like "because everybody does it this way".

**Wilcoxon Mann Whiteney test will be used for comparing the group means. The null hypothesis will be rejected when p-value is less than 0.05. This is appropriate for the hypothesis is because this test is used to compare two groups that receive either of 2 levels of a predictor variable (in this study it is genetic difference: KO and WT). **

## Question7

7) List the procedures and decision rules you have for executing and interpreting the experiment. These procedures range from selection of experimental units, to randomization to primary endpoint to threshold decisions. Define (and defend) what you believe will be the independent replicate.

**There will be 10 APOE-KO mice and 10 WT mice. All mice are female and around 6 months of age. They will be fasted for about 12 hours (from 9pm to 9am) and their blood glucose will be measured after that. The measurement will be performed in a randomized order (without knowing the mice's genotype). The independent replicate is the independent experimental unit that receives a one of the predictor variable (genotype). Here we have 10 replicates for each of the 2 conditions (but the ideal primary endpoint will be tested in the Monte Carlo analysis below but the final sample size will be based on a power of 90%). Wilcoxon Mann Whiteney test will be used for comparing the group means of glucose level. The decision threshold for type1 error will be 5%. The null hypothesis will be rejected when p-value is less than 0.05.**

## Question8

8) Produce a graph of a simulation for the expected results. Create a dataMaker-like function in R to create and plot the data. Label and scale any axis. The graph should illustrate the magnitude of the expected response, or the level of response that you expect to see and would be minimally scientifically relevant. Be sure to illustrate any variation that is expected.

```{r}
library(tidyverse)
library(gplots)
```

```{r}
#first simulate the dependent variables
KO <- rnorm(n=10, mean=150, sd=18)
WT <- rnorm(n=10, mean=190, sd=18)
sim.set <- tibble(KO,
                  WT)
sim.set

#make a data frame suitable for the formula argument
long <- tibble(replicate_id=1:10, KO, WT) %>%  
  pivot_longer(-replicate_id, 
               names_to="genotype", 
               values_to="glucose")
long

#(I know it's not in the question but would do a) Wilcoxon Mann Whiteney test (unpaired t test) for 2 independent groups
wilcox.test(glucose ~ genotype, 
            data = long, 
            alternative ="less", 
            conf.level=0.95, 
            conf.int=T)

#plot the data
ggplot(long, aes(x=genotype, y=glucose))+
  geom_boxplot(width=0.3)+
  geom_jitter(height=0, 
              width = 0.1, 
              size = 4, 
              alpha=0.4)
```

## Question9

9) Write and perform a Monte Carlo analysis to calculate a sample size necessary to test the hypothesis. This Monte Carlo must test the primary endpoint.

```{r}
nonpara.pwr <- function(n){
  
  m1= 150 
  sd1= 18 
  
  m2= 190 
  sd2= 18 
  
  ssims=100
  
  p.values <- c()
  
  i <- 1
  repeat{
    x=rnorm(n, m1, sd1); # random data generator motifs
    y=rnorm(n, m2, sd2);
    p <- wilcox.test(x, y, 
                paired=F, 
                alternative="less", 
                var.equal=F,
                conf.level=0.95)$p.value
    p.values[i] <- p
    
    if (i==ssims) break
    i = i+1
    pwr <- length(which(p.values<0.05))/ssims
  }
  return(pwr)
  
}

# to test the primary endpoint. the expected sample size is based upon a power of 90%
set.seed(1234)
nonpara.pwr(5)
```

**The sample size will be based upon a power of 90%. This Monte Carlo tells me that the primary end point is 5 mice per group.**

## Question10

10) Write up it all in RMarkdown. Code chunks to illustrate specific points are welcome other than for the Monte Carlo code. Knit and submit and upload the html document by the due data. If it is readable to your best friend, it is readable to us.
