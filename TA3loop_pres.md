TA3 Age Estimation method: Shiny app replicate
========================================================
author: jgalsku
date: 8/7/2021
autosize: true

About the original TA3 Age Estimation software
========================================================

This Shiny app is based on the TA3 Age Estimation software (beta version) created by Ron Richardson and Stephen Ousley which can be found [here](https://www.statsmachine.net/software/TA3/). Here is some information to contextualize the TA3 method:
- It was made by Biological Anthropologists to estimate the age-at-death of unidentified human remains.
- The TA in TA3 stands for [Transition Analysis](https://www.researchgate.net/publication/240359583_Transition_Analysis_A_New_Method_for_Estimating_Age_from_Skeletons), a method originally based on logistic regression and Bayesian probabilities.
- The new TA3 method uses machine learning, specifically a random GLM model to estimate age-at-death from skeletal traits using data from other individuals with known-age.


Constructing the Shiny app: Part I
========================================================
The original code from [Ron Richardson's GitHub page](https://github.com/rer145/ta3) was modified and used in the creation of this Shiny app.   

Because there are 121 skeletal traits that can be evaluated for the TA3 method, I wanted to find a way to set these many radioButtons in the Shiny UI without having to manually set them. For this, I used a lapply loop that depended on information from a CSV file. Here is the head of that CSV file:


```
           TraitDBName                  TraitText   0    1 2 3 num cols
1 parietal_depressionL Parietal depression (left) abs pres       1    4
```

Constructing the Shiny app: Part II
========================================================
The next challenge was retrieving those radioButton input values to be able to use them in the random GLM model. This was also done using a lapply loop.


```r
    fieldsAll <- c("IDInput", "indDegInput", "recorderInput", "obsDateInput", "noteInput", 
                   lapply(1:121, function(i) {
                     paste0("radioInput", i)  }))
    list_res <- lapply(fieldsAll, function(x) input[[x]])
```

Future features I would like to add
========================================================
I myself am a Biological Anthropologist very interested on what the TA3 method can achieve. I set out to replicate what the TA3 software does in a quest to understand how the method works.  
This is just the beginning with this Shiny app as I would like to add the following features to it:

- Allow the user to submit more than one case and then estimate their age using a loop.
- Allow the user to input multiple cases by uploading a CSV file with a specific format.
- Improve the method by evaluating other machine learning algorithms!

[Here](https://github.com/jgalsku/TA3loop) is the code for the Shiny app. [Here](https://jgalsku.shinyapps.io/TA3loop/) is my app.



