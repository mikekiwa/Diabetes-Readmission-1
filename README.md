# Diabetes-Readmission

### Objective
- Understand trends among patients getting readmitted for diabetes in US and predict one's readmission rate given his/her demographics and medical history. 
- Build a visual application using R Shiny, targetted towards patients, hospitals and medical practitioners, that depicts these trends brokedn down by differnt parametes. They should also be able to input values for certain parameters and calculate their probability fo readmission

### Data
The dataset represents 10 years (1999-2008) of clinical care at 130 US hospitals and integrated delivery networks. It includes over 50 features representing patient and hospital outcomes. You can find it [here](https://archive.ics.uci.edu/ml/datasets/Diabetes+130-US+hospitals+for+years+1999-2008)

### Inference
Chances of readmission are higher in following cases:
- When a patient is discharged to a hospice facility or a SNF (Skilled Nursing facility)
- When a patient has too many inpatient visits or emergencies in the past year, or large number of procedures
- When the age of patient is above 70
- When secondary diagnosis is either Neoplasms or endocrine, nutritional and metabolic diseases, and immunity disorders
- When primary diagnosis is diseases of the skin and subcutaneous tissue
- When patient is admitted from Emergency Room

*Note* - The ShinyVisualSystem.R is the visual app created using R Shiny.
