# Computational-Model-for-BRAT

## Abstract
By inverting the design of the BART task, Balloon Risk Aversion Task attempts to isolate the avoidance motivation and explore its uniqueness compared to risk taking (approach motivation). Through this project, we explore the computational evidence for whether the change in the task’s design has succeeded in changing the cognitive processes of the subject. Two criteria are used: 1) Do the subjects measure risk based on how much risk they avoided or took? 2) Do the subjects view the situation as loss-only context? Through fitting of four different model designs using hierarchal Bayesian inference and MLE (Maximum Likelihood Estimation), we were able to find computational evidence for change in subjects’ method of calculating risk but no evidence that they view loss from the task. While the best model showed big difference in LOOIC, AIC, and BIC, it showed poor performance in parameter recovery. Improvements in model design concerning how subjects learn during BRAT task seems necessary.

## Motivation for a new behavioral task
_“Admittedly, most if not all, behavioral tasks aimed at understanding risk-taking or risk-aversion can have both approach and avoidance motivation.” (Crowley et al., under review) Risk Taking vs Risk Aversion_

Balloon Risk Aversion Task is an attempt to isolate the avoidance and approach motivation in the conventioinal Balloon Analog Risk Task (BART). Locating a suitable cognitive model of BRAT will provide further insight into how we understand risk.

Here, we compare 4 different models for the Balloon Risk Aversion Task (BRAT), a variation of the Balloon Analog Risk Task (BART).
All the models were fitted through both hierarchal Bayesian Inference and Maximum Likelihood Estimation. Files related to MLE are located in the MLE folder. All the fitted samples in either rds or RData format can be found in a link in the presentation explaining reproducibility. Model 2 showed to be best model out of the four.

## Discussion

Looking at the parameter recovery result done with MLE, it is clear that φ and η, parameters related to learning of the participant, are having difficulty being recovered. This seems due to the fact that with BRAT, the participant does not get the information when the explosion point for the balloon. In non-automatic BART,
if the participant pumps for n times and pops, success will still increase in value of n - 1. However, for BRAT, because the participant does not get any information, no
addition will be made. Therefore, the impact of a single loss is enlarged. For example,
if participant’s φ and η is 0.9 and 1 respectively and pops the balloon on the first trial with 64 pumps (half of what was given), the expected change rate of risk immediately
drops to 0.014. Due to this aspect of design, the impact of φ is abnormally reduced. I suspect this aspect of the model to be the crucial reason for failure in recovery.

## Conclusion

Out of the four models under investigation, Model 2 —which assumed a participant 1) measures the risk based on how much he decided to avoid, rather than to take and 2) views the task as gain-only situation — showed the lowest LOOIC score, showing a 3 standard deviation difference with the next competitive model. When fitted using MLE and compared with BIC and AIC, Model 2 consistently showed better performance.
However, Model 2 showed very poor parameter recovery. Especially when fitted with Bayesian Hierarchal structure, the recovered parameter showed a sign of dramatic shrinkage, failing recovery. It performed relatively better with MLE
parameter recovery, although φ and η continuously was not recovered well. The learning aspect of the model seems to be in need of improvement.
