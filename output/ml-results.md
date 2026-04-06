# Reflections

Sydney Sauer

## What was your final model and why did you choose it over alternatives?

I chose a model with all of the included variables (with a few basic transformations, such as standardizing age, height, and weight, and setting invalid hours to NA) and a few of my own engineered features. I regularized the model with lasso (lambda=10). I experimented using grid search to find the optimal lambda and found that 10 (which is where I started!) happened to be optimal. I used this model to make predictions for all nonmissing rows, then I filled in rows with missing values using the overall arrest probability of the training data's missing rows. I filled in the missing rows this way because I noticed that rows with missing data had a much lower arrest probability than non-missing rows, so I wanted to capture that variation.

I found that this model, when assessed using cross-validation, produced the lowest out of sample log loss of any approach that I tried. This was surprising, because I thought my more complex models--such as one that included race interactions--would perform better, but they did not. I was also surprised that all of my models performed relatively similarly. The baseline out of sample log loss (ie, from a model that is just the intercept) was around 0.23 for all non-missing entries. My approaches ranged from 0.210 to 0.209. This is an improvement, but not by much.

## How predictable are stop outcomes from the information available at the time of the stop? What does this tell us about policing?

My difficulty improving on the intercept-only model shows me that police stop outcomes are hard to predict from data about the stop. It makes me wonder if attributes of the police officer might be more predictive. To be fair, I could have merged additional data, such as Census data, to see if socioeconomic factors play a role. But I think it still says a lot that the information about the stop--which feels like it should be the only information that matters when making an arrest--is not easily used to predict outcomes.

## Should police departments use predictive models like this? What are the risks?

In my opinion, these predictive models are trouble for justice departments. What they might bring in terms of efficiency, they lose with their potential for false positives. As we talked about in class, we'd hopefully want this model to be as conservative as possible to minimize the risk of harm to an innocent person. Of course, someone else might prefer false positives over false negatives for the sake of community safety. However, I think if the goal is community safety, there are probably better programs that could be implemented (such as education, afterschool activities, and vocational training) with the funds that would be used to build and maintain a model that tries to do police officers' jobs for them.