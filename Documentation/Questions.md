# Questions


##### Where should the user be able to choose the amount of burn in? For each plot specifically?




##### Regarding the burn ins: Shouldn't they also influence the results of the model such as R2 and values for state components and not just the plot?

-> in "summary.bsts.R" it states that burn in is specified automatically




##### Is there a way to get the contribution of each component as a number? For now it is only a general R^2








# To Do

- look at https://stackoverflow.com/questions/51274760/how-to-extract-inclusion-probabilities-from-bsts-r-package

### Errors
- [ ] check if existing Error work
- [ ] discuss which errors to includes
- [ ] include Error when manual burn ins exceed actual MCMC draws
- [ ] include error that doesn't calculate model when no components or regressord are included


### Models
- [x] check in ".bstsResultsHelper" whether we have to specify sigma prior or if this can happen by itself -> is usually NULL by default
- [ ] implement regression + give formula as output (https://win-vector.com/2018/09/01/r-tip-how-to-pass-a-formula-to-lm/)
- [ ] write function and qml section for individual priors and connect it to ".bstsResultsHelper"
- [ ] add all important components mentioned in proposal



### Plots
- [ ] try further to bring original bsts plots into ggplot format
- [ ] implement option for actual time series postxi

### Tables
- [ ] need to add burn in to summary function, right now has all samples


### Problems



-> sdpriors

-> capitalisation

-> residual written wrong
-> spell out post dist.
-> capitalise captions and rest only first
-> look if prdictors can be emulated and

-> mcmc options into advances options
-> put distribution into model components
-> maybe

-> what makes sense, do people want it, how should package look

-> for each regressor make possible to always add
-> not automatic burn in but just proportion

-> add a box for factors like linear regression

-> compute r sqaure for each individual component of bayes factor for each



# Bayesian models
- stand code or something in stand for state space models
-> first ar process and then make it latent
- estimate AR process in STAN - start from there
_ bridge sampling
- first without hidden state
-> then add state but no state noise
-> test whether existing code does model AR process existing

https://jrnold.github.io/ssmodels-in-stan/filtering-and-smoothing.html

- goal: use stan or JAGS to model AR1 and then use it to make predictions
