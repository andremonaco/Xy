[![CircleCI](https://circleci.com/gh/andrebleier/Xy.svg?style=svg)](https://circleci.com/gh/andrebleier/Xy)

Simulating Supervised Learning Data <img src="/img/Xy.png" alt="drawing" width="150px" align="right"/> 
===================================



With `Xy()` you can convienently simulate supervised learning data. The simulation can be
very specific, since there are many degrees of freedom for the user. For instance,
the functional shape of the nonlinearity is user-defined as well. Interactions can be formed and (co)variances altered. For a more specific motivation you can visit our [blog](https://www.statworx.com/de/blog/simulating-regression-data-with-xy/)

## Usage

The usage is pretty straight forward. I strongly encourage you to read the help document to explore all functionalities.

### Install

Install the package with <code>devtools</code>:

```
# install.packages("devtools") 
# get it from github
devtools::install_github("andrebleier/Xy")            
```

### Simulate data 

You can simulate regression and classification data with interactions and a user-specified non-linearity. With the <code>stn</code> argument you can alter the signal to noise ratio of your simulation. I strongly encourage you to read this [blog post](https://www.statworx.com/de/blog/pushing-ordinary-least-squares-to-the-limit-with-xy/), where I've analyzed OLS coefficients with different signal to noise ratios.

```
# load the library
library(Xy)
# simulate regression data
my_sim <- Xy(n = 1000, 
             numvars = c(10,10), 
             catvars = c(3, 2), 
             noisevars = 50, 
             task = Xy_task(), 
             nlfun = function(x) x^2, 
             interactions = 1, 
             sig = c(1,4),  
             cor = c(0), 
             weights = c(-10,10), 
             intercept = TRUE, 
             stn = 4)
```

### Feature Selection

You can extract a feature importance of your simulation. For instance, to benchmark feature selection algorithms. You can read up on a small benchmark I did with this feature on our [blog](https://www.statworx.com/de/blog/benchmarking-feature-selection-algorithms-with-xy/).

```
# Feature Importance 
fs_varimp <- varimp(my_sim, plot = TRUE)
```
<img src="/img/imp.png" alt="drawing"/> 

Feel free to [contact](mailto:andre.bleier@statworx.com) me with input, ideas or some dank memes.

