- 👋 Hi, I’m @amcnally89
- Below you will find the code that I am attempting to use for the R Capstone Project. It appears that there is an warning with the test_results_all predict. But the main issue that I am having is with the RSQ and RSME functions.

train_fit_all %>%
     mutate(name = fct_reorder(labels(train_fit_all$fit$coefficients), train_fit_all$fit$coefficients)) %>%
     ggplot( aes(x=fit, y=coefficients)) +
     geom_bar()


<!---
amcnally89/amcnally89 is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->
