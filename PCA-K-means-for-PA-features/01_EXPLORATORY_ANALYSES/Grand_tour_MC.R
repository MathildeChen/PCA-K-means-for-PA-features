library(tourr)

f <- flea[, 1:6]
# Most of the tour methods assume that you are working on a matrix or data frame of p
#continuous variables. By default these variables are scaled to each have range [0; 1], but if
#your variables are measured on a common scale already, you can turn this off by setting
#rescale = FALSE.

# Ouvre un nouveau plot et lance la commande
# Stop le tour avec Echap
animate(f,
        tour_path = grand_tour(),
        display = display_xy()
)

# Differentes presentations
# 1-D with density
animate(f, grand_tour(d = 1),
        display = display_dist()) # Mais qu'est ce qui est représenté par la densité? 
# 2-D tour displayed with a scatterplot
animate(f, tour_path = grand_tour(d = 2),
        display = display_xy())
# 3-D tour displayed with simulated depth
animate(f, tour_path = grand_tour(d = 3), 
        display = display_depth())
# 4-D tour displayed with a parallel coordinates plot
animate(f, tour_path = grand_tour(d = 4), 
        display = display_pcp())

# Possible aussi de changer le tour path (movement through space)
# default: grand_tour: picks a new p * d projection matrix at random

# guided_tour: instead of picking a new projection completely at random, 
# we pick one that is more interesting
# guided_tour(holes()): guides the tour towards projections where there is a "hole" in the center of the distribution
# Un tour qui maximise la dispersion des donnees 
animate(f, tour_path = guided_tour(holes()),
        display = display_xy())
# is equivalent to
animate_xy(f, guided_tour(index = holes))

# little_tour(): provides a smooth sequence between views of 
# all d-dimensional sets of variables in the data
# if d= 2, it steers the views through every pair of variables
animate_xy(f, little_tour(d = 2))
animate_xy(f, little_tour(d = 3))


animate_xy(f, dependence_tour(c(1, 1, 1, 2, 2, 2))) 
# dependence_tour(c(1, 2, 1, 2, 1, 2)): specifies that the first 3 
# variables will be displayed with a 1-D tour on the first axis, 
# and the second 3 with a 1-D tour on the second axis
# --> this version (2 axis) = correlation tour = analogous to PCA

# Ce que ca donne quand tu connais deja les clusters existants
animate(f,
        tour_path = guided_tour(holes()),
        display = display_xy(col = flea$species))

# Idem mais en regardant la distribution de la projection
animate(f,
        tour_path = guided_tour(holes()),
        display = display_dist()
)

# Explications du package
# Article de Jstatsoft: https://www.jstatsoft.org/article/view/v040i02
# Vignette: https://cran.r-project.org/web/packages/tourr/vignettes/tourr.html