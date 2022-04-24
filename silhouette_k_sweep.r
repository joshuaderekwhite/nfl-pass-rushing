# define a function to get the silhouette across different k
# Extracted from: Mitchell Hill. kmeans gmm.rmd. https://webcourses.ucf.edu/files/92680364/, 
# Apr 2022. sil sweep kmeans function.
silhouette_k_sweep = function(cluster_fn, df, max_k=10){
    require(cluster)
    sil_scores = c()
    loss_scores = c()
    for(k in 2:max_k){
        # build cluster model
        g_k = cluster_fn(df, k)
        # get the loss for given value of k (requires different code for different cluster_fn)
        if(identical(cluster_fn, kmeans)){
        loss_scores[k-1] = g_k$tot.withinss
        } else if(identical(cluster_fn, pam)){
        loss_scores[k-1] = g_k$objective[1]
        }
        # get the silhoutte scores for all samples and average
        sil_scores[k-1] = mean(silhouette(g_k$cluster, dist(df))[,3])
    }
    # find the optimal k by find the max of the silhoutte scores
    opt_k_sil = which.max(sil_scores) + 1
    return(list(loss_scores=loss_scores, sil_scores=sil_scores, opt_k_sil=opt_k_sil))
}