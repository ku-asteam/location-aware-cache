path = readline(prompt="Enter a file path: ")
hot_spot = as.integer(readline(prompt="Enter a number of cluster: "))
TS = as.integer(readline(prompt="Enter a number of time slot: "))
intra_p = as.integer(readline(prompt="Enter a probability of outgoing: "))

data<-read.csv(path, header=TRUE)
tr<- data
tr<-tr[,-c(1)]
tr[tr$user==1,]

# Using acast to convert above data as follows:
#       m1  m2   m3   m4
# u1    3   4    2    5
# u2    1   6    5
# u3    4   4    2    5
g<-acast(tr, user ~ movie)
R<-as.matrix(g)

s_Rucol<- matrix(s_Rucol <- R, nrow = dim(R)[1], ncol = dim(R)[2]) 
R <- s_Rucol

train <- as(s_Rucol, "realRatingMatrix")

# Prediction part
model_u1 = Recommender(train, method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
recom_u_p <- predict(model_u1, train, type="ratings")  # Predicted values only
recom_u_a <- predict(model_u1, train, type="ratings") # All rating

train <- as(train, "matrix")
recom_u_a <- as(recom_u_a, "matrix")
recom_u_a1 <- matrix(0, nrow = dim(train)[1], ncol = dim(train)[2]) # recom_u_a1 = recom_u_a & train merge
for (row in 1:dim(train)[1]) {
  for (col in 1:dim(train)[2]){
    if (is.na(recom_u_a[row, col])) {
      recom_u_a1[row, col] <- train[row, col]
    }
    else {
      recom_u_a1[row, col] <- recom_u_a[row, col]
    }}}
recom_u_a <- as(recom_u_a1, "matrix")

# Point to hot spot
hot_spot_x <- c(sample(x = 20:180, size = hot_spot))
hot_spot_y <- c(sample(x = 20:180, size = hot_spot))
hot_spot_location <- matrix(0, nrow = hot_spot, ncol = 2, byrow = TRUE)
for (z in 1:length(hot_spot_x)) {
  hot_spot_location[z, 1] = hot_spot_x[z]
  hot_spot_location[z, 2] = hot_spot_y[z]
}

# Select Rep User
User_list = c(1:dim(s_Rucol)[1]) 
rep_User <- c(sample(x = 1:dim(s_Rucol)[1], size = 1))

# Select Remind Rep User
for (k in 1:(hot_spot - 1)) {
  sim_vector <- c()
  rem_User <- User_list[!User_list %in% rep_User]
  for (z in 1:length(rem_User)){
    sim_sum = 0
    for (r in 1: length(rep_User)) {
      i <- !(is.na(train[rem_User[z],]) | is.na(train[rep_User[r],])) # Exclude na to calculate similarity
      if (10 == length(i[i==FALSE])){
        sim_of_user = 0
      } else {
        sim_of_user = simil(list(train[rem_User[z],][i], train[rep_User[r],][i]), method="euclidean")
      }
      sim_sum = sim_sum + sim_of_user * sqrt(length(train) / length(train[i]))
    }
    sim_vector <- c(sim_vector, sim_sum)
  }
  low_correl_user = rem_User[which.min(sim_vector)]
  rep_User <- c(rep_User, low_correl_user)
}

# Remainder User Distribute
rem_User <- User_list [! User_list %in% rep_User] 
Cluster_matrix <- matrix(0, nrow = length(rep_User), ncol = (dim(s_Rucol)[1]%/%hot_spot)-1)
Cluster_matrix <- cbind(rep_User, Cluster_matrix)

for (z in 1:length(rem_User)){ # Remainder
  sim_vector <- c()
  for (r in 1:length(rep_User)) { # Representer
    i <- !(is.na(train[rem_User[z],]) | is.na(train[rep_User[r],])) # Exclude na to calculate similarity
    if (10 == length(i[i==FALSE])){
      sim_of_user = 0
    } else {
      sim_of_user = simil(list(train[rem_User[z],][i], train[rep_User[r],][i]), method="euclidean")
    }
    sim_sum = sim_sum + sim_of_user * sqrt(length(train) / length(train[i]))
    sim_vector <- c(sim_vector, sim_of_user)
  }
  
  for (c in 1:length(rep_User)) {
    similar_cluster = which.max(sim_vector)
    empty_indexes = which(Cluster_matrix[similar_cluster,] == 0, arr.ind = FALSE)
    if (length(empty_indexes) != 0) {
      Cluster_matrix[similar_cluster, empty_indexes[1]] <- rem_User[z]
      break
    } 
    sim_vector[similar_cluster] <- -1
  }}
print(paste0("Cluster's user: ", length(Cluster_matrix[1,])))

# Variable
num_cluster = hot_spot
User_list = sort(sample(1:dim(s_Rucol)[1]))
Movie_list = sort(sample(1:dim(s_Rucol)[2]))
N = length(User_list)

side_length = 200
area = side_length * side_length
cluster_length = 20/2
user_step = 5
C_D = 5

# User Moblility   
location <- matrix(, nrow = 0, ncol = 4, byrow = TRUE) # user id + time slot + x-axis + y-axis
for (a in 1:hot_spot) {
  hot_spot_user = length(Cluster_matrix[a,]) - length(which(Cluster_matrix[a,] == 0)) # Except 0, # of user
  add_user_x = hot_spot_location[a, 1] + floor(runif(hot_spot_user, -cluster_length, cluster_length)) # Users initial placement
  add_user_y = hot_spot_location[a, 2] + floor(runif(hot_spot_user, -cluster_length, cluster_length)) # a is home cluster
  for (d in 1:length(add_user_x)) { # Each user movement
    location = rbind(location, c(Cluster_matrix[a, d], 1, add_user_x[d], add_user_y[d])) # Add initial placement (TS = 1)
    for (ts_index in 2:TS) {
      if (hot_spot_location[a, 1] - cluster_length <= tail(location, 1)[3] && 
          hot_spot_location[a, 1] + cluster_length >= tail(location, 1)[3] && 
          hot_spot_location[a, 2] - cluster_length <= tail(location, 1)[4] && 
          hot_spot_location[a, 2] + cluster_length >= tail(location, 1)[4]) { # If user in cluster
        if (floor(runif(1, 1, 101)) <= intra_p){
          # 80% stay home cluster
          repeat {
            c_x = tail(location, 1)[3]+ floor(runif(1, -user_step, user_step))
            c_y = tail(location, 1)[4]+ floor(runif(1, -user_step, user_step))
            if (hot_spot_location[a, 1] - cluster_length <= c_x && 
                hot_spot_location[a, 1] + cluster_length >= c_x &&  
                hot_spot_location[a, 2] - cluster_length <= c_y && 
                hot_spot_location[a, 2] + cluster_length >= c_y && 
                0 <= c_x && c_x <= side_length &&  0 <= c_y && c_y <= side_length)
              break
          }
          location = rbind(location, c(Cluster_matrix[a, d], ts_index, c_x, c_y))
        } else {
          # 20% stay another cluster
          repeat {
            z = floor(runif(1, 1, hot_spot + 1))
            if (a != (floor(runif(1, 1, hot_spot + 1))))
              break
          }
          # z is another cluster
          c_x = hot_spot_location[z, 1]+ floor(runif(1, -cluster_length, cluster_length))
          c_y = hot_spot_location[z, 2]+ floor(runif(1, -cluster_length, cluster_length))
          location = rbind(location, c(Cluster_matrix[a, d], ts_index, c_x, c_y))
        }} else { # If away cluster
          c_x = hot_spot_location[a, 1]+ floor(runif(1, -cluster_length, cluster_length))
          c_y = hot_spot_location[a, 2]+ floor(runif(1, -cluster_length, cluster_length))
          location = rbind(location, c(Cluster_matrix[a, d], ts_index, c_x, c_y))
        }}}}

matsplitter<-function(M, r, c) {
  rg <- (row(M)-1)%/%r+1
  cg <- (col(M)-1)%/%c+1
  rci <- (rg-1)*max(cg) + cg
  N <- prod(dim(M))/r/c
  cv <- unlist(lapply(1:N, function(x) M[rci==x]))
  dim(cv)<-c(r,c,N)
  cv
} 
user_location = matsplitter(location, TS, 4)

dist <- matrix(0, nrow=N*N*TS, ncol = 5, byrow = TRUE ) # user1 id + user2 id  + time slot + dist + contact
dc=1
for(uc1 in 1:N){
  temp_loc1 <- location[location[,1]==uc1,]
  for(uc2 in 1:N){
    temp_loc2 <- location[location[,1]==uc2,]
    for(tsc in 1:TS){
      xu1= temp_loc1[tsc,3]
      xu2= temp_loc2[tsc,3]
      yu1= temp_loc1[tsc,4]
      yu2= temp_loc2[tsc,4]
      
      dist[dc,1]= uc1    # User1 index
      dist[dc,2]= uc2    # User2 index
      dist[dc,3]= tsc   # Time slot index
      dist[dc,4]= ((xu2-xu1)^2 + (yu2-yu1)^2)^0.5
      if(dist[dc,4]<=C_D){
        dist[dc,5]=1
      }else {dist[dc,5]=0}
      dc=dc+1
    }}}

con_p <- matrix(0, nrow=N, ncol = N, byrow = TRUE ) # Contact probability
for(uc1 in 1:N){
  temp_dis <- dist[dist[,1]==uc1,]
  for(uc2 in 1:N){
    con_p[uc1,uc2]= sum(temp_dis[temp_dis[,2]==uc2,5])/TS
  }}
P_uiuj <- con_p

# Parameter Learning
u_size = length(User_list)
f_size = length(Movie_list)
s_data_u1a<- as(recom_u_a, "matrix")[User_list, Movie_list]
s_data_u1a[s_data_u1a<0]=0
s_data_u1a[is.na(s_data_u1a)] <- 0
s_data_u1p<- as(recom_u_p, "matrix")[User_list, Movie_list]

rat_u_all <- s_data_u1a
rat_u_par <- s_data_u1p
p_u_u <- c()
p_u_f <- c()
p_u_fu <- matrix(, nrow = u_size, ncol = f_size)

for (u in 1:u_size) {
  for (f in 1:f_size) {
    p_u_fu[u,f] = rat_u_all[u,f]/sum(rat_u_all[u,]) # User preference
    p_u_f[f] = sum(rat_u_all[,f])/sum(rat_u_all) # File popularity
  }
  p_u_u[u]= sum(is.na(rat_u_par[u,])) / sum(is.na(rat_u_par)) # User activity level
}
p_u_fu[is.na(p_u_fu)] <- 0

# Content Placement
tic()
greedy_hit <- c()  # Hit ratio greedy
Cont_placement <- matrix(0, nrow=N, ncol = f_size, byrow = TRUE )  # Caching startegy
cache_size = 2

for (k in 1:num_cluster) { # k is Cluster Id
  Cluster_User_list <- c()
  for (j in 1:(length(Cluster_matrix[k,]) - length(which(Cluster_matrix[k,] == 0)))) {
    Cluster_User_list <- c(Cluster_User_list, Cluster_matrix[k,j])
  }
  u_size = length(Cluster_User_list)
  # Cluster user's movie preference
  Cluster_p_u_fu <- matrix(, nrow = u_size, ncol = f_size)
  for (user_p in 1:u_size){
    for (movie_p in 1:f_size){
      Cluster_p_u_fu[user_p, movie_p] = p_u_fu[Cluster_User_list[user_p],movie_p]
    }}
  
  # Cluster user's activity level
  Cluster_p_u_u <- c()
  for (user_a in 1:u_size) {
    Cluster_p_u_u[user_a] = p_u_u[Cluster_User_list[user_a]]
  }
  
  # Cluster user's contact probability
  Cluster_P_uiuj <- matrix(, nrow = u_size, ncol = u_size)
  for (i in 1:u_size) {
    for (j in 1:u_size) {
      Cluster_P_uiuj[i,j] = P_uiuj[Cluster_matrix[k,i], Cluster_matrix[k,j]]
    }}
  

  # Greedy method
  c_uf_pg <- matrix(0, nrow=u_size, ncol = f_size, byrow = TRUE )  # Caching startegy
  c_temp_pg <- matrix(0, nrow=u_size, ncol = f_size, byrow = TRUE )  # Temporary caching startegy
  file_pointer = 1
  Hit_rp_pg = 0 # Proposed 
  
  while(file_pointer <= cache_size * u_size){
    max_pair_user_pg = 0
    max_pair_file_pg = 0
    max_hit_rp_pg = Hit_rp_pg
    unassigned_pair_pg <- as.matrix(which(c_uf_pg == 0, arr.ind = TRUE))
    for (pc in 1:dim(unassigned_pair_pg)[1]) {   # Loop to check each unassigned pair
      c_temp_pg <-  c_uf_pg
      user_pg = as.numeric(unassigned_pair_pg[pc, 1])
      file_pg = as.numeric(unassigned_pair_pg[pc, 2])
      if (sum(c_temp_pg[user_pg,]) < cache_size ) { # Check storage condition+ already assign condition &&c_temp[user,file]==0
        c_temp_pg[user_pg,file_pg] = 1
      }
      # Start hit_ratio after new assignment
      if (c_temp_pg[user_pg, file_pg] == 1){
        a_Hit_rp_pg = 0 # Proposed 
        for (ui in 1:u_size) {
          for (f in 1:f_size) {
            if (is.nan(p_u_fu[ui,f])) {
              a_Hit_rp_pg = a_Hit_rp_pg + 0
            } else {
              a_Hit_rp_pg = a_Hit_rp_pg + Cluster_p_u_u[ui] * Cluster_p_u_fu[ui,f] * (1-prod(1-c_temp_pg[,f] * Cluster_P_uiuj[,ui]))
            }}}}
      else {a_Hit_rp_pg = max_hit_rp_pg}
      # End: hit_ratio after new assignment
      if (a_Hit_rp_pg > max_hit_rp_pg) { # Check hit ratio increment
        c_temp_pg[user_pg, file_pg] = 1
        max_pair_user_pg = user_pg
        max_pair_file_pg = file_pg
        max_hit_rp_pg = a_Hit_rp_pg
      }}
    c_uf_pg[max_pair_user_pg, max_pair_file_pg] = 1 # Add best pair in the caching strategy permenanlty  
    Cont_placement[Cluster_matrix[k, max_pair_user_pg], max_pair_file_pg] = 1
    Hit_rp_pg = max_hit_rp_pg
    file_pointer = file_pointer + 1
  }
  greedy_hit[k] <- max_hit_rp_pg
}


# Validation
average_Hit_ratio = 0
for(i in 1:N) {
  dist <- matrix(0, nrow=N*N*TS, ncol = 5, byrow = TRUE ) # user1 id + user2 id  + time slot + dist + contact
  dc=1
  for (user_p in 1:N){
    cached_content = which(Cont_placement[user_p,] == 1)
    for (k in 1:cache_size){
      rat_u_all[user_p,][cached_content[k]]= 0
    }
    req_cont_vector = which(rat_u_all[user_p,] == max(rat_u_all[user_p,]))
    req_cont = sample(req_cont_vector, 1, replace = TRUE) 
    temp_loc1 <- location[location[,1]==user_p,]
    for (user_p2 in 1:N) {
      if (Cont_placement[user_p2,req_cont] == 1 ) {
        temp_loc2 <- location[location[,1]==user_p2,]
        for(tsc in 1:TS){
          xu1= temp_loc1[tsc,3]
          xu2= temp_loc2[tsc,3]
          yu1= temp_loc1[tsc,4]
          yu2= temp_loc2[tsc,4]
          
          dist[dc,1]= user_p    # user1 index
          dist[dc,2]= user_p2    # user2 index
          dist[dc,3]= tsc   # timeslot index
          dist[dc,4]= ((xu2-xu1)^2 + (yu2-yu1)^2)^0.5
          if(dist[dc,4]<=C_D) {
            dist[dc,5]=1
          } else {
            dist[dc,5]=0
          }
          dc=dc+1
        }}}}
  
  com_matrix = dist[dist[,5] == 1,]
  Hit = 0
  for (k in 1:N) {
    temp = com_matrix[com_matrix[,1] == k,]
    if (length(temp) == C_D) {
      Hit = Hit + length(unique(temp[3]))
    } else {
      Hit = Hit + length(unique(temp[,3]))
    }}
  average_Hit_ratio_temp = Hit/(length(User_list)*TS)
  average_Hit_ratio = average_Hit_ratio + average_Hit_ratio_temp
}
print(average_Hit_ratio/N)

toc()