

df1 <-data.frame(coeff=c('Intercept','pC','CN'), RDA1_coeff=c(0.5,2,1), RDA2_coeff=c(0.5,2,1))
df1<-df1[-1]  %>% t() %>% as.data.frame()%>% setNames(df1[,1])
df1


df2 <- data.frame(lieu=c('lieu1','lieu2', 'lieu3'), RDA1=c("x1",'x2','x3'), pC=c(1,2,3), CN=c(3,7,9))

df2




cols<-intersect(names(df1), names(df2)) #select all factors coefficients 








df2 %>% 
  
  #1. Multiplication of coefficient * values
  mutate(
    across( #across : apply function to multiple columns
      all_of(cols), #were to apply it
      ~ .x * as.numeric(df1[['RDA1_coeff',cur_column()]]))) %>% #which function. Here multiply the column by the coefficient factor
  
  #2. Sum of coefficient and add intercept
  mutate(y1_est=rowSums(across(all_of(cols)))) %>%
  mutate(y1_est, y1_est=y1_est + as.numeric(df1[['RDA1_coeff', 'Intercept']])) %>%
  
  #3. Select to have only RDA1, y1_est
  select(RDA1,y1_est)


         