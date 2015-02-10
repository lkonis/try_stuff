#cmp_n=0
#t <- read.table("http://spark-public.s3.amazonaws.com/algo1/programming_prob/QuickSort.txt")
#t <- t$V1
subt <- function(t, piv=1, cmp_n=0)
{  
  if (length(t) == 1)
  {
    return(t)
    #return(cmp_n)
  }
  if (length(t) == 2)
  {
    if (t[1]>t[2])
    {
      return (t[2:1])
    }    
    else
    {
      return (t)
    }
    #return(cmp_n+1)
  }
  
  if (piv>(length(t)-1))
    piv=length(t)
  
  cmp_n <- cmp_n + (length(t) - 1)
  tmp <- t[piv]
  t[piv] <- t[1]
  t[1] <- tmp
  # call partition, get same array (smaller by 1) partitioned
  # get pointer limit between 'smaller' 'greater'
  t <- part(t)
  split_ind=as.numeric(t[2])
  t<-unlist(t[1])
  if (split_ind==1)
    t_small=NULL
  else
  {
    t_small = t[1:(split_ind-1)]
    # call recursion
    t_small = subt(t_small, piv)
  }
  if (split_ind==length(t))
    t_greater = NULL
  else
  {
    t_greater = t[(split_ind+1):length(t)]
    t_greater = subt(t_greater,piv)
  }
  t = c(t_small,t[split_ind],t_greater)
#  print (t)
  
}

# this function assumes first element is pivot
part <- function(t)
{
  piv_ind=1
  unpart_ind=2
  t1=t[1]
  for (unpart_ind in 2:length(t))
  {
    
    if (t[unpart_ind]<t1){
      #swap
      tmp = t[unpart_ind]
      t[unpart_ind] = t[piv_ind+1]
      t[piv_ind+1] = tmp
      # advance indeces
      piv_ind <- piv_ind+1
    }
    else { # new is > t1
      # actually do nothing
    }
    
  }
  ind1=2 # start of part1
  ind2=piv_ind # start of part 2
  # put back piv in its place
  tmp = t[piv_ind]
  t[piv_ind]=t[1]
  t[1]=tmp
  return(list(t,piv_ind))
}