

# Copy vector of letters (vec1), but substitute "x" for previous letter.
# Letters before "x" should be reduplicated in vec2.

vec1 = c("s","w","x","e","x","r","t","f","x","l")

#### solution 1 ###
vec2 = c()
counter = 1
for (i in vec1){
  if (i == "x"){
    vec2[counter] = vec1[counter-1]
  }
  else if (i != "x"){
    vec2[counter] = i
  }
  counter = counter + 1
}

#check
cbind(vec1,vec2)

### solution2 ###
library(data.table)
vec2 = ifelse(vec1=="x", shift(vec1), vec1)

#check
cbind(vec1,vec2)
