library('readr')
library('stringr')
library('hash')

split_inside = function(i){
  t = str_split(str_replace_all(i, '[[:punct:]]', ""), ' ')[[1]]
  return(t[t != ""])
}


hash_count = function(split){
  count = hash()
  for (i in split){
    if (is.null(count[[i]])){
      count[[i]] = 1
    }
    else{
      count[[i]] = count[[i]] + 1
    }
  }
  return(count)
}


str_in = function(x, pattern, logic = NULL, switch = FALSE) {
  count = c()
  x = tolower(x)
  pattern = tolower(pattern)
  for (k in pattern) {
    # append result
    if (switch)
      cnt <- c(cnt, !sjmisc::is_empty(grep(x, k, fixed = T)))
    else
      cnt <- c(cnt, !sjmisc::is_empty(grep(k, x, fixed = T)))
  }
  # which logical combination?
  if (is.null(logic))
    return(cnt)
  else if (logic %in% c("or", "OR", "|"))
    return(any(cnt))
  else if (logic %in% c("and", "AND", "&"))
    return(all(cnt))
  else if (logic %in% c("not", "NOT", "!"))
    return(!any(cnt))
  return(cnt)
}


get_second = function(i){
  return(str_split(i, ' ')[[1]][[2]])
}


# challenge 1
path = getwd()
if (path != 'E:/class/statistic/BIO384K/ningze-sun-ada-homework-1'){
  setwd('E:/class/statistic/BIO384K/ningze-sun-ada-homework-1')
}
file = read_delim('darwin.txt', '\n')[[1]]
print(paste("In darwin.txt, there are", toString(length(file)), "paragraphs"))
print("The 34th paragraph is: ")
print(file[[34]])
split = c()
for (i in file){
  split = c(split, split_inside(i))
}
print(paste("There are", toString(length(unique(split))), "unique words in this vector"))
count = hash_count(split)
max_name = ""
max_value = 0
equal = 0
larger = 0
for (i in keys(count)){
  if (count[[i]] == 1){
    equal = equal + 1
  }
  if (count[[i]] >= 5){
    larger = larger + 1
  }
  if (count[[i]] > max_value){
    max_value = count[[i]]
    max_name = i
  }
}
print(paste("The most common word are", max_name))
print(paste(toString(equal), "words are appear 1 time"))
print(paste(toString(larger), "words are appear more than 5 time"))
final_quote = file[[length(file)]]
split_quote = split_inside(final_quote)
every_third = split_quote[seq(1, length(split_quote), 3)]
print("Every third elemnt from this vector are: ")
print(every_third)
print("The reverse alphabetical order of every_third are ")
print(sort(every_third, decreasing = TRUE))


# Challenge 2
t = c(35, 88, 42, 84, 81, 30)
city= c('Beijing', 'Lagos', 'Paris', 'Rio de Janeiro', 'San Juan', 'Toronto')
names(t) = city
print("The first 3 cities in the list are")
print(t[city[1:3]])
print("The temperatures of Paris and San Juan are")
print(t[city[c(3, 5)]])


# Challenge 3
m1 = matrix(seq(159,0), nrow = 8, ncol = 20, byrow= TRUE)
m1[5,2]
m1[5:7,]
m2=m1[3:4,6:9]
mode(m2)

a = array(seq(800, 1, -2), dim = c(5,5,4,4))
a[1, 1, 1, 2]
a[2, 3, 2, ]
a[1:5, 1:5, 3, 3]




# challenge 4
# superfamily
Lorisoidea = c('Lorisidae', 'Galagidae')
Lemuroidea = c('Cheirogaleidae', 'Lepilemuridae', 'Indriidae', 'Lemuridae', 'Daubentoniidae')
Tarsioidea = c('Tarsiidae')
Ceboidea = c("Cebidae", "Atelidae", 'Pitheciidae')
Hominoidea = c('Hylobatidae', 'Hominidae')
Cercopithecoidea = c('Cercopithecidae')
# infraorder
Lorisiformes = list(Lorisoidea=Lorisoidea)
Lemuriformes = list(Lemuroidea=Lemuroidea)
Tarsiiformes = list(Tarsioidea=Tarsioidea)
# Parvorder
Platyrrhini = list(Ceboidea=Ceboidea)
Catarrhini = list(Hominoidea=Hominoidea, Cercopithecoidea=Cercopithecoidea)
Simiiformes = list(Platyrrhini=Platyrrhini, Catarrhini=Catarrhini)
# suborder
Strepsirhini = list(Lorisiformes=Lorisiformes, Lemuriformes=Lemuriformes)
Haplorhini = list(Tarsiiformes=Tarsiiformes,Simiiformes=Simiiformes)
# order
Primates = list(Strepsirhini=Strepsirhini,Haplorhini=Haplorhini)
platyrrhines = Primates[['Haplorhini']][['Simiiformes']][['Platyrrhini']]
print(paste("The class of variable is", toString(class(platyrrhines))))
print(paste("The mode of this variable is", toString(mode(platyrrhines))))
Primates$Haplorhini$Tarsiiformes$Tarsioidea
