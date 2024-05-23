## simulation BFS(breadth-first-search) algorithm
limit = 1e6
queue = rep(NA, limit)
q2 = rep(NA, limit)
front = tail = 1
queue[1] = ""
q2[1] = 0
data = {}
paste1 = function(str, c){
  if(str == ""){
    if(c == "H")
      return ("H")
    if(c == "T")
      return ("");
  }
  if(str == "H"){
    if(c == "H")
      return ("H")
    if(c == "T")
      return ("HT")
  }
  if(str == "HT"){
    if(c == "T")
      return ("HTT")
    if(c == "H")
      return ("H")
  }
  if(str == "HTT"){
    if(c == "H")
      return ("HTTH")
    if(c == "T")
      return ("")
  }
  # print("error")
  # print(str)
  return ("")
}
while(front <= tail){
  pos = sample(front:tail,1)
  
  t = queue[front]
  queue[front] = queue[pos]
  queue[pos] = t
  
  t = q2[front]
  q2[front] = q2[pos]
  q2[pos] = t
  
  str = queue[front]
  len = nchar(str)
  if(len == 4 && str == "HTTH"){
    data = c(data, q2[front])
  }
  else{
    if(tail < limit){
      tail = tail + 1
      queue[tail] = paste1(str,"H")
      q2[tail] = q2[front] + 1
    }
    if(tail < limit){
      tail = tail + 1
      queue[tail] = paste1(str,"T")
      q2[tail] = q2[front] + 1
    }
  }
  front = front + 1
}
mean(data)
hist(data)