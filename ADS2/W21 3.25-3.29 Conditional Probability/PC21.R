setwd("E:\\A大学\\大二下\\ADS2\\W21 3.25-3.29 Conditional Probability")


## Task 1.1
## NOTE: 问的是how many of them are probably thief, "them" 指的是被机器认为是thief的staff

## use Euler diagram
n = 10
data = {}
now = 1e6
ans = 0
while(T){
  n = n + 1
  thief = n * 0.1
  non_thief = n - thief
  director_thief = thief * 0.8 + non_thief * 0.2
  if(abs(director_thief - 50) < now){
    now = abs(director_thief - 50)
    ans = n
  }
  if(n > 300)
    break
}
# length(11:n)
# length(data)
# plot(x = 11:n, y = data)
print(ans)
# print(now)

## use simulation

work = function(){
  detector = 0
  count = 0
  while(detector < 50){
    staff = sample(c("thief","nonthief"),1,prob=c(0.1,0.9))
    result = sample(c("correct","wrong"),1,prob=c(0.8,0.2))
    if((staff == "thief" && result == "correct") ||
       (staff == "nonthief" && result == "wrong")){
      detector = detector + 1
      if(staff == "thief")
        count = count + 1
    }
  }
  return (count)
}
mean(rep(work(),1e8))
# although we repeated this process by 10^8 times, the average results vary a lot.

## Task 1.2 Use Bayes’ theorem

# p(thief | lied according to the lie detector) =
# p(thief) * p(lied according to lie detecto | thief) / p(lied according to the lie detector)
# = 0.1 * 0.8 / (50/n) (n = 192)
# = 0.3072
# 这个也算行吧，但是std的算法更好些


# p(fail) = 0.9*0.2 + 0.1*0.8 = 0.26
# p(thief|fail) = p(fail|thief)*p(thief)/p(fail) = 0.8 * 0.1/0.26 = 0.31
# ans = 50 * 0.31 = 15

## Task 2.2 数学期望算法
N = 10
f = rep(0, N)
nxt = rep(0, N)
str = c("H","T","T","H")
len = length(str)
i = 2
j = 0
while(i <= len){
  while(j > 0 && str[i] != str[j + 1]){
    j = nxt[j]
  }
  if(j < len && str[i] == str[j + 1]){
    j = j + 1
  }
  nxt[i] = j
  i = i + 1
}
# nxt[i] represents the maximum length where the str[i-nxt[i]+1 : i]
# is the same as str[1:nxt[i]]
# for(i in 1:len)
#   print(nxt[i])
get_f = function(i){
  if(i == 0 || i > len){
    return (0)
  }
  else{
    return (f[i])
  }
}
for(i in 1:len)
  f[i] = 2*(get_f(i-1) + 1 - 0.5 * get_f(nxt[i]))
print(f[len])

# 动态规划
# 设 f(i) 表示合成序列1~i的期望步数
# 考虑转移式：f(i) = f(i-1) + 1 + 0.5 * 0 + 0.5*(f(i) - f(nxt[i]))
# 考虑f(i)肯定是从f(i-1)这个状态转移过来，然后肯定要+1步因为要花费一步去完成抛硬币这个动作，
# 有0.5的概率什么都不用做了，因为已经得到了正确的位置i对应的硬币面
# 还有0.5的概率位置i的硬币面错了，现在根据贪心的思想，会转移到f(nxt[i])这个状态，
# 相当于保留了当前生成的序列最大的后缀，这个后缀可以匹配目标序列的最大前缀
# 所以是0.5的概率乘上(f(i)-f(nxt[i]))这个期望步数，即已经合成了1-nxt[i]的序列，
# 到还要合成1-i的序列需要多少步
# 然后上面的那个转移式整理一下就变成了 f(i) = 2*(f(i-1)+1-0.5*f(nxt[i]))
# 时间复杂度为O(n)

## simulation DFS算法
maxlen = 40
limit = 1e5
count = 0
data = rep(NA, limit)
sum = 0
dfs = function(len, str){
  #print(str)
  if(len>=4 && substr(str,len-3,len) == "HTTH"){
    count <<- count + 1
    data[count] <<- len
    sum <<- sum + (0.5^len)*len
    # print(data[1:count])
    return (0)
  }
  if(len < maxlen && count < limit){
    if(sample(1:2, 1) == 1){
      dfs(len+1,paste0(str,"H"))
      dfs(len+1,paste0(str,"T"))
    }
    else{
      dfs(len+1,paste0(str,"T"))
      dfs(len+1,paste0(str,"H"))
    }
  }
  else{
    count = count - 1
  }
}
dfs(0,"")
#data
#length(data)
hist(data)
mean(na.omit(data))


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
# boxplot(data)
# summary(data)
# Task 2.1 simulation
