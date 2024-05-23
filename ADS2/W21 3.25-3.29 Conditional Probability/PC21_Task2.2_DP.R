## Task 2.2
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