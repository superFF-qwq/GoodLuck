## Task 1.1
# D = test positive
# H1 = pregant
# H2 = non-pregnant
# p(H1|D)?

# p(D|H1) = 0.95
# p(D|H2) = 0.2
# p(H1) = 0.222
# p(H2) = 1 - 0.222 = 0.778
1 - 0.222

# p(A|B) * p(B) = p(B|A) * P(A)
# p(D|H1) * p(H1) = p(H1|D) * p(D)
# 0.95 * 0.222 = p(H1|D) * p(D)

# p(D) = p(D|H1) * P(H1) + p(D}H2) * P(H2)
# p(D) = 0.95 * 0.222 + 0.2 * 0.778 = 0.3665
0.95 * 0.222 + 0.2 * 0.778

# p(H1|D) = 0.95 * 0.222 / 0.3665
0.95 * 0.222 / 0.3665
# p(H1|D) = 0.5754434

# Is the result convincing ?
# Not really. （意思是不太convincing）Basically, it means that the girl may be
# pregnant or not with almost equal probability (0.5756 and 0.4244,
# respectively; OK, the probability of pregnancy is slightly higher).
# Still much better than before (0.2222) that is rather low.                                                                                                 and 0.4244, respectively; OK, the probability of pregnancy is slightly higher). Still much better than before

## Task 1.2
# p(pregnant | test negative) ? 注意这个girl不是normal population中随便选一个
# 出来的那种，是做过IVF的人群
# D = test negative
# H1 = pregant
# H2 = non-pregnant
# p(H1 | D)?
# p(H1 | D) = p(D | H1) * p(H1) / p(D)
# p(D) = 0.05 * 0.222 + 0.8 * 0.778 = 0.6335
0.05 * 0.222 + 0.8 * 0.778
# p(H1 | D) = 0.05 * 0.222 / 0.6335 = 0.0175217
0.05 * 0.222 / 0.6335

## Task 1.3
# Maybe.
# Because the previous study says p(pregant after IVF) = 0.222
# She is pregant after IVF(how many times)?

# STD:
# Sure! The prior belief in success (pregnancy) was rather low (0.2222).
# But after the first positive result, there is a good reason to think that
# the girl is actually pregnant. Although the test kit is not very good
# (we will discuss why later), it is still able to tell whether the person
# is pregnant or not. So, there is a good reason to consider pregnancy

## Task 1.4
# P(pregnant | test positive twice)
# D = test positive twice
# H1 = pregant
# H2 = non-pregnant
# P(pregnant | test positive twice) = p(H1|D) ?
# p(H1) = 0.222
# p(H2) = 1 - 0.222 = 0.778 
# p(D|H1) = 0.95 * 0.95 = 0.9025
0.95 * 0.95
# p(D|H2) = 0.2 * 0.2 = 0.04
# p(D) = p(D|H1) * P(H1) + p(D|H2) * p(H2) = 0.231475
0.9025 * 0.222 + 0.04 * 0.778
# p(D|H1) * p(H1) = p(H1|D) * p(D)
# p(H1|D) = 0.9025 * 0.222 / 0.231475 = 0.8655578
0.9025 * 0.222 / 0.231475
# Using the kit twice quite increase the validity of the result.

## Task 1.5
# odds and odds ratio? Remind yourself of the learning from BG2.
# 而“odds ratio”又称比值比或优势比，是一个统计指标，
# 用于衡量两个不同发生的概率之间的比值。
# 它通常被用来比较两组数据中某一发生的概率是否存在差异，
# 从而判断两个变量之间是否存在相关性，并预测未来的发生概率。
# 具体来说，它表示某一发生的概率与该不发生的概率之间的比值，
# 以“a:b”或“a/b”的形式表示，其中a表示成功次数，b表示失败次数。
# 在计算时，其公式为Odds Ratio = (A/C) / (B/D)，
# 其中A、B分别表示两组数据中某一发生和不发生的次数，
# C、D分别表示另一组数据中该发生和不发生的次数。
# 在流行病学研究中，OR值（odds ratio）是病例对照研究中的一个常用指标，
# 它指的是病例组中暴露人数与非暴露人数的比值除以对照组中暴露人数与非暴露人数的比值。
# OR值的具体意义如下：如果OR值等于1，表示该因素对疾病的发生不起作用；
# OR值大于1，表示该因素是危险因素；OR值小于1，表示该因素是保护因素。
#               test positive   test negative
# pregnant            0.95            0.05
# non-pregnant        0.2             0.8
# odds ratio = (0.95/0.2) / (0.05/0.8) = 76
(0.95/0.2) / (0.05/0.8)
1/76

# I feel the following is the correct format.
#               pregnant   non-pregnant
# positive        0.95            0.2
# negative        0.05            0.8
# odds ratio = (0.95/0.05) / (0.2/0.8) = 76
(0.95/0.05) / (0.2/0.8)
# suprisingly, odds ratio does NOT change!
# I do not think it is good.
# It is not specific.
# But it is sensitive.
# The odds ratio shows that the kit has an assistant effect on
# the detection of pregnancy.

## Task 2

## TAsk 2.1: Does the choice of prior influence the end result?

# THey definitely are.
# prior -> posterior

## Task 2.2: Also, why do the priors for each person have to add up to 1?
# The prior constitutes all the possible events.

## Task 2.3: And why does nobody choose priors of exactly 0 or exactly 1?
## What would happen if they did?
# 0 means definitely no.
# 1 means definitely yes.

