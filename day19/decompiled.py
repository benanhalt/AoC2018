r0 = r1 = r2 = r3 = r4 = 0
#r0 = 1

r2 = 10551430 #1030
# ans = sum(
#   x
#   for x in range(1,r2+1)
#   for y in range(1,r2+1)
#   if x*y == r2
# )
# print(ans)

ans = sum(
  x
  for x in range(1,r2+1)
  if r2%x == 0
)
print(ans)


# def a(r0, r1, r2, r3, r4):
#   print(r0,r1,r2,r3,r4)
#   r4 = 1
#   while not r4 > r2:
#     r3 = 1
#     while not r3 > r2:
#       r1 = r4 * r3
#       if r1 == r2:
#         r0 += r4
#       r3 += 1
#     r4 += 1
#   print(r0,r1,r2,r3,r4)


# r1 = 22*(r1 + 8) + 18
# r2 = 19*11*(r2 + 2)**2 + r1
# if r0 == 0:
#   a(r0, r1, r2, r3, r4)
# else:
#   r1 = 30*14*32*(27*28 + 29)
#   r2 += r1
#   r0 = 0
#   a(r0, r1, r2, r3, r4)
