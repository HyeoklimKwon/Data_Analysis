
cases = int(input()) #3 

for i in range(cases) : # for i in range (3)
    days = int(input())  # 3
    values = list(map(int, input().split( ))) # [10, 7, 6]
    result = 0
    interest_list = []
    for value , idx in values, range(days) :
        values2 = values[idx:]
        for value2 in values2 :
            interest_list.append((value2 - value))
        result += max(interest_list)
    
    print(result)