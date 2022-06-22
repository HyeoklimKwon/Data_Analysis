import pandas as pd
import numpy as np
import sys
import os

clearConsole = lambda: os.system('cls' if os.name in ('nt', 'dos') else 'clear')

def do_study(cycle, q_list, df):
    t_list = []
    t_value = []
    f_list = []
    f_value = []
    
    print('\n[{}] 번째 학습을 시작합니다.'.format(cycle))
    print('-'*40)
    
    quit = False
    for i in range(len(q_list)):
        q = df.loc[q_list[i], "question"].replace(' / ', '\n')   
        a = df.loc[q_list[i], "answer"]
        
        print('\nQuestion {}/{} - (정답 : {})'.format(i+1, len(q_list), len(t_list)))
        
        t = input('[{}] {}\n\tI : '.format(q_list[i], q))
        if t == '.':
            quit = True
            break
        
        print('\tA :', a)
        
        s = t.lower().replace(' ', '').replace('-', '').split(',')
        v = a.lower().replace(' ', '').replace('-', '')

        chkAnswer = True if len(s) > 0 else False
        for c in s:
            if(v.find(c))<0:
                chkAnswer = False
                break           
        
        if chkAnswer == False:
            f_list.append(q_list[i])
            f_value.append(a)
            print("\t☞☞☞☞☞ 오답 입니다.")
        else : 
            t_list.append(q_list[i])
            t_value.append(a)
            print("\t♡♡♡♡♡ 정답 입니다.")
    
    print('-'*40)
    print('정답({}) : {}'.format(len(t_list), t_value))
    print('오답({}) : {}'.format(len(f_list), f_value))
    print('-'*40)
    print('')
    
    return t_list, f_list, quit
    
df = pd.read_csv('exam.csv', encoding='utf-8', delimiter='|')

test_cnt = int(sys.argv[1]) if len(sys.argv)>1 else 0
if test_cnt > len(df) or test_cnt == 0:
    test_cnt = len(df)

test = df.sample(frac=1)
q_list = list(test.index[0:test_cnt])
   
clearConsole()
cycle = 0
while len(q_list) > 0:
    cycle += 1
    t_list, f_list, quit = do_study(cycle, q_list, df)
    
    if quit == True:
        break
    else:
        q_list = f_list