f_window = open("regions_R1_buff.txt")
line_window = f_window.readline()
n_window = 0
while line_window:
    x_window = line_window.split()
    if int(x_window[5]) > 0:
        print (x_window[0],x_window[3],x_window[4])
    elif int(x_window[5]) <= 0:
        print (x_window[0],x_window[3])
    line_window = f_window.readline()
#    n_window += 1

f_window.close()