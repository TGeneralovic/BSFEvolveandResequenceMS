# Python3 program for merging overlapping intervals

def mergeIntervals(intervals):
    # Sort the array on the basis of start values of intervals.
    intervals.sort()
    stack = []
    # insert first interval into stack
    stack.append(intervals[0])
    for i in intervals[1:]:
        # Check for overlapping interval,
        # if interval overlap
        if stack[-1][0] <= i[0] <= stack[-1][-1]:
            stack[-1][-1] = max(stack[-1][-1], i[-1])
        else:
            stack.append(i)
 
    #print("The Merged Intervals are :", end="\n")
    for i in range(len(stack)):
        print(stack[i], end="\n")
 
 
arr = [[4460000,4680000], [4480000,4700000], [4520000,4740000]]
mergeIntervals(arr)