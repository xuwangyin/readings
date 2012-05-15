def inclusiveRange(start, stop, step=1):
    return range(start, stop + 1, step)

class Solution:
    def __init__(self, path, headOrder):
        assert len(path) >= 1
        self.path = path
        self.head = path[-1]
        self.headOrder = headOrder
    
    def getPath(self):
        return list(self.path)

    def getHeadOrder(self):
        return self.headOrder
    
def solutionInRange(solutions, ordering, correctOrdering, lhs, rhs):
    totalEvents = rhs - lhs + 1
    value = ordering[rhs]
    valueOrder = correctOrdering[value]
    # valueOrder = ordering[rhs]    
    newPath = []
    newHeadOrder = -1
    
    for nevents in inclusiveRange(1, totalEvents - 1):
        for eventIdx in inclusiveRange(lhs, rhs - nevents + 1):
            solutionRange = (eventIdx, eventIdx + nevents - 1)
            solutionPath = solutions[solutionRange].getPath()
            solutionHeadOrder = solutions[solutionRange].getHeadOrder()
            newSolutionHeadOrder = solutionHeadOrder
            if valueOrder > solutionHeadOrder:
                solutionPath.append(value)
                newSolutionHeadOrder = valueOrder
            if len(solutionPath) > len(newPath):
                newPath = solutionPath
                newHeadOrder = newSolutionHeadOrder
                
    return Solution(newPath, newHeadOrder)
        
def maxOrdered(ordering, correctOrdering):
    solutions = {}

    totalEvents = len(ordering)
    dummy = -1
    ordering.insert(0, dummy)
    lhsIdx = 1
    rhsIdex = totalEvents
    
    # add solutions of one event
    for i in inclusiveRange(lhsIdx, rhsIdex):
        solutions[(i, i)] = Solution(ordering[i: i + 1], correctOrdering[ordering[i]])
        # solutions[(i, i)] = Solution(ordering[i: i + 1], ordering[i])        

    for nevents in inclusiveRange(2, totalEvents):
        for eventIdx in inclusiveRange(lhsIdx, rhsIdex - nevents + 1):
            solutionRange = (eventIdx, eventIdx + nevents - 1)
            solutions[solutionRange] = solutionInRange(solutions, ordering, correctOrdering, solutionRange[0], solutionRange[1])
        
    return solutions[(lhsIdx, rhsIdex)].getPath()


    
if __name__ == "__main__":
    inputOrdering = [3, 1, 2, 4, 9, 5, 10, 6, 8, 7]
    inputOrdering2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    inputOrdering2 = [4, 7, 2, 3, 10, 6, 9, 1, 5, 8]
    inputOrdering2 = [3, 1, 2, 4, 9, 5, 10, 6, 8, 7]
    inputOrdering2 = [2, 10, 1, 3, 8, 4, 9, 5, 7, 6]
    # print ordering

    correctOrdering = range(0, len(inputOrdering) + 1)
    for order, value in enumerate(inputOrdering):
        correctOrdering[order + 1] = value

    ordering = range(0, len(inputOrdering2) + 1)
    for order, value in enumerate(inputOrdering2):
        ordering[value] = order + 1

    del ordering[0]
    print ordering
    # print correctOrdering
    maxOrdered =  maxOrdered(ordering, correctOrdering)
    print len(maxOrdered)
    print maxOrdered
    # for i in range(0, len(maxOrdered)):
        # print correctOrdering[maxOrdered[i]],
        # print maxOrdered[i]
        # assert correctOrdering[maxOrdered[i]] > correctOrdering[maxOrdered[i - 1]]
    
    # print len(maxOrdered)
    
