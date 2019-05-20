import anomaly

x = [ '1','2','3','4','5']
y = [ 1,2,3,4,5 ]
y2 = [ 15,14,3,4,1 ]

s = []
t = []
i = 0
while (i<len(x)):
    s.append((x[i],y[i]))
    t.append((x[i],y2[i]))
    i += 1

print s,t
a = anomaly.histogram_comp(s,t)
print a
a = anomaly.histogram_comp(s,s)

print a
    
X = range(0,60)
Y = []

for x in X:
    Y.append((x,x,x))

P = {"periodicity":10, "alpha":0.5, "beta":0.5, "gamma":0.5, "k":1, "type":1}

state= None
dim = 2

F = anomaly.holtwinters(state,Y,P,dim)
print "HW %%%%%%"
print F
print "%%%%%%"
print F[0]
for f in F[1]:
    print "HW",f

state= None
T = [ 10, 0.5, 0.5, 0.5, 1, 1] 


F = anomaly.general_anomaly(0,state,Y,T,dim)
print "HWA %%%%%%"
print F
print "%%%%%%"

print F[0]
for f in F[1]:
    print "HWA",f

print "----------------------------------------------------------"

NP = [20,20, 1,0.2,0,1.0 ]
PC = [20,20, 1,1 ]

R = Y[0:NP[0]]
F = anomaly.general_anomaly(1,state,R,NP,dim)
print F

W = Y[NP[0]:NP[0]+NP[1]]
F = anomaly.general_anomaly(1,F[0],W,NP,dim)
print "#2", F[1]
X = Y[NP[0]+NP[1]:]
for x in X:
    F = anomaly.general_anomaly(1,F[0],[x],NP,dim)
    print F[1]





print "----------------------------------------------------------"

dim = 2
print dim

X = range(0,60)
Y = []
for x in X:
    t = [x]
    for e in range(1,dim+1):
        t.append((0.0+e)/(dim+1))
    Y.append(t)

print NP
print len(Y),Y

state = None
R = Y[0:NP[0]]
F = anomaly.general_anomaly(1,F[0],R,NP,dim)
print "RF",F

W = Y[NP[0]:NP[0]+NP[1]]
F = anomaly.general_anomaly(1,F[0],W,NP,dim)
for f in F[1]:
    print "RF",f


X = Y[NP[0]+NP[1]:]

for x in X:
    F = anomaly.general_anomaly(1,F[0],[x],NP,dim)
    print "\t",x,F[1]

    
