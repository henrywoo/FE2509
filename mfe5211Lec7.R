r= 0.10
sigma=0.30
s0=100
X=100
H=97
T=0.2

GBSOption(TypeFlag = "c", S = s0, X = X, Time = T, r = r, b = 0, sigma = sigma)


PTSingleAssetBarrierOption(TypeFlag="cdoB", S=s0, X=X, H=H, time1=0,Time=T, r=r, b=0, sigma= sigma)
