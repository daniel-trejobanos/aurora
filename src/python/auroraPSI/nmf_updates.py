import numpy as np
from sklearn.decomposition._nmf import _initialize_nmf as initialize_nmf 
from scipy.sparse import lil_matrix

dicotomy_tol = 1e-6
shift = 1e-10
sigmaL = 4

def dichotomy_simplex(num, denum, tol=dicotomy_tol, maxit=40):
    """
    Function to solve the num/(x+denum) -1 = 0 equation. Here, x is the Lagragian multiplier which is used to apply the simplex constraint.
    The first part consists in finding a and b such that num/(a+denum) -1 > 0 and num/(b+denum) -1  < 0. 
    The second part applies the dichotomy algorithm to solve the equation.
    """
    # The function has exactly one root at the right of the first singularity (the singularity at min(denum))
    
    # num = num.astype("float64")
    # denum = denum.astype("float64")

    # do some test

    assert((num>=0).all())
    assert((denum>=0).all())
    assert((np.sum(num, axis=0)>0).all())
    
    # Ideally we want to do this, but we have to exclude the case where num==0.
    # a = np.max(num/2 - denum, axis=0)
    if denum.shape[1]>1:
        a = []
        for n,d in zip(num.T, denum.T):
            m = n>0
            a.append(np.max(n[m]/2 - d[m]))
        a = np.array(a)
    else:
        d = denum[:,0]
        def max_masked(n):
            m = n>0
            return np.max(n[m]/2-d[m])
        a = np.apply_along_axis(max_masked, 0, num)
        
        
    # r = np.sum(num/denum, axis=0)
    # b = np.zeros(r.shape)
    # b[r>=1] = (len(num) * np.max(num, axis=0)/0.5 - np.min(denum, axis=0))[r>=1]
    
    b = len(num) * np.max(num, axis=0)/0.5 - np.min(denum, axis=0)

    def func(x):
        return np.sum(num / (x + denum), axis=0) - 1
    func_a = func(a)
    func_b = func(b)
    
    assert(np.sum(func_b>=0)==0)
    assert(np.sum(func_a<=0)==0)
    assert(np.sum(np.isnan(func_a))==0)
    assert(np.sum(np.isnan(func_b))==0)


    return dicotomy(a, b, func, maxit, tol)

def dicotomy(a, b, func, maxit, tol):
    """
    Dicotomy algorithm searching for func(x)=0.

    Inputs:
    * a: bound such that func(a) > 0
    * b: bound such that func(b) < 0
    * maxit: maximum number of iteration
    * tol: tolerance - the algorithm stops if |func(sol)| < tol
    
    This algorithm work for number or numpy array of any size.
    """
    # Dichotomy algorithm to solve the equation
    it = 0
    new = (a + b)/2
    func_new = func(new)
    while np.max(np.abs(func_new)) > tol:
        
        it=it+1
        func_a = func(a)
        # func_b = func(b)

        # if f(a)*f(new) <0 then f(new) < 0 --> store in b
        minus_bool = func_a * func_new <= 0
        
        # if f(a)*f(new) > 0 then f(new) > 0 --> store in a
        # plus_bool = func_a * func_new > 0
        plus_bool = np.logical_not(minus_bool)

        b[minus_bool] = new[minus_bool]
        a[plus_bool] = new[plus_bool]
        new = (a + b) / 2
        func_new = func(new)
        if it>=maxit:
            print("Dicotomy stopped for maximum number of iterations")
            break
    return new

def Frobenius_loss(X, W, H, average=False):
    """
    Compute the generalized KL divergence.

    \sum_{ji} | X_{ij} - (D H)_{ij} |^2
    """
    
    DH = W @ H

    if average:
        return np.mean((DH - X)**2)
    else:
        return np.sum((DH - X)**2)

def trace_xtLx(L, x, average=False):
    if average:
        return np.mean(x * (L @ x))
    else:
        return np.sum(x * (L @ x))
    
    

def create_laplacian_matrix(nx):
    """
    Helper method to create the laplacian matrix for the laplacian regularization
    :param n: width of the original image
    :return:the n x n laplacian matrix
    """
    assert(nx>1)
    #Blocks corresponding to the corner of the image (linking row elements)
    mat=lil_matrix((nx,nx),dtype=np.float32)
    mat.setdiag(2)
    mat.setdiag(-1,k=1)
    mat.setdiag(-1,k=-1)
    mat[0,0] = 1
    mat[nx-1, nx-1] = 1
    return mat

def multiplicative_step_G(X, F, G, force_simplex=True,  safe=True, dicotomy_tol=dicotomy_tol, shift=shift):
    """
    Multiplicative step in G.
    """

    if safe:
        # Allow for very small negative values!
        assert(np.sum(G<-shift/2)==0)
        assert(np.sum(F<-shift/2)==0)
        G = np.maximum(G, shift)
        F = np.maximum(F, shift)
    
    FF = F.T @ F
    FX = F.T @ X
    num = G * FX
    denum = FF @ G


    if force_simplex:
        nu = dichotomy_simplex(num, denum,dicotomy_tol)
    else:
        nu = 0
        
    if safe:
        assert np.sum(denum<0)==0
        assert np.sum(num<0)==0

    new_G = num/(denum+nu)
    return new_G



def multiplicative_step_F(X, F, G, lambda_L=0, L=None, sigmaL=sigmaL, safe=True, shift=shift):
    """
    Multiplicative step in F.
    """
    if not(lambda_L==0):
        if L is None:
            raise ValueError("Please provide the laplacian")
    if safe:
        # Allow for very small negative values!
        assert(np.sum(G<-shift/2)==0)
        assert(np.sum(F<-shift/2)==0)
        G = np.maximum(G, shift)
        F = np.maximum(F, shift)

    GG = G @ G.T
    XG = X @ G.T
    num = F * XG
    denum = F @ GG  

    if not(lambda_L==0):
        denum += np.maximum(lambda_L * (L @ F),0)
            
    if safe:
        assert np.sum(denum<0)==0
        assert np.sum(num<0)==0

    new_F = num/denum

    return new_F

def initialize_algorithms(X, F, G, n_components, init, random_state, force_simplex):
    # Handle initialization
    if F is None:
        if G is None:
            F, G = initialize_nmf(X, n_components=n_components, init=init, random_state=random_state)
            if force_simplex:
                scale = np.sum(G, axis=0, keepdims=True)
                G = np.nan_to_num(G/scale, nan = 1.0/G.shape[0] )
        else:
            F = np.abs(np.linalg.lstsq(G, X.T, rcond=None)[0]).T

    elif G is None:
        G = np.abs(np.linalg.lstsq(F, X, rcond=None)[0])
        if force_simplex:
            scale = np.sum(G, axis=0, keepdims=True)
            G = G/scale

    return F, G
