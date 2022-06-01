from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.utils.validation import check_is_fitted
import numpy as np

from auroraPSI.nmf_updates import multiplicative_step_F,multiplicative_step_G, Frobenius_loss, trace_xtLx, initialize_algorithms, create_laplacian_matrix
from auroraPSI.nmf_updates import dicotomy_tol, sigmaL, shift
import time

class NMFEstimator(TransformerMixin, BaseEstimator):
    
    loss_names_ = ["Frobenius loss", "Laplacian loss"]

    
    def __init__(self, n_components=2, init='warn', tol=1e-4, max_iter=200,
                 random_state=None, verbose=1, debug=False, lambda_L=0,
                shift=shift, eval_print=10, force_simplex=False, dicotomy_tol=dicotomy_tol):
        self.n_components = n_components
        self.init = init
        self.tol = tol
        self.max_iter = max_iter
        self.random_state = random_state
        self.verbose = verbose
        self.shift = shift
        self.debug = debug
        self.eval_print = eval_print
        self.lambda_L = lambda_L
        self.force_simplex = force_simplex
        self.dicotomy_tol = dicotomy_tol



    def _more_tags(self):
        return {'requires_positive_X': True}

    def _iteration(self,  F, G, noG):
        F = multiplicative_step_F(self.X_,  F, G, safe=self.debug, lambda_L=self.lambda_L, L=self.L_, shift=self.shift)
        if not noG:
            G = multiplicative_step_G(self.X_, F, G, force_simplex=self.force_simplex, safe=self.debug, dicotomy_tol=self.dicotomy_tol, shift=self.shift)
        return F, G
    

    def loss(self, F, G, average=True, X = None):
        if X is None : 
            X = self.X_

        assert(X.shape == (F.shape[0],G.shape[1]))

        self.FG_numel_ = F.shape[0] * G.shape[1]
        
        loss_fro = Frobenius_loss(X, F, G, average=average)
        loss = loss_fro
        self.detailed_loss_ = [loss_fro, 0]

        if self.lambda_L>0:
            loss_tr =  self.lambda_L * trace_xtLx(self.L_, F, average=average)
            loss += loss_tr
            self.detailed_loss_[1] = loss_tr
        
        # if average:
        #     loss = loss / self.FG_numel_
        return loss

    def fit_transform(self, X, y=None, F=None, G=None, noG=False):
        """Learn a NMF model for the data X and returns the transformed data.
        This is more efficient than calling fit followed by transform.
        Parameters
        ----------
        X : {array-like, sparse matrix} of shape (n_samples, n_features)
            Data matrix to be decomposed
        F : array-like of shape (n_samples, n_components)
            If specified, it is used as initial guess for the solution.
        G : array-like of shape (n_components, n_features)
            If specified, it is used as initial guess for the solution.
        Returns
        -------
        F,G : ndarrays
        """

        self.X_ = self._validate_data(X, dtype=[np.float64, np.float32])

  


        self.X_ = self.remove_zeros_lines(self.X_, self.shift)

        
        self.F_, self.G_ = initialize_algorithms(X = self.X_, F= F, G = G, n_components = self.n_components, init = self.init, random_state = self.random_state, force_simplex = self.force_simplex)

        if self.lambda_L > 0:
            self.L_ = create_laplacian_matrix(self.F_.shape[0])
        else:
            self.L_ = None


        algo_start = time.time()
        # If mu_sparse != 0, this is the regularized step of the algorithm
        # Otherwise this is directly the data fitting step
        eval_before = np.inf
        eval_init = self.loss(self.F_, self.G_)
        self.n_iter_ = 0

        # if self.debug:
        self.losses_ = []
        self.rel_ = []
        self.detailed_losses_ = []
        
        try:
            while True:
                # Take one step in A, P
                old_F, old_G = self.F_.copy(), self.G_.copy()
                
                self.F_, self.G_ = self._iteration(self.F_, self.G_, noG=noG)
                eval_after = self.loss(self.F_, self.G_)
                self.n_iter_ +=1
                
                rel_F = np.max((self.F_ - old_F)/(self.F_ + self.tol*np.mean(self.F_) ))
                rel_G = np.max((self.G_ - old_G)/(self.G_ + self.tol*np.mean(self.G_) ))

                # store some information for assessing the convergence
                # for debugging purposes
                detailed_loss_ = self.detailed_loss_

                self.losses_.append(eval_after)
                self.detailed_losses_.append(detailed_loss_)
                self.rel_.append([rel_F,rel_G])
                              
                # check convergence criterions
                if self.n_iter_ >= self.max_iter:
                    print("exits because max_iteration was reached")
                    break

                # If there is no regularization the algorithm stops with this criterion
                # Otherwise it goes to the data fitting step
                elif max(rel_F,rel_G) < self.tol:
                    print(
                        "exits because of relative change rel_F {} or rel_G {} < tol ".format(
                            rel_F,rel_G
                        )
                    )
                    break
                elif abs((eval_before - eval_after)/eval_init) < self.tol:
                    print(
                        "exits because of relative change < tol: {}".format(
                            (eval_before - eval_after)/min(eval_before, eval_after)
                        )
                    )
                    break

                elif np.isnan(eval_after):
                    print("exit because of the presence of NaN")
                    break

                elif (eval_before - eval_after) < 0:
                    if hasattr(self, "accelerate"):
                        if not self.accelerate:
                            print("exit because of negative decrease {}: {}, {}".format((eval_before - eval_after), eval_before, eval_after))
                            break
                    else:
                        print("exit because of negative decrease {}: {}, {}".format((eval_before - eval_after), eval_before, eval_after))
                        break
                
                if self.verbose > 0 and np.mod(self.n_iter_, self.eval_print) == 0:
                    print(
                        f"It {self.n_iter_} / {self.max_iter}: loss {eval_after:0.3f},  {self.n_iter_/(time.time()-algo_start):0.3f} it/s",
                    )
                    pass
                eval_before = eval_after
        except KeyboardInterrupt:
            pass
        
        # if not(self.force_simplex):
        #     self.F_, self.G_ = rescaled_DH(self.F_, self.G_ )
        
        algo_time = time.time() - algo_start
        print(
            f"Stopped after {self.n_iter_} iterations in {algo_time//60} minutes "
            f"and {np.round(algo_time) % 60} seconds."
        )
        self.reconstruction_err_ = self.loss(self.F_, self.G_)
        
        self.n_components_ = self.G_.shape[0]
        self.components_ = self.G_
        return self.F_

    def fit(self, X, y=None, **params):
        """Learn a NMF model for the data X.
        Parameters
        ----------
        X : {array-like, sparse matrix} of shape (n_samples, n_features)
            Data matrix to be decomposed
        y : Ignored
        Returns
        -------
        self
        """
        self.fit_transform(X, **params)
        return self

    def inverse_transform(self, F):
        """Transform data back to its original space.
        Parameters
        ----------
        W : {ndarray, sparse matrix} of shape (n_samples, n_components)
            Transformed data matrix.
        Returns
        -------
        X : {ndarray, sparse matrix} of shape (n_samples, n_features)
            Data matrix of original shape.
        .. versionadded:: 0.18
        """
        check_is_fitted(self)
        return  F @ self.G_
    
    def get_losses(self):
        
        names = ["full_loss"] + self.loss_names_ + ["rel_W","rel_H"]
        dt_list = []
        for elt in names : 
            dt_list.append((elt,"float64"))
        dt = np.dtype(dt_list)

        tup_list = []
        for i in range(len(self.losses_)) : 
            tup_list.append((self.losses_[i],) + tuple(self.detailed_losses_[i]) + tuple(self.rel_[i]))
        
        array = np.array(tup_list,dtype=dt)

        return array

    def remove_zeros_lines (self, X, epsilon) : 
        if np.all(X >= 0) : 
            new_X = X.copy()
            sum_cols = X.sum(axis = 0)
            sum_rows = X.sum(axis = 1)
            new_X[:,np.where(sum_cols == 0)] = epsilon
            new_X[np.where(sum_rows == 0),:] = epsilon
            return new_X
        else : 
            raise ValueError("There are negative values in X")