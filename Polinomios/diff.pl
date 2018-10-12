
d( X, X, 1 ):- !. /* d(X) w.r.t. X is 1 */

d( C, X, 0 ):- atomic(C). /* If C is a constant then *//* d(C)/dX is 0 */

d( U+V, X, R ):- /* d(U+V)/dX = A+B where */
	d( U, X, A ), /* A = d(U)/dX and */
	d( V, X, B ), 
	R = A + B.
d( U-V, X, R ):- 
	d( U, X, A),
	d( V, X, B),
	R = A - B.
d( C*U, X, R ):-
	atomic(C),
	C \= X,
	d( U, X, A ),
	R = C * A,
	!.
d( U*V, X, U*B+V*A ):- /* d(U*V)/dX = B*U+A*V where */
	d( U, X, A ),
	/* A = d(U)/dX and */
	d( V, X, B ).
	/* B = d(V)/dX */
d( U/V, X, (A*V-B*U)/(V^2) ):- /* d(U/V)/dX = (A*V-B*U)/(V*V) */
	d( U, X, A), /* where A = d(U)/dX and */
	d( V, X, B). /* B = d(V)/dX */
d( U^C, X, R ):- /* d(U^C)/dX = C*A*U^(C-1) */
	atomic(C),
	/* where C is a number or */
	C\=X,
	d( U, X, A ),
	R = C * A * U ^ ( C - 1 ).

d( sin(W), X, Z*cos(W) ):- /* d(sin(W))/dX = Z*cos(W) */
	d( W, X, Z). /* where Z = d(W)/dX */
d( exp(W), X, Z*exp(W) ):- /* d(exp(W))/dX = Z*exp(W) */
	d( W, X, Z). /* where Z = d(W)/dX */
d( log(W), X, Z/W ):- /* d(log(W))/dX = Z/W */
	d( W, X, Z). /* where Z = d(W)/dX */
d( cos(W), X, -(Z*sin(W)) ):- /* d(cos(W))/dX = Z*sin(W) */
	d( W, X, Z). /* where Z = d(W)/dX */
d( tan(W), X, (Z*sec(W)^2) ):- /* d(tan(W))/dX = Z*sec(W)^2 */
	d( W, X, Z). /* where Z = d(W)/dX */
