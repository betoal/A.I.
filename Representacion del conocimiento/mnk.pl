%commands to the interpreter are submitted from stdin input ('show input' box below)
%'halt.' will be automatically appended to stdin input.
%swi-prolog 7.2.3

%%%%%
% Utils
%%%%%

% Replaces an item in a list
replace(0, V, [_|T], [V|T]) :- !. 
replace(I, V, [H|T], [H|DT]) :-
    DI is I - 1,
    replace(DI, V, T, DT).

% Combines two lists
combine([], L, L) :- !.
combine([H|T], L, [H|M]) :-
    combine(T, L, M).

combine2([], L, L) :- !.
combine2(T, L, M) :-
    combine([T], L, M).

%%%%%
% MNK
%%%%%

% An MNK object is defined with [K, P1, P2, B]:
% K - Row length for winning
% P1 - AI for player 1 (1 or 0)
% P2 - AI for player 2 (1 or 0)
% B - The Board representation

% Gets the k
getK(MNK, K) :-
    nth0(0, MNK, K).

% Gets the player 1
isBlackAI(MNK, P) :-
    nth0(1, MNK, P).
    
% Gets the player 2
isWhiteAI(MNK, P) :-
    nth0(2, MNK, P).
    
% Gets the turn
getTurn(MNK, T) :-
    nth0(3, MNK, T).

% Gets the board
getBoard(MNK, B) :-
    nth0(4, MNK, B).

% Gets the board width
getN(MNK, N) :-
    getBoard(MNK, B),
    nth0(0, B, Aux),
    length(Aux, N).
    
% Gets the board height
getM(MNK, M) :-
    getBoard(MNK, B),
    length(B, M).
    
% Gets the position
getPosition(MNK, X, Y, P) :-
    getBoard(MNK, B),
    getBoardPosition(B, X, Y, P).
    
% Gets the position receiving the board
getBoardPosition(B, X, Y, P) :-
    nth0(Y, B, R),
    nth0(X, R, P).
    
% Makes a move and moves to the next turn
makeMove(MNK, X, Y, DMNK) :-
    getK(MNK, K),
    isBlackAI(MNK, P1),
    isWhiteAI(MNK, P2),
    getTurn(MNK, T),
    getNextTurn(MNK, NT),
    getBoard(MNK, B),
    % Edits the board
    nth0(Y, B, R),
    replace(X, T, R, DR),
    replace(Y, DR, B, DB),
    DMNK = [K, P1, P2, NT, DB].
    

% Gets previous turn
getPreviousTurn(MNK, NT) :-
    getTurn(MNK, T),
    (T =< 1 ->
        NT is 2 ;
        NT is T - 1).

% Gets the next turn
getNextTurn(MNK, NT) :-
    getTurn(MNK, T),
    (T >= 2 ->
        NT = 1 ;
        NT is T + 1).

% Gets the opponent
getOpponent(P, O) :-
    getNextTurn(P, O).
    
% Gets the board's score depending on the player
getBoardScore(MNK, P, W, S) :-
    getM(MNK, M),
    getN(MNK, N),
    getK(MNK, K),
    NK is -K,
    DK is K - 1,
    NDK is NK + 1,
    getBoard(MNK, B),
    getBoardScoreR(M, N, K, NK, DK, NDK, P, B, 0, 0, W, S).
    
    
% Gets the score of a position
getPositionScore(MNK, X, Y, P, W, S) :-
    getM(MNK, M),
    getN(MNK, N),
    getK(MNK, K),
    NK is -K,
    DK is K - 1,
    NDK is NK + 1,
    getBoard(MNK, B),
    getPositionScoreR(M, N, K, NK, DK, NDK, P, B, X, Y, W, S).
    
    
% Gets the score of a position
% We want to maximize
% Iterates through whole board
%------
% Max width
getBoardScoreR(M, N, K, NK, DK, NDK, P, B, N, Y, W, S) :-
    DY is Y + 1,
    getBoardScoreR(M, N, K, NK, DK, NDK, P, B, 0, DY, W, S).
% End
getBoardScoreR(M, _, _, _, _, _, _, _, 0, M, 0, 0) :- !.
% Function
getBoardScoreR(M, N, K, NK, DK, NDK, P, B, X, Y, W, S) :-
    getPositionScoreR(M, N, K, NK, DK, NDK, P, B, X, Y, W1, NewS),
    % Next position
    DX is X + 1,
    getBoardScoreR(M, N, K, NK, DK, NDK, P, B, DX, Y, W2, DS),
    % Checks if won
    ((W1 =\= 0 ; W2 =\= 0) -> W is 1 ; W is 0),
    % Sums
    S is NewS + DS.

% Gets score of a position
% Used for iterations
getPositionScoreR(M, N, K, NK, DK, NDK, P, B, X, Y, W, S) :-
    % Up-Down score
    getRelativeScore(M, N, K, NK, B, P, X, Y, 0, NDK, "Y", 0, 0, 0, W1, S1),
    % Left-Right score
    getRelativeScore(M, N, K, NK, B, P, X, Y, NDK, 0, "X", 0, 0, 0, W2, S2),
    % Negative Diagonal score
    getRelativeScore(M, N, K, NK, B, P, X, Y, NDK, NDK, "N", 0, 0, 0, W3, S3),
    % Positive Diagonal score
    getRelativeScore(M, N, K, NK, B, P, X, Y, DK, NDK, "P", 0, 0, 0, W4, S4),
    % Checks if it won
    ((W1 =\= 0 ; W2 =\= 0 ; W3 =\= 0 ; W4 =\= 0)  -> W is 1 ; W is 0),    
    % Sums results
    S is S1 + S2 + S3 + S4.
    
% Gets the relative score of a position
% NK - Negative K
% A - Axis used
% PC - Player count
% OC - Opponent count
% FC - Free count (empty or player)
% -----
% Winning Scenario
getRelativeScore(_, _, K, _, _, _, _, _, _, _, _, K, _, _, 1, S) :-
    S is K ** K * 8.
% Blocking Scenario
getRelativeScore(_, _, K, _, _, _, _, _, _, _, _, _, K, _, 0, S) :-
    S is K ** K * 2.
% End of Vertical
getRelativeScore(_, _, K, _, _, _, _, _, 0, K, _, _, _, _, 0, 0) :- !.
% End of Horizontal
getRelativeScore(_, _, K, _, _, _, _, _, K, 0, _, _, _, _, 0, 0) :- !.
% End of Negative Diagonal
getRelativeScore(_, _, K, _, _, _, _, _, K, K, _, _, _, _, 0, 0) :- !.
% End of Positive Diagonal
getRelativeScore(_, _, K, NK, _, _, _, _, K, NK, _, _, _, _, 0, 0) :- !.
% Function
getRelativeScore(M, N, K, NK, B, P, X, Y, DX, DY, A, PC, OC, FC, W, S) :-
    % Gets the relative position
    NewX is X + DX,
    NewY is Y + DY,
    getBoardPosition(B, X, Y, PS),
    % Gets the value of the stone
    ((PS > 0, NewX >= 0, NewY >= 0, NewX < N, NewY < M) ->
        % Gets relative position
        getBoardPosition(B, NewX, NewY, DPS),
        % Gets a value to give preference to centered movements
        (NewY > M / 2 ->
            CY is M - NewY ; CY is NewY),
        (NewX > N / 2 ->
            CX is N - NewX ; CX is NewX),
        CC is CX + CY,
        % Relative position is of same status
        % F checks if there has already been a
        % stone of the same player
        (DPS =:= PS ->
            DPC is PC + 1,
            DOC is 0,
            DFC is FC + 1,
            V is K * max(DX, DY) * 4;
        % Relative position is empty
        (DPS =:= 0 ->
            DPC is 0,
            DOC is 0,
            DFC is FC + 1,
            V is CC ;
        % Opponent
            DPC is 0,
            % Case when blocking totally
            (OC + 1 >= K - 1 ->
                DOC is K ;
                DOC is OC + 1),
            DFC is 0,
            V is K * max(DX, DY) * 2)) ;
        % Input was invalid or position is zero
        DPC is PC,
        DOC is OC,
        DFC is FC,
        V is 0),
    % Player's stone
    ((P =:= PS ; PS =:= 0) -> 
        DS is V ;
    % Opponent's stone
        DS is -V),
    % Checks if an axis is blocked
    (A =:= "Y" ->
        % Vertical
        NewDX is 0,
        NewDY is DY + 1 ;
    (A =:= "X" ->
        % Horizontal
        NewDX is DX + 1,
        NewDY is 0 ;
    (A =:= "N" ->
        % Negative Diagonal
        NewDX is DX + 1,
        NewDY is DY + 1 ;
        % Positive Diagonal
        NewDX is DX + 1,
        NewDY is DY - 1))),
    % Recursive call - moves one position towards the other side
    getRelativeScore(M, N, K, NK, B, P, X, Y, NewDX, NewDY, A, DPC, DOC, DFC, W, NewS),
    S is DS + NewS.

%%%%%
% Tree
%%%%%

% A tree is defined with a list of nodes,
% and a node is defined with [ID, P, Player, D, MNK, X, Y, W, V]
% ID - Id
% P - Parent
% D - Depth
% MNK - An MNK object
% V - Herusitic value

% Gets the id
getID(N, ID) :-
    nth0(0, N, ID).
    
% Gets the parent
getParent(N, P) :-
    nth0(1, N, P).
    
% Gets the player
getPlayer(N, P) :-
    nth0(2, N, P).
    
% Gets the depth
getDepth(N, D) :-
    nth0(3, N, D).
    
% Gets the MNK object
getMNK(N, MNK) :-
    nth0(4, N, MNK).
    
% Gets the x position
getX(N, X) :-
    nth0(5, N, X).

% Gets the y position
getY(N, Y) :-
    nth0(6, N, Y).

% if it's a terminal node
isTerminalNode(N, W) :-
    nth0(7, N, W).

% Gets the heuristic value
getValue(N, V) :-
    nth0(8, N, V).
    
% Gets a node in a tree
getNode([], []) :- !.
getNode([TH|TT], ID, N) :-
    getID(TH, NID),
    (NID =:= ID ->
        N = TH ;
        getNode(TT, ID, N)).
    
% Gets the parent of nth height
getParentAtDepth(T, N, D, P) :-
    getParent(N, PID),
    getNode(T, PID, PN),
    getDepth(PN, PD),
    (PD < D ->
        % It is the same node
        P = N ;
        (PD =:= D ->
            % It is the parent
            P = PN ;
            % Needs to go up
            getParentAtDepth(T, PN, D, P))).
        
% Gets all the nodes that have the same value
getNodesWithValue([], _, []) :- !.
getNodesWithValue([TH|TT], V, NL) :-
    getNodesWithValue(TT, V, DNL),
    getValue(TH, V2),
    (V =:= V2 ->
        % Checks it is not the first node
        getID(TH, ID),
        (ID > 0 ->
            combine2(TH, DNL, NL) ;
            NL = DNL) ;
        NL = DNL).

% Gets all the children of a node from a tree
getChildren([], _, []) :-!.
getChildren([TH|TT], N, C) :-
    getChildren(TT, N, NewC),
    getID(N, ID),
    getParent(TH, P),
    (ID =:= P ->
        combine2(TH, NewC, C) ;
        C = NewC).

% Generates children and saves them in a tree
%-------
% Function that calls the recursive one
generateChildren(T, Node, DT, C) :-
    getMNK(Node, MNK),
    getM(MNK, M),
    getN(MNK, N),
    generateChildrenR(T, Node, M, N, MNK, 0, 0, DT, [], C).
% End of y
generateChildrenR(T, _, M, _, _, _, M, T, C, C) :- !.
% End of x
generateChildrenR(T, Node, M, N, MNK, N, Y, DT, C, DC) :-
    DX is 0,
    DY is Y + 1,
    generateChildrenR(T, Node, M, N, MNK, DX, DY, DT, C, DC).
% Function
generateChildrenR(T, Node, M, N, MNK, X, Y, DT, C, DC) :-
    getPosition(MNK, X, Y, Pos),
    (Pos =:= 0 ->
        % The position is free
        makeMove(MNK, X, Y, DMNK),
        getID(Node, PID),
        getPlayer(Node, P),
        length(T, ID),
        getDepth(Node, D),
        DD is D + 1,
        getValue(Node, V1),
        getPositionScore(DMNK, X, Y, P, W, V2),
        S is (V1 + V2) / (DD*2),
        NewNode = [ID, PID, P, DD, DMNK, X, Y, W, S],
        combine2(NewNode, T, NewT),
        combine2(NewNode, C, NewC) ;
        % The position is used
        NewT = T,
        NewC = C),
    % Moves to next position
    DX is X + 1,
    generateChildrenR(NewT, Node, M, N, MNK, DX, Y, DT, NewC, DC).
        
    
% Gets the infinite value representation
getPlusInfinity(Inf) :-
    Inf is 2 << 16.
    
% Gets the minus infinite value representation
getMinusInfinity(MInf) :-
    MInf is -2 << 16.

% Aplha-Beta Pruning with Minimax
%-----
% Function for siblings
alphaBetaS(T, [], V1, _, _, _, _, T, V1) :- !.
alphaBetaS(T, [SH|ST], V1, D, A, B, M, DT, R) :-
    (M =:= 0 ->
        % Minimizing
        % Makes own alphabeta
        alphaBeta(T, SH, D, A, B, 0, NewT, V2),
        V3 is min(V1, V2),
        DB is min(B, V3),
        (A >= DB ->
            % Cuts
            DT = T,
            R is V3 ;
            % Continues with siblings
            alphaBetaS(NewT, ST, V3, D, A, DB, 0, DT, R))
        ;
        % Maximizing
        % Makes own alphabeta
        alphaBeta(T, SH, D, A, B, 1, NewT, V2),
        V3 is max(V1, V2),
        DA is max(A, V3),
        (DA >= B ->
            % Cuts
            DT = T,
            R is V3 ;
            % Continues with siblings
            alphaBetaS(NewT, ST, V3, D, DA, B, 1, DT, R))).
            
% Function for single node
alphaBeta(T, N, D, A, B, M, DT, R) :-
write(N),nl,
    isTerminalNode(N, TN),
    ((TN =:= 1 ; D =:= 0) ->
        % Returns because terminal node or max depth
        DT = T,
        getValue(N, R) ;
        % Continues
        DD is D - 1,
        % Generates all posible movements
        generateChildren(T, N, NewT, C),
        (M =:= 0 ->
            % Maximizes (changes from the parent)
            getMinusInfinity(V1),
            alphaBetaS(NewT, C, V1, DD, A, B, 1, DT, R)
                ;
            % Minimizes (changes from the parent)
            getPlusInfinity(V1),
            % Children's alphabeta
            alphaBetaS(NewT, C, V1, DD, A, B, 0, DT, R)
        )
    ).

% Gest the best value
% Also returns the generated tree
getBestValue(MNK, P, D, T, V) :-
    getBoardScore(MNK, P, _, V1),
    N = [0, -1, P, 0, MNK, -1, -1, 0, V1],
    OldT = [N],
    getMinusInfinity(A),
    getPlusInfinity(B),
    alphaBeta(OldT, N, D, A, B, 0, T, V).

% Gets the best movement
% D - Difficulty
getBestMove(MNK, P, D, M) :-
    getBestValue(MNK, P, D, T, V),
    write(V),
    % Gets all the best nodes and extracts a random one
    getNodesWithValue(T, V, NL),
    length(NL, L),
    random(0, L, I),
    nth0(I, NL, N),
    % Gets the first movement
    getParentAtDepth(T, N, 1, RN),
    getX(RN, X),
    getY(RN, Y),
    M = [X, Y].
    

% Test
:- MNK = [3,0,0,1,[

    [1,0,0],
    [0,1,0],
    [0,2,2]
    
    ]],
    getBestMove(MNK, 1, 1, M),
    write(M).



