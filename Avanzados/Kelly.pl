
% Code made by
% Francisco L�pez Franco
% Alberto Acosta
% Adolfo Priego

% % %
% Utils
% % %


% Gets a list representing a csv and transforms it into a normal list
csvToList(Csv, List) :-
    maplist(rowToList, Csv, List).
rowToList(Row, List) :-
    Row =..[_|List].

listToCsv(List, Csv) :-
    maplist(listToRow, List, Csv).
listToRow(List, Csv) :-
    Csv =.. [row|List].

% Combines two lists
combine([], L, L) :- !.
combine([H|T], L, [H|M]) :-
    combine(T, L, M).

% Gets a sublist
sublist(Until, Until, _, []) :- !.
sublist(_, _, [], []) :- !.
sublist(I, Until, List, [SH|ST]) :-
    nth0(I, List, SH),
    DI is I + 1,
    sublist(DI, Until, List, ST).

% Index of
indexOf(V, List, I) :-
    indexOf(V, List, 0, I).
indexOf(_, [], _, -1) :- !.
indexOf(V, [V|_], DI, DI) :- !.
indexOf(V, [_|LT], DI, I) :-
    DDI is DI + 1,
    indexOf(V, LT, DDI, I).

% Gets the max of a list
getMax([LH|LT], Max) :-
    getMax(LT, LH, Max).
getMax([], DMax, DMax) :- !.
getMax([LH|LT], DMax, Max) :-
    (LH > DMax ->
        getMax(LT, LH, Max) ;
        getMax(LT, DMax, Max)).

% Gets the min of a list
getMin([LH|LT], Min) :-
    getMin(LT, LH, Min).
getMin([], DMin, DMin) :- !.
getMin([LH|LT], DMin, Min) :-
    (LH < DMin ->
        getMin(LT, LH, Min) ;
        getMin(LT, DMin, Min)).


% % %
% Kelly
% % %
% Language:
% Practices - Number of children per generation
% Hours - Number of generations

getGrade(Kelly, Grade) :-
    nth0(0, Kelly, Grade).

getKellyNotes(Kelly, Notes) :-
    nth0(1, Kelly, Notes).

% Writes the generations into kelly.csv
writeKelliesSongs(Piano, KelliesNotes, FileName) :-
    listToCsv(KelliesNotes, Csv),
    getPath(Piano, Path),
    atom_concat(Path, 'kellies/', P1),
    atom_concat(P1, FileName, P4),
    atom_concat(P4, '.csv', NewPath),
    csv_write_file(NewPath, Csv).

% Teaches kelly n Practices a year for m Hours
% Prints the result in Kellies.csv
startPractices(Piano, SongName, Practices, Difficulty, Hours, Name) :-
    loadSongNotes(Piano, SongName, SongNotes),
    % Runs each finger
    startPracticesIndividual(Piano, SongNotes, Practices, Difficulty, Hours, Name).

% Runs for individual fingers
startPracticesIndividual(Piano, SongNotes, Practices, Difficulty, Hours, Name) :-
    startPracticesIndividual(Piano, SongNotes, 1, Practices, Difficulty, Hours, Name).
startPracticesIndividual(_, [], _, _, _, _, _) :- !.
startPracticesIndividual(Piano, [[Instrument|SongNote]|OtherNotes], Finger, Practices, Difficulty, Hours, Name) :-
    % Gets the first generation
    getPianoNotes(Piano, PianoNotes),
    takeFirstPractices(PianoNotes, SongNote, Practices, Difficulty, FirstGeneration),
    getBestKelly(FirstGeneration, Mom),
    getWorstKelly(FirstGeneration, Dad),
    % Gets next generations
    continuePractices(PianoNotes, SongNote, Practices, Hours, Mom, Dad, BestOfGenerations),
    AllGenerations = [Mom|BestOfGenerations],
    maplist(getKellyNotes, AllGenerations, KellyNotes),
    addInstrument(KellyNotes, Instrument, FinalKellyNotes),
    length(FinalKellyNotes, Length),
    write(Length), write(' Generations created.'),
    % Prints into Kelly.csv each Kelly
    atom_concat(Name, '_', P),
    atom_concat(P, Finger, NewName),
    writeKelliesSongs(Piano, FinalKellyNotes, NewName),
    NewFinger is Finger + 1,
    startPracticesIndividual(Piano, OtherNotes, NewFinger, Practices, Difficulty, Hours, Name).

% Adds the instrument
addInstrument([], _, []) :- !.
addInstrument([KH|KT], Instrument, [[Instrument|KH]|NT]) :-
    addInstrument(KT, Instrument, NT).


% Second generation and beyond
% Mom is best and Dad is worst
continuePractices(PianoNotes, SongNotes, Practices, Hours, Mom, Dad, BestOfGenerations) :-
    % Starts in the year 1 since we already had the year 0
    continuePractices(PianoNotes, SongNotes, Practices, Hours, 1, Mom, Dad, BestOfGenerations).
% Stops when found
continuePractices(_, _, _, _, _, [0, _], _, []) :- !.
continuePractices(_, _, _, Hours, Hours, _, _, []) :- !.
continuePractices(PianoNotes, SongNotes, Practices, Hours, Year, Mom, Dad, [BH|BT]) :-
    takeMorePractices(PianoNotes, SongNotes, Practices, Mom, Dad, GH),
    getBestKelly(GH, BH),
    write('Best: '), write(BH),nl,
    getWorstKelly(GH, Worst),
    NextYear is Year + 1,
    continuePractices(PianoNotes, SongNotes, Practices, Hours, NextYear, BH, Worst, BT).

% Gets the best kelly of each generation
getBestKellyEachGeneration([], []) :- !.
getBestKellyEachGeneration([GH|GT], [KH|KT]) :-
    getBestKelly(GH, KH),
    getBestKellyEachGeneration(GT, KT).

% Gets the best kelly
getBestKelly([KH|KT], Best) :-
    getBestKelly(KT, KH, Best).
getBestKelly([], DBest, DBest) :- !.
getBestKelly([KH|KT], DBest, Best) :-
    getGrade(KH, GradeA),
    getGrade(DBest, GradeB),
    (GradeA < GradeB ->
        getBestKelly(KT, KH, Best) ;
        getBestKelly(KT, DBest, Best)).

% Gets the worst kelly
getWorstKelly([KH|KT], Worst) :-
    getWorstKelly(KT, KH, Worst).
getWorstKelly([], DWorst, DWorst) :- !.
getWorstKelly([KH|KT], DWorst, Worst) :-
    getGrade(KH, GradeA),
    getGrade(DWorst, GradeB),
    (GradeA > GradeB ->
        getWorstKelly(KT, KH, Worst) ;
        getWorstKelly(KT, DWorst, Worst)).

% First generation
% Takes the first Practices
takeFirstPractices(PianoNotes, SongNotes, Practices, Difficulty, Kellies) :-
    takeFirstPractices(PianoNotes, SongNotes, Practices, 0, Difficulty, Kellies).
takeFirstPractices(_, _, Practices, Practices, _, []) :- !.
takeFirstPractices(PianoNotes, SongNotes, Practices, DL, Difficulty, [KH|KT]) :-
    takeFirstPractice(PianoNotes, SongNotes, Difficulty, KH),
    DDL is DL + 1,
    takeFirstPractices(PianoNotes, SongNotes, Practices, DDL, Difficulty, KT).
% Individual kelly
takeFirstPractice(PianoNotes, SongNotes, Difficulty, [Grade, Notes]) :-
    % Length depends on the beats
    getNotesNewLength(SongNotes, Difficulty, Length),
    takeFirstPractice(PianoNotes, SongNotes, Length, 0, OldNotes),
    Notes = [Difficulty|OldNotes],
    gradeNotes(Notes, PianoNotes, SongNotes, Grade).
takeFirstPractice(_, _, Length, Length, []) :- !.
takeFirstPractice(PianoNotes, SongNotes, Length, DL, [NH|NT]) :-
    (DL =:= 0 ->
        getFirstRandomNote(PianoNotes, NH) ;
        getRandomNote(PianoNotes, NH)),
    DDL is DL + 1,
    takeFirstPractice(PianoNotes, SongNotes, Length, DDL, NT).

% Mutated generation
% Mom is best and Dad is worst
takeMorePractices(PianoNotes, SongNotes, Practices, Mom, Dad, Kellies) :-
    % Mutates mom and dad
    %mixKellies(Mom, Dad, NewMom, NewDad),
    NewMom = Mom,NewDad = Dad,
    takeMorePractices(PianoNotes, SongNotes, Practices, 0, NewMom, NewDad, Kellies).
takeMorePractices(_, _, Practices, Practices, _, _, []) :- !.
takeMorePractices(PianoNotes, SongNotes, Practices, Practice, Mom, Dad, [Sister,Brother|KT]) :-
    takePractice(PianoNotes, SongNotes, Mom, Dad, Sister, Brother),
    NextPractice is Practice + 1,
    takeMorePractices(PianoNotes, SongNotes, Practices, NextPractice, Mom, Dad, KT).
% Mutates two kellies and gives siblings
takePractice(PianoNotes, [Tempo|Notes], [MomGrade, [Difficulty|MomNotes]], [DadGrade, [_|DadNotes]], [SisterGrade, [Difficulty|SisterNotes]], [BrotherGrade, [Difficulty|BrotherNotes]]) :-
    length(PianoNotes, L1),
    length(MomNotes, L3),
    Max is L1 * L3,
    MomIQ is (MomGrade) / Max,
    DadIQ is (DadGrade) / Max,
    takePractice(PianoNotes, 0, MomNotes, DadNotes, MomIQ, DadIQ, SisterNotes, BrotherNotes),
    gradeNotes([Difficulty|SisterNotes], PianoNotes, [Tempo|Notes], SisterGrade),
    gradeNotes([Difficulty|BrotherNotes], PianoNotes, [Tempo|Notes], BrotherGrade).
takePractice(_, _, [], [], _, _, [], []) :- !.
takePractice(PianoNotes, First, [MH|MT], [DH|DT], MomIQ, DadIQ, [MMH|MMT], [DMH|DMT]) :-
    random(0.0, 1.0, M),
    random(0.0, 1.0, D),
    % Mutates if true
    (M < MomIQ -> (First =:= 0 -> getFirstRandomNote(PianoNotes, MMH) ; getRandomNote(PianoNotes, MMH)) ; MMH = MH),
    (D < DadIQ -> (First =:= 0 -> getFirstRandomNote(PianoNotes, DMH) ; getRandomNote(PianoNotes, DMH)) ; DMH = DH),
    takePractice(PianoNotes, 1, MT, DT, MomIQ, DadIQ, MMT, DMT).

% Mixes two kellies
mixKellies([Grade, [Difficulty|MomNotes]], [_, [_|DadNotes]], [Grade, [Difficulty|MomMixNotes]], [Grade, [Difficulty|DadMixNotes]]) :-
    % Gets random split
    length(MomNotes, Length),
    random(1, Length, Split),
    % Gets random side (0 or 1)
    random(0, 2, Side),
    % Splits
    (Side =:= 0 ->
        % Left
        sublist(0, Split, MomNotes, LeftDadNotes),
        sublist(0, Split, DadNotes, LeftMomNotes),
        sublist(Split, Length, MomNotes, RightMomNotes),
        sublist(Split, Length, DadNotes, RightDadNotes) ;
        % Right
        sublist(0, Split, MomNotes, LeftMomNotes),
        sublist(0, Split, DadNotes, LeftDadNotes),
        sublist(Split, Length, MomNotes, RightDadNotes),
        sublist(Split, Length, DadNotes, RightMomNotes)),

    combine(LeftMomNotes, RightMomNotes, MomMixNotes),
    combine(LeftDadNotes, RightDadNotes, DadMixNotes).


% Fitness function
%
gradeNotes([NT|NN], PianoNotes, SongNotes,  Grade) :-
    % Removes the tempo from the notes for easier recursion
    gradeNotes(NN, PianoNotes, SongNotes, _, NT, 0, 0, Grade).
gradeNotes([], _, _, _, _, _, Sum, Sum) :- !.
gradeNotes([NH|NT], PianoNotes, [Tempo|DS], LastNote, Difficulty, I, Sum, Grade) :-
    % I can be a float
    Index is floor(I + 0.009),
    % Gets the note
    nth0(Index, DS, Note),

    % When difficulty is more
    (I > Index ->
    ((LastNote == Note, NH == '-') ->
         NoteGrade is 0 ; NoteGrade is 1),
         DLastNote = LastNote ;
         % Integer index
        (NH == Note -> NoteGrade is 0 ; NoteGrade is 1),
         DLastNote = NH
    ),
    % I relative to the difficulty
    DI is I + Tempo / Difficulty,
    NewSum is Sum + NoteGrade,
    % The note of the song equivalent with the difficulty
    gradeNotes(NT, PianoNotes, [Tempo|DS], DLastNote, Difficulty, DI, NewSum, Grade).


% Gets the grade of a note
gradeNotes(N1, N2, _, _, _, Grade) :-
    (N1 == N2 -> Grade is 0 ; Grade is 1).
        %(N1 == '-' -> D1 = LastN1, E1 is 0.25 ; D1 = N1, E1 is 0),
        %(N2 == '-' -> D2 = LastN2, E2 is 0.25 ; D2 = N2, E2 is 0),
        %(D1 == D2 -> Grade is 1 ; Grade is 1)).
        %indexOf(D1, Notes, I1),
        %indexOf(D2, Notes, I2),

% % %
% Piano
% % %

% Makes a Piano
piano(Path, NotesDifficulty, Piano) :-
    loadPianoNotes(Path, NotesDifficulty, Notes),
    Piano = [Path, Notes].

getPath(Piano, Path) :-
    nth0(0, Piano, Path).

getSongsPath(Piano, SongsPath) :-
    getPath(Piano, Path),
    atom_concat(Path, 'songs/', SongsPath).

getPianoNotes(Piano, Notes) :-
    nth0(1, Piano, Notes).

% Loads the notes of the piano
loadPianoNotes(Path, NotesDifficulty, Notes) :-
    atom_concat(Path, 'notes_', P1),
    atom_concat(P1, NotesDifficulty, P2),
    atom_concat(P2, '.csv', NewPath),
    csv_read_file(NewPath, Csv),
    csvToList(Csv, N),
    nth0(0, N, Notes).

% Gets the notes tempo
getTempo(Notes, Tempo) :-
    nth0(0, Notes, Tempo).

% Gets the length of the notes depending on the beat
getNotesNewLength(Notes, Tempo, Length) :-
    getTempo(Notes, OldTempo),
    length(Notes, L),
    % Removes the part of the tempo
    OldLength is L - 1,
    Compasses is OldLength / OldTempo,
    Length is round(Compasses * Tempo).

% Gets a random note
getRandomNote(PianoNotes, Note) :-
    length(PianoNotes, OldL),
    L is OldL + 1,
    random(0, L, I),
    (I >= OldL ->
        Note = '-' ;
        nth0(I, PianoNotes, Note)).

% Gets a random note excluding the string
getFirstRandomNote(PianoNotes, Note) :-
    length(PianoNotes, L),
    random(0, L, I),
    nth0(I, PianoNotes, Note).

% Loads a song's notes for 5 fingers
loadSongNotes(Piano, Name, Notes) :-
    atom_concat(Name, '.csv', NewName),
    getSongsPath(Piano, Songs),
    atom_concat(Songs, NewName, Path),
    csv_read_file(Path, Csv),
    csvToList(Csv, Notes).


:-
    Path =  'C:/Users/franc/Google Drive/Trabajo/Escuela/ITAM/Computaci�n/Inteligencia Artificial/Kelly/',


    Practices is 100, % * 2
    Difficulty is 24,
    Hours is 1000,
    NotesDifficulty = 'easy',
    SongName = 'Twinkle',


    piano(Path, NotesDifficulty, Piano),
    atom_concat(SongName, '_', P1),
    atom_concat(P1, Difficulty, P2),
    atom_concat(P2, '_', P3),
    atom_concat(P3, NotesDifficulty, FileName),
    startPractices(Piano, SongName, Practices, Difficulty, Hours, FileName).
