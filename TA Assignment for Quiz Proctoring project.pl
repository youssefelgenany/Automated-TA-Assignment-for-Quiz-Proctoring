assign_proctors(AllTAs, Quizzes, TeachingSchedule, ProctoringSchedule):-
	free_schedule(AllTAs,TeachingSchedule,FreeSchedule),!,
	assign_quizzes(Quizzes, FreeSchedule, ProctoringSchedule).


assign_quizzes([],_,[]).
assign_quizzes([Quiz | Quizzes], FreeSchedule, [proctors(Quiz, AssignedTAs) | RestProctoringSchedule]):-
    assign_quiz(Quiz, FreeSchedule, AssignedTAs),
	removeFromFreeSchedule(Quiz,AssignedTAs,FreeSchedule,NewFreeSchedule),
    assign_quizzes(Quizzes, NewFreeSchedule, RestProctoringSchedule).

	
removeFromFreeSchedule(Quiz,AssignedTAs,[Day|Rest],[Day|NewRest]):-
	Quiz=quiz(_,QuizDayName,_,_),
	Day=day(DayName,_),
	QuizDayName\=DayName,
	removeFromFreeSchedule(Quiz,AssignedTAs,Rest,NewRest).
	
	
removeFromFreeSchedule(quiz(_,Day,Slot,_),AssignedTAs,[day(Day,FreeDaySchedule)|Rest],[day(Day,NewFreeDaySchedule)|Rest]):-
	nth1(Slot,FreeDaySchedule,FreeTAs),
	deleteTAs(AssignedTAs,FreeTAs,NewFreeTAs),
	replace(FreeDaySchedule,Slot,NewFreeTAs,NewFreeDaySchedule).

	
replace([_|T],1,X,[X|T]).
replace([H|T],I,X,[H|R]):- I>0,I1 is I-1,replace(T,I1,X,R).

deleteTAs(_,[],[]).
deleteTAs([],[H|T],[H|T]).
deleteTAs([TA|TAs],L,NewFreeTAs):-
	member(TA,L),
	delete(L,TA,NL),
	deleteTAs(TAs,NL,NewFreeTAs).

assign_quiz(quiz(_,Day,Slot,Count),FreeSchedule,AssignedTAs):-
    member(day(Day,DaySchedule),FreeSchedule),
    nth1(Slot,DaySchedule,FreeTAs),
    permutation(NewList,FreeTAs),
    append_count(NewList,0,Count,AssignedTAs).

append_count(_,X,X,[]).
append_count([H|T1],Start,End,[H|T2]):-
    S is Start+1,
    append_count(T1,S,End,T2).
	
	
free_schedule(AllTAs, TeachingSchedule, FreeSchedule):-
	schedule_helper1(AllTAs,TeachingSchedule,FreeSchedule).
	
   
schedule_helper1(_,[],[]).
schedule_helper1(AllTAs,[day(Day,DaySchedule)|TeachingSchedule],[day(Day,DayFreeSchedule)|FreeSchedule]):-
	deleteOffTAs(AllTAs,Day,NewAllTAs),
	schedule_helper2(NewAllTAs,DaySchedule,DayFreeSchedule),
	schedule_helper1(AllTAs,TeachingSchedule,FreeSchedule).
	
deleteOffTAs([],_,[]).
deleteOffTAs([ta(Name,Day)|TAs],NDay,[ta(Name,Day)|NewAllTAs]):-
	NDay\=Day,
	deleteOffTAs(TAs,NDay,NewAllTAs).
deleteOffTAs([ta(_,Day)|TAs],Day,NewAllTAs):-
	deleteOffTAs(TAs,Day,NewAllTAs).
  
schedule_helper2(_,[],[]).
schedule_helper2(NewAllTAs,[Slot|Slots],[NewSlotPermutation|NewSlots]):-
	names(NewAllTAs,NewAllTAsNames),
	deleteTAs(Slot,NewAllTAsNames,NewSlot),
	permutation(NewSlot,NewSlotPermutation),
	schedule_helper2(NewAllTAs,Slots,NewSlots).
	
names([],[]).
names([ta(Name,_)|T],[Name|L]):-
	names(T,L). 

	
	