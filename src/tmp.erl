
%% draw a convex! polygon. last point is connected to the first
draw_poly(S, Poly) ->
    %% split the points in two side around the top point
    %% select the middle point among serveral top points
    N = size(Poly),
    {P0,I} = find_p0(Poly),
    draw_poly_step0(S,P0,index(I-1,N),index(I+1,N),Poly).

draw_poly_step0(S,P0,I,J,Poly) ->
    P1 = point(I,Poly),
    P2 = point(J,Poly),
    L0 = mk_line(S, P0, P1),
    L1 = mk_line(S, P0, P2),
    plot(S, L0, S#s.fg1),
    draw_poly_step(S,L0,L1,I,J,Poly).


draw_poly_step(S,A,B,I,J,Poly) ->
    {A1,I1} = step_y(S,A,I,-1,Poly),
    {B1,J1} = step_y(S,B,J,+1,Poly),
    if I1 == 0, J1 == 0 ->
	    ok;
       I1 == 0; J1 == 0 ->
	    draw_poly_step(S,A1,B1,I1,J1,Poly);
       true ->
	    {Ax,Ay} = coord(A1),
	    {Bx,By} = coord(B1),
	    Ay = By,
	    %% io:format("fill: (~w, ~w), (~w,~w)\n", [Ax,Ay,Bx,By]),
	    %% to blend with bg we should draw fill first!
	    fill(S#s.pix,Ax,Bx,Ay),
	    draw_poly_step(S,A1,B1,I1,J1,Poly)
    end.

    
	
%% step Segment while Y value stay the same
step_y(_S,Segment,0,_Is,_Poly) ->
    {Segment,0};
step_y(S,Segment,I,Is,Poly) ->
    case done(Segment) of
	true ->
	    %% change to next segment
	    {Segment1, I1} = next_segment(S,I,Is,Poly),
	    {_,Y} = coord(Segment),
	    {_,Y1} = coord(Segment1),
	    if Y == Y1 ->
		    step_y(S,Segment1,I1,Is,Poly);
		true ->
		    {Segment1,I1}
	    end;
	false ->
	    Segment1 = step(Segment),
	    {_,Y} = coord(Segment),
	    {_,Y1} = coord(Segment1),
	    if Y > Y1 ->
		    {Segment,0};
	       true ->
		    plot(S,Segment1,S#s.fg1),
		    if Y == Y1 ->
			    step_y(S,Segment1,I,Is,Poly);
		       true ->
			    {Segment1,I}
		    end
	    end
    end.

%% pick next segment from poly
next_segment(S,I,Is,Poly) ->
    I1 = index(I+Is, size(Poly)),
    P0 = element(I,Poly),
    P1 = element(I1,Poly),
    Segment = mk_line(S, P0, P1),
    io:format("next_segment\n"),
    plot(S,Segment,S#s.fg1),
    {Segment, I1}.

%% Split (convex) polygon into two lists of lines segments
split_poly(Poly) ->
    if size(Poly) == 1 ->
	    {[{Poly,Poly}],[]};
       size(Poly) == 2 ->
	    {[Poly],[]};
       true ->
	    {P0,I} = find_p0(Poly),
	    split_poly(Poly,I,P0,[],I,P0,[])
    end.

split_poly(Poly,I,Pi,Pis,J,Pj,Pjs) ->
    {I1,Pi1,Pis1} = split_step(Poly,I,-1,Pi,Pis),
    if I1 == J ->
	    {reverse(Pis1),reverse(Pjs)};
       true ->
	    {J1,Pj1,Pjs1} = split_step(Poly,J,+1,Pj,Pjs),
	    if I1 == J1 ->
		    {reverse(Pis1),reverse(Pjs1)};
	       true ->
		    split_poly(Poly,I1,Pi1,Pis1,J1,Pj1,Pjs1)
	    end
    end.

split_step(Poly,I,S,Pi={_,Yi},Pis) ->
    I1 = index(I+S,size(Poly)),
    Pi1 = {_,Yi1} = point(I1,Poly),
    if Yi1 >= Yi ->
	    {I1,Pi1,[{Pi,Pi1}|Pis]};
       true ->
	    {I,Pi,Pis}
    end.

find_p0(Poly) when is_tuple(Poly) ->
    P0 = {_,Y0} = element(1, Poly),
    P1 = {_,Y1} = element(2, Poly),
    if Y0 =< Y1 ->
	    find_p0(P0,1,size(Poly),-1,Poly);
       Y0 > Y1 ->
	    find_p0(P1,2,3,1,Poly)
    end.

find_p0(P0,I0,0,_Is,_Pt) ->
    {P0,I0};
find_p0(P0,I0,I,_Is,Pt) when I > size(Pt) ->
    {P0,I0};
find_p0(P0={_,Y0},I0,I,Is,Pt) ->
    P1={_,Y1} = element(I,Pt),
    if  Y1 =< Y0 ->
	    find_p0(P1,I,I+Is,Is,Pt);
	true ->
	    find_p0(P0,I0,I+Is,Is,Pt)
    end.
