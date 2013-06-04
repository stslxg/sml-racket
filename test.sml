
fun is_older(x, y) = 
#1 x < #1 y orelse (#1 x = #1 y andalso #2 x < #2 y) orelse (#1 x = #1 y andalso #2 x = #2 y andalso #3 x < #3 y)

fun number_in_month(ls , y) =
    if null ls
    then 0
    else if #2 (hd ls) = y
    then number_in_month(tl ls, y) + 1
    else number_in_month(tl ls, y)

fun number_in_months(x, y    ) = 
    if null y
    then 0
    else number_in_month(x, hd y) + number_in_months(x, tl y)

fun dates_in_month(   ls, y) = 
    if null ls
    then []
    else if #2 (hd ls) = y
    then (hd ls) :: dates_in_month(tl ls, y)
    else dates_in_month(tl ls, y)

fun dates_in_months(x, y) =
    if null y
    then []
    else dates_in_month(x, hd y) @ dates_in_months(x, tl y)

fun get_nth(ls, n ) = 
    if n = 1
    then hd ls
    else get_nth(tl ls, n-1)

fun date_to_string(date) =
    let val months = ["January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(months, #2 (hd date)) ^ " " ^ Int.toString(#3 (hd date)) ^ ", " ^ Int.toString(#1 (hd date))
    end

fun number_before_reaching_sum(sum, x) =
    let fun tmp(xx,  s) = 
	    if s <= hd xx
	    then 0
	    else tmp(tl xx, s - (hd xx)) + 1
    in tmp(x, sum)
    end

fun what_month(day  ) = 
    let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
    in number_before_reaching_sum(day, days_in_months) + 1
    end

fun month_range(day1 , day2) =
    let fun countup(from , to  ) = 
	if from > to
	then []
	else what_month(from) :: countup(from + 1, to)
    in countup(day1, day2)
    end

(*
fun oldest(l) = 
    if null l
    then NONE
    else let fun tmp(ll) = 
		 if null (tl ll) 
		 then hd ll
		 else let val tl_ans = tmp(tl ll)
		      in if is_older(hd ll, tl_ans)
			 then hd ll
			 else tl_ans
		      end
	 in SOME (tmp(l))
	 end 
*)
fun remove_duplicate(l) = 
    if null l
    then []
    else let val tl_ans = remove_duplicate(tl l)
	     fun in_list ( x , ll) = 
		 if null ll
		 then false
		 else if x = hd ll
		 then true
		 else in_list(x, tl ll)
	 in if in_list(hd l, tl_ans)
	    then tl_ans
	    else hd l :: tl_ans
	 end

fun number_in_months_challenge(x, y ) = 
    number_in_months(x, remove_duplicate(y))

fun dates_in_months_challenge(x, y ) =
    dates_in_months(x, remove_duplicate(y))

fun reasonable_date(y) = 
    let val x = hd y
	fun max_days_in_month(year, month) = 
	    let fun leap(x ) = 
		    x mod 400 = 0 orelse (x mod 4 = 0 andalso x mod 100 <> 0)
		fun get_nth(ls , n ) = 
		    if n = 1
		    then hd ls
		    else get_nth(tl ls, n-1)
		val leap_year = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		val normal_year = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    in if leap(year)
	       then get_nth(leap_year, month)
	       else get_nth(normal_year, month)
	    end
    in #1 x > 0 andalso #2 x >= 1 andalso #2 x <= 12 andalso #3 x >= 1 andalso #3 x <= max_days_in_month(#1 x, #2 x)
    end


val a = is_older((0,1,2),(3,4,5));
print a;

val b = get_nth(["1","2"], 1);
print b;

val c = date_to_string([(2013, 1, 12)]);
print c;

val d = number_before_reaching_sum(6, [1,2,3,4,5,5]);
val e = number_before_reaching_sum(5, [1,2,3,4,5]);
print d;
print e;

val f = month_range(10,12);
val g = month_range(12,10);
print f;
print g;

val h = reasonable_date([(2013, 1, 12)]);
val i = reasonable_date([(2013, 2, 30)]);
val j = reasonable_date([(2000, 2, 29)]);
print h;
print i;
print j;
