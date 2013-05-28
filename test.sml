fun number_in_month (   ls, y) =
    if null ls
    then 0
    else if (hd ls) = y
    then number_in_month(tl ls, y) + 1
    else number_in_month(tl ls, y)

fun number_in_months(  x,  y) = 
    if null y
    then 0
    else number_in_month(x, hd y) + number_in_months(x, tl y)

fun dates_in_month(ls, y) = 
    if null ls
    then []
    else if hd ls = y
    then append(hd ls, dates_in_month(tl ls, y))
    else dates_in_month(tl ls, y)

fun get_nth(ls, n ) = 
    if n = 1
    then hd ls
    else get_nth(tl ls, n-1)

fun date_to_string(date) =
    let val months = ["January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(months, date)
    end

fun number_before_reaching_sum( sum , x ) =
    let fun tmp(xx: int list,  s: int) = 
	    if s <= hd xx
	    then 0
	    else tmp(tl xx, s - (hd xx)) + 1
    in tmp(x, sum)
    end

fun what_month(day : int) = 
    let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
    in number_before_reaching_sum(day, days_in_months) + 1
    end

fun month_range(day1, day2 ) =
    let fun countup(from , to ) = 
	if from > to
	then []
	else what_month(from) :: countup(from + 1, to)
    in countup(day1, day2)
    end

fun remove_duplicate(l    ) = 
    if null l
    then []
    else let val tl_ans = remove_duplicate(tl l)
	     fun in_list ( x  , ll  ) = 
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

fun dates_in_months_challenge(x, y) =
    dates_in_months(x, remove_duplicate(y))
