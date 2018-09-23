(* 1. val is_older = fn : (int * int * int) * (int * int * int) -> bool *)
fun is_older (date1 : (int * int * int), date2 : (int * int * int)) =
  let 
    val (y1, m1, d1) = date1
    val (y2, m2, d2) = date2
  in
    y1 < y2 
    orelse (y1=y2 andalso m1<m2)
    orelse (y1=y2 andalso m1=m2 andalso d1<d2)
  end
  
  
(* 2. val number_in_month = fn : (int * int * int) list * int -> int *)
fun number_in_month (dates : (int * int * int) list, month : int) = 
  if null dates
  then 0
  else 
    let val cond = if (#2 (hd dates) = month) then 1 else 0
    in cond + number_in_month(tl dates, month)
    end
    

(* 3. val number_in_months = fn : (int * int * int) list * int list -> int *)
fun number_in_months (dates : (int * int * int) list, months : int list) = 
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(* 4. val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list *)
fun dates_in_month (dates : (int * int * int) list, month : int) = 
  if null dates
  then []
  else if #2 (hd dates) = month
  then (hd dates) :: dates_in_month(tl dates, month) 
  else dates_in_month(tl dates, month)


(* 5. val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list *)
fun dates_in_months (dates : (int * int * int) list, months : int list) = 
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
    

(* 6. val get_nth = fn : string list * int -> string *)
fun get_nth (ls : string list, n : int) = 
  if n = 1
  then hd ls
  else get_nth(tl ls, n-1)


(* 7. val date_to_string = fn : int * int * int -> string *)
fun date_to_string (date : int * int * int) = 
  let
    val month_names = ["January", "February", "March", "April",
                       "May", "June", "July", "August",
                       "September", "October", "November", "December"]
  in 
      get_nth(month_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^
      Int.toString(#1 date)
  end


(* 8. val number_before_reaching_sum = fn : int * int list -> int *)
fun number_before_reaching_sum (s : int, ls : int list) = 
  if s <= hd ls
  then 0
  else 1 + number_before_reaching_sum (s - hd ls, tl ls)


(* 9. val what_month = fn : int -> int *)
fun what_month (day : int) = 
  let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in number_before_reaching_sum(day, days) + 1
  end


(* 10. val month_range = fn : int * int -> int list *)
fun month_range (day1 : int, day2 : int) = 
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1, day2)


(* 11. val oldest = fn : (int * int * int) list -> (int * int * int) option *)
fun oldest (dates : (int * int * int) list) = 
  if null dates
  then NONE
  else let
      fun oldest_nonempty (dates : (int * int * int) list) = 
        if null (tl dates)
        then hd dates
        else 
            let
                val first_date = hd dates
                val tl_ans = oldest_nonempty(tl dates)
            in
                if is_older(first_date, tl_ans)
                then first_date
                else tl_ans
            end
       in
         SOME (oldest_nonempty dates)
       end


(* 12. val number_in_months_challenge = fn : (int * int * int) list * int list -> int 
       val dates_in_months_challenge = fn : (int * int * int) list * int list -> int *)

(* auxiliary function *)
fun is_in(elm : int, ls : int list) =
    if null ls
    then false
    else
        if elm = hd ls
        then true
        else is_in(elm, tl ls)


fun number_in_months_challenge (dates : (int * int * int) list, months : int list) = 
    let 
        val hd_months = hd months
        val tl_months = tl months

    in
        if null(tl_months)
        then number_in_month(dates, hd_months)
        else 
            if is_in(hd_months, tl_months) 
            then number_in_months_challenge(dates, tl_months)
            else number_in_month(dates, hd_months) + number_in_months_challenge(dates, tl_months)
    end


fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) = 
    let 
        val hd_months = hd months
        val tl_months = tl months

    in
        if null(tl_months)
        then dates_in_month(dates, hd_months)
        else 
            if is_in(hd_months, tl_months) 
            then dates_in_months_challenge(dates, tl_months)
            else dates_in_month(dates, hd_months) @ dates_in_months_challenge(dates, tl_months)
    end


(* 13. val reasonable_date = fn : int * int * int -> bool *)

fun get_nth_int (ls : int list, n : int) = 
    if n = 1
    then hd ls
    else get_nth_int(tl ls, n-1)


fun reasonable_date (date : int * int * int) = 
    let 
        val year = #1 date
        val month = #2 date
        val day = #3 date
        val days = 
            if ((year mod 100 <> 0) andalso (year mod 4 = 0)) orelse (year mod 400 = 0)
            then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
        if year <= 0 
        orelse month < 1 orelse month > 12 
        orelse day < 1 orelse day > get_nth_int(days, month)
        then false
        else true
    end
