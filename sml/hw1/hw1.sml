(* Homework1 *)

(* Richard Wu *)

fun is_older(date_1: int*int*int, date_2: int*int*int) : bool =
    if #1 date_1 < #1 date_2
    then true
    else if #1 date_1 > #1 date_2
    then false
    else if #2 date_1 < #2 date_2
    then true
    else if #2 date_1 > #2 date_2
    then false
    else if #3 date_1 < #3 date_2
    then true
    else false

fun number_in_month(dates: (int*int*int) list, month: int) : int =
    if null dates
    then 0
    else
    let
      val r = number_in_month(tl dates, month)
    in
      if #2 (hd dates) = month
      then r + 1
      else r
    end

fun number_in_months(dates: (int*int*int) list, months: int list) : int =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int*int*int) list, month: int) : (int*int*int) list =
    if null dates
    then nil
    else
    let
      val r = dates_in_month(tl dates, month)
    in
      if #2 (hd dates) = month
      then hd dates::r
      else r
    end

fun dates_in_months(dates: (int*int*int) list, months: int list) : (int*int*int) list =
    if null months
    then nil
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings: string list, n: int) : string = 
    if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)

fun date_to_string(date: int*int*int) : string = 
    let
      val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September",
       "October", "November", "December"]
    in
      get_nth(month_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum: int, nums: int list) : int = 
    if null nums
    then 0
    else if null (tl nums)
    then 0
    else if sum - hd nums > 0
    then 1 + number_before_reaching_sum(sum - hd nums, tl nums)
    else 0

fun what_month(day: int) : int =
    let
      val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
      1 + number_before_reaching_sum(day, month_days)
    end

fun month_range(day_1: int, day_2: int) : int list = 
    if day_1 > day_2
    then nil
    else what_month(day_1)::month_range(day_1 + 1, day_2)

fun oldest(dates: (int*int*int) list) : (int*int*int) option = 
    if null dates
    then NONE
    else 
    let
      val r = oldest(tl dates)
    in
      if isSome r andalso is_older(valOf r, hd dates)
      then r
      else SOME(hd dates)
    end

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) : int =
    let
      fun exist_number(m: int, ms: int list) : bool =
        if null ms
        then false
        else hd ms = m orelse exist_number(m, tl ms)
      fun delete_repeated(ms: int list) : int list =
        if null ms
        then nil
        else
        let
          val r = delete_repeated(tl ms)
        in
          if exist_number(hd ms, tl ms)
          then r
          else (hd ms)::r
        end
    in
      number_in_months(dates, delete_repeated(months))
    end

fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) : (int*int*int) list =
    let
      fun exist_number(m: int, ms: int list) : bool =
        if null ms
        then false
        else hd ms = m orelse exist_number(m, tl ms)
      fun delete_repeated(ms: int list) : int list =
        if null ms
        then nil
        else
        let
          val r = delete_repeated(tl ms)
        in
          if exist_number(hd ms, tl ms)
          then r
          else (hd ms)::r
        end
    in
      dates_in_months(dates, delete_repeated(months))
    end

fun reasonable_date(date: int*int*int) : bool = 
    let
      fun get_nth_m(n: int, ms: int list) : int = 
        if n = 1
        then hd ms
        else get_nth_m(n - 1, tl ms)
      val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
      if #1 date <= 0
      then false
      else if #2 date <= 0 orelse #2 date > 12
      then false
      else if #3 date <= 0
      then false
      else if #2 date = 2 andalso ((#1 date mod 400) = 0 orelse ((#1 date mod 4) = 0 andalso (#1 date mod 100) <> 0)) 
      then #3 date <= 29
      else #3 date <= get_nth_m(#2 date, month_days)
    end