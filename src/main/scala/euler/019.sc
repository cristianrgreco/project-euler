def daysOfMonth(month: Int, year: Int): Int = month match {
  case 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
  case 2 => if (isLeapYear(year)) 29 else 28
  case 4 | 6 | 9 | 11 => 30
}

def numberOfDaysForYear(year: Int): Int = if (isLeapYear(year)) 366 else 365

def isLeapYear(year: Int): Boolean = {
  if (year % 4 == 0) {
    if (year % 100 == 0) {
      if (year % 400 == 0) true
      else false
    }
    else true
  }
  else false
}


val startYear = 1900  // We use this year to capture the Monday and then discard it
val endYear = 2000

val numberOfDays = startYear.to(endYear).map(numberOfDaysForYear).sum
val numberOfYears = startYear.to(endYear).length
val numberOfMonths = numberOfYears * 12

var days = 1.to(numberOfDays).toList
days = days.drop(numberOfDaysForYear(1900))

val mappedDays = 1.to(numberOfYears - 1).flatMap(yearIndex => {
  1.to(12).map(monthIndex => {
    val daysToTake = daysOfMonth(monthIndex, yearIndex)
    val result = days.take(daysToTake)
    days = days.drop(daysToTake)
    result
  })
}).toList


mappedDays.map(_.head).count(_ % 7 == 0)
