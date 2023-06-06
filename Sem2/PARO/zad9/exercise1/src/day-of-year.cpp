#include "day-of-year.hpp"


inline bool isLeap(int year){
    return (year % 400 == 0) or  
           ((year % 100 != 0) and  
            (year % 4 == 0));
}

int dayOfYear(int month, int dayOfMonth, int year) {
    const int FEB = 2;
    const int days_in_month[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

    dayOfMonth += (isLeap(year) && month > FEB ? 1 : 0);
    for(int i = 0; i < month-1; i++){
        dayOfMonth += days_in_month[i];
    }

    // eeeee, tak sie nie robi
    // if (month == 2) {
    //     dayOfMonth += 31;
    // } else if (month == 3) {
    //     dayOfMonth += 59;
    // } else if (month == 4) {
    //     dayOfMonth += 90;
    // } else if (month == 5) {
    //     dayOfMonth += 31 + 28 + 31 + 30;
    // } else if (month == 6) {
    //     dayOfMonth += 31 + 28 + 31 + 30 + 31;
    // } else if (month == 7) {
    //     dayOfMonth += 31 + 28 + 31 + 30 + 31 + 30;
    // } else if (month == 8) {
    //     dayOfMonth += 31 + 28 + 31 + 30 + 31 + 30 + 31;
    // } else if (month == 9) {
    //     dayOfMonth += 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31;
    // } else if (month == 10) {
    //     dayOfMonth += 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30;
    // } else if (month == 11) {
    //     dayOfMonth += 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31;
    // } else if (month == 12) {
    //     dayOfMonth += 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 31;
    // }
    return dayOfMonth;
}

