use std::env;
extern crate pidgin;
use pidgin::{Grammar, Pidgin};
extern crate regex;
use regex::Regex;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut p = Pidgin::new().enclosed(true);
    let weekdays = vec![
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday",
    ];
    p.add(&weekdays);
    // various abbreviations
    for s in weekdays {
        p.add_str(&s[0..3]);
        p.add_str(&s[0..2]);
    }
    p.add(&vec!["M", "T", "W", "R", "F", "S", "U"]);
    let weekdays = p.compile();
    let months = vec![
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December",
    ];
    p.add(&months);
    for m in months {
        p.add_str(&m[0..3]);
    }
    let months = p.compile();
    for i in 1..31 {
        p.add_str(i.to_string().as_str());
    }
    let monthdays = p.compile();
    p = p.string_bound().normalize_whitespace();
    p.rule("weekday", &weekdays);
    p.rule("month", &months);
    p.rule("monthday", &monthdays);
    p.foreign_rule("year", "[12][0-9]{3}");
    p.add(&vec![
        "weekday, month monthday, year",
        "month monthday",
        "weekday",
        "monthday month year",
        "month monthday, year",
    ]);
    println!("{:?}", p.compile().matcher().unwrap());
    // let rx = Regex::new(&p.compile()).unwrap();
    // p.add(&args.iter().map(String::as_str).collect::<Vec<&str>>());
    // println!("{}", p.compile());
}
