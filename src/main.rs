use std::env;
extern crate pidgin;
use pidgin::Pidgin;
fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    println!(
        "{}",
        Pidgin::new().compile(&args.iter().map(String::as_str).collect::<Vec<&str>>())
    );
}
