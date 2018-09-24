extern crate pidgin;
use pidgin::Pidgin;
fn main() {
    // let mut v = vec!["cat", "dog", "bat"];
    // println!("{}", v.join(","));
    // v.sort();
    // for s in v {
    //     println!("{}", s);
    // }
    println!("{}", Pidgin::new().compile(&["a cog", "a dog", "a hog", "a frog","an og"]));
}
