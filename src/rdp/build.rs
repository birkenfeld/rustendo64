extern crate gcc;

fn main() {
    gcc::Config::new()
        .cpp(false)
        .file("n64video.c")
        .include(".")
        .compile("libn64video.a");
}
