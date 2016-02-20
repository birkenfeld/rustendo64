extern crate gcc;

fn main() {
    gcc::Config::new()
        .cpp(false)
        .flag("-ffast-math")
        .flag("-msse4")
        .flag("-O3")
        .flag("-std=c99")
        .file("n64video.c")
        .include(".")
        .compile("libn64video.a");
}
