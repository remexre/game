#[macro_export]
macro_rules! must {
    ($e:expr) => {
        match $e {
            Ok(x) => x,
            Err(err) => {
                std::eprintln!("Renderer error: {}", err);
                std::process::exit(1);
            }
        }
    };
}
