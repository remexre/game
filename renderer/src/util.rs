#[macro_export]
macro_rules! must {
    ($e:expr) => {
        match $e {
            Ok(x) => x,
            Err(err) => {
                std::eprintln!("Renderer error: {}", err);
                std::eprintln!("  in expression: {}", stringify!($e));
                std::process::exit(1);
            }
        }
    };
}
