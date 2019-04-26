#[macro_export]
macro_rules! dprintln {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            let s = format!($($arg)*);
            println!("{}", ansi_term::Colour::White.dimmed().paint(s));
        }
    }
}

#[macro_export]
macro_rules! dprint {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            let s = format!($($arg)*);
            print!("{}", ansi_term::Colour::White.dimmed().paint(s));
        }
    }
}

#[macro_export]
macro_rules! when_debug {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            $($arg)*;
        }
    };
}

#[macro_export]
macro_rules! retrieve {
    ($e:expr,$p:path) => {{
        match $e {
            $p(x) => x,
            _ => panic!(),
        }
    }};
}

#[macro_export]
macro_rules! matches {
    ($e:expr, $p:pat) => {
        match $e {
            $p => true,
            _ => false,
        }
    };
}
