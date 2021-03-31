#[macro_export]
macro_rules! debug {
    (@print $a:expr)=>{
        {
            eprintln!("─ {}:{}: {} ────────────────", file!(), line!(), stringify!($a));
            eprintln!("{:#?}", $a);
            eprintln!("──────────────────────────────────────────────────");
        }
    };
    [$a:expr]=>{
        {
            $crate::debug!(@print $a);
        }
    };
    [$a:expr,$b:expr]=> {
        {
            $crate::debug!(@print $a);
            $crate::debug!(@print $b);
        }
    };
    [$a:expr,$($b:tt)*]=>{
        {
            $crate::debug!($a);
            $crate::debug!($($b)*);
        }
    };
}

#[macro_export]
macro_rules! syn_unoption {
    ($e:expr) => {
        {
            $e.ok_or_else(
                || syn::Error::new(
                    $e.span(), 
                    format!("{} is None", stringify!($e))
                )
            )
        }
    }
}
