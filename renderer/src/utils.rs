use std::ffi::CString;

macro_rules! cstrings {
    ($($l:literal),* $(,)*) => {{
        let mut v: std::vec::Vec<std::ffi::CString> = std::vec::Vec::new();
        $(v.push(std::ffi::CString::new($l.as_bytes()).unwrap());)*
        v
    }};
}

pub fn char_array_to_cstring(bs: &[i8]) -> CString {
    let bs = bs
        .iter()
        .map(|&n| n as u8)
        .take_while(|&b| b != 0)
        .collect::<Vec<_>>();
    CString::new(bs).unwrap()
}
