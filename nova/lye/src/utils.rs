use byteorder::{LittleEndian, ReadBytesExt};
use std::{
    ffi::CString,
    fs::File,
    io::{ErrorKind as IoErrorKind, Result as IoResult},
    path::Path,
};

macro_rules! extensions {
    ($($ext:ident),* $(,)*) => {{
        let mut v: std::vec::Vec<std::ffi::CString> = std::vec::Vec::new();
        $(v.push(std::borrow::ToOwned::to_owned($ext::name()));)*
        v
    }};
}

macro_rules! deref_field {
    ($ty:ty, $field:ident : $field_ty:ty) => {
        impl std::ops::Deref for $ty {
            type Target = $field_ty;
            fn deref(&self) -> &$field_ty {
                &self.$field
            }
        }

        impl std::ops::DerefMut for $ty {
            fn deref_mut(&mut self) -> &mut $field_ty {
                &mut self.$field
            }
        }
    };
}

pub fn char_array_to_cstring(bs: &[i8]) -> CString {
    let bs = bs
        .iter()
        .map(|&n| n as u8)
        .take_while(|&b| b != 0)
        .collect::<Vec<_>>();
    CString::new(bs).unwrap()
}

pub fn read_u32s<P: AsRef<Path>>(path: P) -> IoResult<Vec<u32>> {
    // TODO: Is this right for reading SPIR-V on a big-endian system?
    let mut file = File::open(path)?;
    let mut v = Vec::new();
    loop {
        match file.read_u32::<LittleEndian>() {
            Ok(x) => v.push(x),
            Err(ref err) if err.kind() == IoErrorKind::UnexpectedEof => break Ok(v),
            Err(err) => break Err(err),
        }
    }
}

pub(crate) trait Sealed {}
