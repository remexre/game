use crate::RendererState;
use std::{
    mem::replace,
    panic::{catch_unwind, AssertUnwindSafe},
    process::exit,
    slice,
    sync::Arc,
};
use vulkano::{
    buffer::{BufferUsage, ImmutableBuffer},
    sync::GpuFuture,
};

#[no_mangle]
pub extern "C" fn renderer_alloc_immutable(
    state: &mut RendererState,
    ptr: *const f32,
    length: usize,
) -> Box<Arc<ImmutableBuffer<[f32]>>> {
    catch_unwind(AssertUnwindSafe(|| {
        let data = unsafe { slice::from_raw_parts(ptr, length) };
        let (buf, fut) = ImmutableBuffer::from_iter(
            data.iter().cloned(),
            BufferUsage::all(),
            state.queue.clone(),
        )
        .unwrap();
        let cleanup_future = replace(&mut state.cleanup_future, None).unwrap().join(fut);
        state.cleanup_future = Some(Box::new(cleanup_future));
        Box::new(buf)
    }))
    .unwrap_or_else(|e| {
        eprintln!("Caught panic: {:?}", e);
        exit(1);
    })
}

#[no_mangle]
pub extern "C" fn renderer_free_immutable(buf: Box<Arc<ImmutableBuffer<[f32]>>>) {
    catch_unwind(AssertUnwindSafe(|| drop(buf))).unwrap_or_else(|e| {
        eprintln!("Caught panic: {:?}", e);
        exit(1);
    })
}
