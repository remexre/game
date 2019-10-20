//! Different pipelines.

pub mod forward;

// TODO: Set up some shared pipeline cache. Sounds like making a pipeline can be pretty damn
// expensive. As with anything, profile it first though...
//
// If we do that, we also might as well go the full nine yards and get enough info to pass an
// ApplicationInfo when making the instance, then store the cache to an XDG-approved cache
// location. That gets weird though, since the engine proper needs to be sure to replicate the
// behavior for its own file management.
//
// I suppose we could expose the file management we do here to the engine, but... ew.
