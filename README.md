# neovim-ocaml
neovim api for OCaml

This is a work-in-progress.  

So far it works with the few functions I've tested, but the return values aren't being parsed correctly.  Also, none of the neovim events are handled.

Note: This uses Async because it's what I'm familiar with.  If you wish to use
with Lwt, making it work for that wouldn't be too hard.  You'd need to
implement a Conn for Lwt, and functorize the generated code over it so you can
substitute in Lwt or Async.  
