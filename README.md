# neovim-ocaml
neovim api for OCaml

This is a work-in-progress.  

So far it works with the few functions I've tested, but the return values aren't being parsed correctly.  Also, none of the neovim events are handled.

Note: This uses Async because it's what I'm familiar with.  If you wish to use
with Lwt, making it work for that wouldn't be too hard.  You'd need to
implement a Conn for Lwt, and functorize the generated code over it so you can
substitute in Lwt or Async.  


TODO:
- Test with more functions.
- Make sure return values are working
- Figure out subscribe handling.  Maybe switch to a Pipe?
- Add MLIs for files in src/
- Figure out how to represent the Dictionary type
- Is the Object representation correct?
- Change Conn to support stdio, and unix pipes
- Maybe move Async-dependent things to its own sublibrary so that Lwt implementation is easier?
- Automate testing.  Gotta be some way of running nvim non-interactively. Otherwise, maybe tmuxish.
