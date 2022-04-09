This is a basic, beginner-made haskell program.

Compile it with `$ ghc chooseNext.hs -o chooseNext`. If you dont have GHC: https://www.haskell.org/ghc/

Use `$ ./chooseNext` to get instructions on how to use the program

Example uses:

`$ ./chooseNext Damian Otis Mollymauk`
`$ ./chooseNext myDevFile.txt`

If you use a dev file, use the format given in devs-example.txt

Have fun! Or not! This is about work! But you never know!

Because this is my first solo Haskell project, I didn't know how to add explicit dependencies.
I used libraries that you may not have and I haven't documented which ones they are, nor given an easy way to get them or compile the project with them. 
Don't expect the project to compile as-is. You will probably have to add libraries via `Cabal`, `Stack`, `Nix`, or in any way you may want to do it.
Next version will include dependencies and improve UI and code (hopefully)

Roadmap :
- add explicit dependencies
- add feature to handle days of week in files (if your team changes during the week)
- add feature to create file from given devs (so you don't have to do it through a text editor you sloth)
- I don't know, ice cream? Ice cream sounds good at the end of the road(map)
