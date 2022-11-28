# Gunk
Note: This project is abandoned but has been salvaged into a library: https://github.com/pema99/plonk. Excuse my terrible memes, I don't expect anyone to actually read this stuff.

What's up gamers? Coming back at you again with yet another bad language brought to you by me, the absolute god of making bad languages.
It's gonna be functional this time though, woo.

The compiler is gonna be written (hopefully) completely functionally too, through power of ***monads!*** *... OOOooooOOOoooO Cue the spooky music.*.

The goal is to make a functional language with familiar syntax. Fear not, C programmers, for you are my target audience. I want to make a language that looks and smells like C, C++, Rust and the likes, but which is (almost) purely functional.

Targetting my own proprietary stack-based VM, based somewhat on the Caml Virtual Machine.

VM will probably be written in Rust because F# has the big slow.

Lexing and parsing is EZPZ but I might mess up the compilation part, so don't get your hopes up. Heh, who am I kidding, it's not like anyone looks at my repos anyways. Such is life when you are but a sole developer of useless programs, in sea of well-payed programmers working on stuff that someone might actually use.

Damn.

# How to build
If you have read this far, you are probably the first person to ever do so. Congratulations. Here is how you build with the dotnet core cli:
```sh
git clone https://github.com/pema99/Gunk
cd Gunk
dotnet run
```
