# elm-lox-1

A port of the `jlox` interpreter from "Crafting Interpreters".

![Screenshot of it in action](./screenshot.png)

## TODO

- [ ] Chapters 6.3.1..6.3.3, 8.2.2: `Parser.onError`, `Parser.recover`, `Parser.synchronizeTo : Token -> ...`? `Result` holding a list of errors instead of a single error? On error skip tokens until you see a specific one
- [ ] Everywhere we return an error with line -1, thread some token in there to be able to get its line

- [ ] check that we set global variable if not found in the local scope, and that it survives the deletion of the local Env
```
var x = 1;
{
  print x; // 1
  x = 2;
  print x; // 2

  var y = 1;
  print y; // 1
}
print x; // 2
```

- [ ] Implement everything :)
- [ ] Ask to be added in [the list of Lox implementations](https://github.com/munificent/craftinginterpreters/wiki/Lox-implementations)
