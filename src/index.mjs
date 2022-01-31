import fs from 'fs/promises';
import readline from 'readline-sync';
import Elm from '../dist/elm.js';
const args = process.argv.slice(2); // 0=node, 1=index.js
const flags = {args};
const app = Elm.Elm.Main.init({flags});
const oldConsoleLog = console.log;
console.log = function(...args) {
  if (args.length !== 1 || typeof args[0] !== 'string') {
    oldConsoleLog(...args);
  } else if (args[0].endsWith('"[PRINT]"')) {
    oldConsoleLog(args[0].slice(0,-11));
  } else {
    oldConsoleLog(args[0]);
  }
};
const subscribe = (portName, fn) => {
  app.ports && app.ports[portName] && app.ports[portName].subscribe(fn);
};
subscribe('readFile', async filename => {
  let contents = null;
  try {
    contents = await fs.readFile(filename, 'utf-8');
  } catch (e) {}
  app.ports.readFileResult.send(contents);
});
subscribe('print', async msg => process.stderr.write(msg));
subscribe('println', async msg => process.stderr.write(msg + '\n'));
subscribe('exitWithMessage', async (codeAndMsg) => {
  const [code, msg] = codeAndMsg;
  process.stderr.write(msg + '\n');
  process.exit(code);
});
subscribe('waitForUserInput', async () => {
  const userInput = readline.question('');
  app.ports.userInput.send(userInput);
});

