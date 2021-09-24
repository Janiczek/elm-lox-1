import fs from 'fs/promises';
import Elm from '../dist/elm.js';
const args = process.argv.slice(2); // 0=node, 1=index.js
const flags = {args};
const app = Elm.Elm.Main.init({flags});
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
subscribe('exit', async code => process.exit(code));
