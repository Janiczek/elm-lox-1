import fs from 'fs/promises';
import Elm from '../dist/elm.js';
const args = process.argv.slice(2); // 0=node, 1=index.js
const flags = {args};
const app = Elm.Elm.Main.init({flags});
const subscribe = (portName, fn) => {
  app.ports && app.ports[portName] && app.ports[portName].subscribe(fn);
};
subscribe('readFile', async filename => {
  const contents = await fs.readFile(filename, 'utf-8');
  app.ports.readFileResult.send(contents);
});
subscribe('print', async msg => console.log(msg));
