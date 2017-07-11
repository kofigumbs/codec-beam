const Elm = require('./elm');
const fs = require('fs');

const beam = {
  name: 'module',
  file: 'module.beam',
};

const app = Elm.Main.worker(beam.name);
app.ports.done.subscribe(bytes => {
  const stream = fs.createWriteStream(beam.file);
  const buffer = new Buffer(Uint8Array.from(bytes));

  stream.write(buffer);

  stream.end();
});
