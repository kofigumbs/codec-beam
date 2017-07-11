const Elm = require('./elm');
const fs = require('fs');

const beam = {
  name: 'module',
  file: 'module.beam',
};

const app = Elm.Main.worker(beam.name);
app.ports.done.subscribe(array => {
  const stream = fs.createWriteStream(beam.file);
  const buffer = new Buffer(Uint8Array.from(array));

  stream.write(buffer);

  stream.end();
});
