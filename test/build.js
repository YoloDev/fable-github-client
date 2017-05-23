const fs = require('fs-extra');
const path = require('path');
const glob = require('glob');
const rollup = require('rollup');
const fable = require('rollup-plugin-fable');
const sourcemaps = require('rollup-plugin-sourcemaps');
const package = require('./package.json');

const external = [
  ...Object.keys(package.dependencies || {}),
  ...Object.keys(package.devDependencies || {}),
];

const replaceExt = (file, ext) => 
  path.basename(file, path.extname(file)) + ext;

const globP = dir => new Promise((resolve, reject) => {
  glob('**/*.fs', { cwd: dir, nodir: true }, (err, files) => {
    if (err) {
      reject(err);
      return;
    }

    resolve(files);
  });
});

const plugins = [
  fable({
    babel: {
      presets: [
        ['env', {
          targets: {
            node: 'current'
          },
          modules: false
        }]
      ],
      babelrc: false,
      sourceMaps: true,
    }
  }),
  sourcemaps()
];

const main = async (argv) => {
  const proj = argv[2];

  if (!proj) {
    console.log('no project file given');
    return 1;
  }

  const stat = await fs.stat(proj);
  if (!stat.isFile()) {
    console.log(`${proj} is not a file`);
    return 1;
  }

  const port = process.env.FABLE_SERVER_PORT;
  if (!port) {
    console.log(`fable port not set`);
    return 1;
  }

  const dir = path.dirname(proj);
  const outDir = path.join(dir, 'bin', 'js');
  const files = await globP(dir);

  let cache = await rollup.rollup({
    entry: proj,
    plugins,
    external
  });

  for (const file of files) {
    if (path.basename(file).startsWith('_') || file.includes('helpers')) {
      continue;
    }

    console.log(`process file: ${file}`);
    const bundle = await rollup.rollup({
      entry: file,
      cache,
      plugins,
      external
    });

    cache = bundle;
    const dest = path.join(outDir, replaceExt(file, '.js'));
    await bundle.write({
      format: 'cjs',
      sourceMap: true,
      dest
    });
  }
};

main(process.argv).then(process.exit).catch(e => {
  console.log(`ERR: ${e.toString()}`);
  process.exit(1);
});