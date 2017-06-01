const tapParser = require('tap-parser');
const through = require('through2');
const duplexer = require('duplexer');
const format = require('chalk');
const symbols = require('figures');
const fetch = require('node-fetch');

const nullLogger = () => {};
const createAppVeyorLogger = (baseUrl) => {
  console.log('Using AppVeyor test logger');
  let chain = Promise.resolve();

  const translateOutcome = (outcome) => {
    switch (outcome) {
      case 'passed': return 'Passed';
      case 'failed': return 'Failed';
      case 'todo': return 'Ignored';
      case 'skipped': return 'Skipped';
      default: return 'None';
    }
  };
  
  return (name, outcome, error = null) => {
    const parts = name.indexOf('»') > -1 ? name.split('»') : name.split('›');
    let file = 'test';
    if (parts.length > 1) {
      file = parts[0].trim();
      name = parts.slice(1).join('›');
    }

    chain = chain.then(async () => {
      try {
        const body = {
          testName: name,
          fileName: file,
          testFramework: 'ava',
          outcome: translateOutcome(outcome)
        };

        if (error) {
          body.ErrorMessage = error.message;
          body.ErrorStackTrace = error.stack;
        }

        const result = await fetch(`${baseUrl}api/tests`, {
          method: 'post',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify(body)
        });

        const text = await result.text();
      } catch (e) {
      }
    });
  };
}

const logger = process.env.APPVEYOR_API_URL
  ? createAppVeyorLogger(process.env.APPVEYOR_API_URL)
  : nullLogger;

const makeStream = () => {
  const output = through();
  const parser = tapParser();
  const stream = duplexer(parser, output);
  const printTest = (symbol, format, name) =>
    output.push(format(` ${symbol}  ${name}\n`));

  parser.on('assert', assert => {
    output.push(' ' + symbols.pointer + ' ');
    if (assert.skip) {
      printTest(symbols.circlePipe, format.dim, assert.name);
      logger(assert.name, 'skipped');
    } else if (assert.ok) {
      printTest(symbols.circleCircle, format.green, assert.name);
      logger(assert.name, 'passed');
    } else if (assert.todo) {
      printTest(symbols.circleQuestionMark, format.yellow, assert.name);
      logger(assert.name, 'todo');
    } else {
      printTest(symbols.circleCross, format.red.bold, assert.name);
      const message = `${assert.diag.name}: ${assert.diag.message}`;
      const stack = `${message}\n  at ${assert.diag.at}`;
      logger(assert.name, 'failed', { message, stack });
    }
  });

  parser.on('complete', results => {
    output.push('\n\n');
    if (results.failures.length > 0) {
      results.failures.forEach(failure => {
        output.push('  ' + format.red(failure.name) + ':\n');
        output.push(`    ${failure.diag.name}: ${failure.diag.message}\n`);
        output.push(`        at ${failure.diag.at}`);
      });
      output.push('\n\n');
    }

    if (!results.ok) {
      handler.failed = true;
    }
  });

  return stream;
};

const handler = makeStream();

process.stdin
  .pipe(handler)
  .pipe(process.stdout);

process.on('exit', status => {
  if (status > 0 || handler.failed) {
    process.exit(1);
  }
});