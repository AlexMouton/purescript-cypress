const Main = require('./ps');

describe('My First Test', () => {
  it('Visits the Kitchen Sink', () => {
    Main.main(cy)();
  })
})
