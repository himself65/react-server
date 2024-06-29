import test from 'ava'
import { validate } from '..'

test('test', async t => {
  await validate('./tests/fixtures/valid.yml')
})
