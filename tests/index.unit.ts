import test from 'ava'
import { validate, reactServerAction } from '..'

test('function exist', async (t) => {
  t.is(typeof validate, 'function')
  t.is(typeof reactServerAction, 'function')
})
