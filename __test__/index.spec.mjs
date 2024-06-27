import test from 'ava'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const fixturesDir = fileURLToPath(new URL('./fixtures', import.meta.url))

import { validate, FileType } from '../index.js'

test('use server basic example', async (t) => {
  const { fileType, error } = await validate(path.join(fixturesDir, 'basic.js'))
  t.is(fileType, FileType.ServerComponent)
  t.falsy(error)
})

test('use server with use client', async (t) => {
  const { error } = await validate(path.join(fixturesDir, 'use-server-and-client.js'))
  t.truthy(error)
  t.is(error, 'Cannot use both client and server')
})

test('import client and use server', async (t) => {
  const { error } = await validate(path.join(fixturesDir, 'import-client-and-use-server.js'))
  t.truthy(error)
  t.is(error, 'Cannot use both client and server')
})
