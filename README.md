# React Server

## Usage

```shell
npm add react-server-action
yarn add react-server-action
pnpm add react-server-action
```

```tsx
import { validate, reactServerAction } from 'react-server-action'

const isServerLayer = true
const result = validate('path/to/file', isServerLayer)
if (!result.error) {
  const code = await reactServerAction('path/to/file', '__prefix__', isServerLayer)
  console.log(code)
}
```

> Collection of React Server tools

## RSC rules

### Valid RSC

```tsx
'use server'
'use client'
// ❌
```

```tsx
'use server'
// ✅
```

```tsx
'use client'
// ✅
```

```tsx
'use client'
export const foo = async () => {
  'use server'
  return 'rsc'
}
// ❌
```

```tsx
export const foo = async () => {
  'use server'
  return 'rsc'
}
// ✅
```

### Register RSC

#### Option 1: export `__PREFIX__+name` in the same file

```tsx
import { registerServerReference } from 'react-server-dom-webpack/server'

export const foo = async () => {
  'use server'
  return 'rsc'
}

export const __prefix__foo = registerServerReference(foo, 'file_id', 'foo')
```

#### Option 2: register in local map

```tsx
import { registerServerReference } from 'react-server-dom-webpack/server'

export const rscMap = new Map()

function register (
  fn: Function,
  file: string,
  name: string,
) {
  registerServerReference(fn, file, name)
  rscMap.set(id, fn)
  return fn
}

export const foo = async () => {
  'use server'
  return 'rsc'
}

register(foo, 'file_id', 'foo')
```

### Transform RSC

#### Case 1: server action file

```ts
'use server'

export async function foo () {
  return 'rsc'
}

export function not_rsc () {
  return 'not_rsc'
}
```

⬇️

```ts
'use server'
import { registerServerReference } from 'react-server-dom-webpack/server'

export async function foo () {
  return 'rsc'
}

export function not_rsc () {
  return 'not_rsc'
}

export const __prefix__foo = registerServerReference(foo, 'file_id', 'foo')
```

#### Case 2: non-exported server action

```tsx
async function wrapFn (...args: any[]) {
  'use server'
}

export const App = () => {
  wrapFn()
  return <div>App</div>
}
```

⬇️

```tsx
import { registerServerReference } from 'react-server-dom-webpack/server'

async function wrapFn (...args: any[]) {
  'use server'
}

export const App = () => {
  wrapFn()
  return <div>App</div>
}

export const __prefix__wrapFn = registerServerReference(wrapFn, 'file_id', 'wrapFn')
```

Note that all server actions should be exported, otherwise server runtime cannot access them.

#### Case 3: server action in nested function

```tsx
import { Component } from '@/components'

export const App = (props) => {
  const foo = async (input: string) => {
    'use server'
    return JSON.stringify(props) + input;
  }
  return <Component action={foo} />
}
```

⬇️

```tsx
import { registerServerReference } from 'react-server-dom-webpack/server'
import { Component } from '@/components'

export const App = (props) => {
  const foo = __prefix__1.bind(null, [props])
  return <Component action={foo} />
}

export const __prefix__1 = registerServerReference(async (bound, input) => {
  'use server'
  const [props] = bound;
  return JSON.stringify(props) + input;
}, 'file_id', '__prefix__1')
```

You should save the context of the function correctly, otherwise the function will not work as expected.

#### Case 4: server action in jsx

```tsx
export const App = (props) => {
  return <div onClick={async () => {
    'use server'
    console.log(props)
  }} />
}
```

⬇️

```tsx
import { registerServerReference } from 'react-server-dom-webpack/server'

export const App = (props) => {
  return <div onClick={__prefix__action_1.bind(null, [props])} />
}

export const __prefix__action_1 = registerServerReference(async (bound) => {
  'use server'
  const [props] = bound
  console.log(props)
}, 'file_id', '__prefix__action_1')
```

## LICENSE

MIT
