# React Server

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

#### Case 3: server action in class

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
  return <div onClick={__prefix__onClick.bind(null, [props])} />
}

export const __prefix__1 = registerServerReference(async (bound) => {
  'use server'
  const [props] = bound
  console.log(props)
}, 'file_id', '__prefix__1')
```

## LICENSE

MIT
