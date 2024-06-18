# swc-plugin-react-server

This is a swc plugin for [react server](https://github.com/reactjs/rfcs/blob/main/text/0188-server-components.md).

- [x] server action transform (port from [next.js](https://github.com/vercel/next.js/blob/f30e5dbb29e16b652172e92a2691bb6a0a75768d/packages/next-swc/crates/next-custom-transforms/src/lib.rs))

## Usage

```ts
import { transform } from '@swc/core'
import { createRequire } from 'node:module'

const require = createRequire(import.meta.url)

const output = await transform(`
  async function foo() {
    "use server";
    return 0;
  }
  export async function bar() {
    "use server";
    return "Hello world";
  }
`, {
  swcrc: false,
  jsc: {
    target: 'esnext',
    experimental: {
      plugins: [
        [
          require.resolve('swc-plugin-react-server'), {}
        ]
      ]
    }
  }
})
```

## LICENSE

MIT
