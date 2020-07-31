# Purescript-Cypress

A purescript wrapper for the Cypress.io testing framework.

Currently a ReaderT pattern on an opaque Cypress type.
Still working out the types and structure so please give any feedback.

## Local example
```
spago bundle-module --watch -t ./cypress/integration/ps.js
yarn run cypress open
```
