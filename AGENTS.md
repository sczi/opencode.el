# AGENTS.md - opencode.el

Emacs frontend for OpenCode LLM coding assistant server.

# Tool use

Interact with the user's emacs process to perform introspection, find
information about functions, values of variables etc. Use it to run and verify
that code you write is working correctly.

## Verification Commands

DO NOT byte-compile to check for warnings. After you are finished you will be
presented all warnings and errors from emacs' flycheck, including byte compiler
warnings.
DO NOT run all tests, they will also be automatically run after you are finished
and you will be notified if any failed.

DO use emacs_eval_elisp to perform runtime verification that the code you edited
works as expected.

### Async HTTP Pattern

This codebase uses macro-generated async API calls:

```elisp
;; The macro opencode-api-session expands to async plz call
(opencode-api-session (session-id)
    result-var
  ;; This body runs in callback with result-var bound
  (do-something-with result-var))
```

### Choosing a test

Integration tests are slow, so use the cheapest test that covers the change:

- **No server involved**: use a unit test in a fake session buffer
(`with-opencode-session-test-buffer` in `opencode-test-helpers.el`).  This
covers buffer rendering, prompt and input handling, parsing, and code paths that
finish or error before any API call.
- **Anything that needs to talk to the server**: use an integration test against
a real server (below).  Don't mock the API, or hand-write events or responses to
fake a server; those test against our guess of the protocol, not the real one.

If a unit test needs a fake server to work, make it an integration test.

### Tests against a real server

`opencode-integration-test-helpers.el` starts one shared server per test run,
isolated in a temp directory, in opencode's simulation mode, where the test
scripts the LLM's responses:

```elisp
(opencode-test-with-session '(("a.txt" . "alpha\n"))   ; files of a new project
  (opencode-test-llm-reply
   '((:reasoning "Let me look") (:tool "read" ((path . "a.txt"))))
   "It says alpha.")
  (opencode-test-send "What's in a.txt?")  ; waits until back at a prompt
  (should (string-search "It says alpha." (opencode-test-output))))
```

See `opencode-test-llm-respond` for the response parts, including `(:hold)` to
finish a response yourself.  Never use `sit-for` to wait, use
`opencode-test-wait-until`.

### Logging

- Log warnings to dedicated buffer: `*opencode-event-log*`
  use `(opencode--log-event "WARNING TYPE" "msg")`

### Opencode 

The OpenAPI spec for the OpenCode server is in ~/opencode/packages/protocol/openapi.json

The source code to OpenCode itself is in ~/opencode, if you need to look at it's
source code to find how it does something, use the explore subagent.
