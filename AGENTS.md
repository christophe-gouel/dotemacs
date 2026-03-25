# AGENTS.md

## Emacs Config Workflow

- Treat `README.org` as the source of truth for configuration changes.
- Do not edit `init.el` directly.
- `init.el` is automatically generated when `README.org` is saved/tangled.
- When changing configuration, update `README.org` only and let the normal Org workflow regenerate `init.el`.
