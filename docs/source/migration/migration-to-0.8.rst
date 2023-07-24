Migration to 0.8.x
==================

Version 0.8.0 is is the first version which cleaned up the API. It introduced
several breaking changes:

- lifted transformers, deprecated in 0.7.0, got removed in favor of partial
  transformers. Since 0.7.0 migration guide from lifted to partial is available
  in `partial transformers section <partial-transformers/migrating-from-lifted.html>`_.
- ``.enableUnsafeOption`` option was removed - if ``Option`` unwrapping is
  needed, it is recommended to use
  `partial transformers section <partial-transformers/partial-transformers.html>`_
- TODO autoderivation description
