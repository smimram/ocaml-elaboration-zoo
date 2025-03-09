Compared to previous versions this implementation

- adds support for implicit arguments, which are handled by inserting meta-variables during elaboration
- modularizes the code:
  - it is split in various modules
  - environments (for types, values, etc.) are packed in one record
- add support for position in errors
