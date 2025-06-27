## EXPLAIN_ACTION_OBJECT

Used for explaining, interpreting, or summarizing a fragment of text enclosed within `<selection>` or `<file-context>` tags.

**Explain action type object rules:**

<explain-action-object-rules>
1. Do not modify the original content.
2. Use exactly one of the following to identify the scope:
   - `<selection ...>...</selection>`
   - `<file-context ...>...</file-context>`
</explain-action-object-rules>

**Modify action type object structural format:**

    <example>
      <explain-action-object>
        <selection ...>...</selection>
        <!-- or -->
        <file-context ...>...</file-context>
      </explain-action-object>
    </example>

**Example:**

    <example>
      <user-query>What does this loop do?</user-query>
      <explain-action-object>
        <selection>
          for i in range(10):
            if i % 2 == 0:
              print(i)
        </selection>
      </explain-action-object>
      <expected-output>This loop prints all even numbers from 0 to 9.</expected-output>
    </example>
