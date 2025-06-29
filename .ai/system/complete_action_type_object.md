## How to Apply Instructions to `<complete-action-object>`

Used for generating a continuation — such as completing a sentence, finishing a function, or inserting the next logical item.

### Complete action type object structural format:**

    <example>
      <complete-action-object>
        <preceding-context ...>...</preceding-context>
        <cursor/>
        <following-context ...>...</following-context>
      </complete-action-object>
    </example>

### Complete action object rules

<complete-action-rules>
1. Insert new content at the <cursor/> position, based on the instruction.
2. Use both preceding and following context to inform the output.
3. Return only the inserted content — no surrounding context, formatting, tags, quotes, or comments unless explicitly instructed.
</complete-action-rules>
