## How to Apply Instructions to `<explain-action-object>`


Used for explaining, interpreting, or summarizing a fragment of textual information located between the `<selection>` or `<file-context>` tags — such as code analysis, algorithm explanations, or documentation summaries.

### Explain action type object structural format

    <example>
      <explain-action-object>
        <preceding-context ...>...context before...</preceding-context>
        <selection ...>...text fragment...</selection>
        <following-context ...>...context after...</following-context>
        <!-- or -->
        <file-context file="...">...text fragment...</file-context>
        <!-- or for multiple files -->
        <multi-file-context>
          <file-context file="...">...text fragment...</file-context>
          <file-context file="...">...text fragment...</file-context>
        </multi-file-context>
      </explain-action-object>
    </example>



<explain-action-object-rules>
1. Analyze and explain the content between the tags (`<selection>`, `<file-context>`, or `<multi-file-context>`) based on the instruction.
2. Do not modify the original content — provide explanations only.
3. Return only raw explanatory text — no backticks, tags, wrappers, or code blocks.
4. Use exactly one of the following to identify the scope:
   - `<selection ...>...</selection>`
   - `<file-context ...>...</file-context>`
   - `<multi-file-context>...</multi-file-context>`
5. Context blocks `<preceding-context ...>...</preceding-context>` and `<following-context ...>...</following-context>` provide surrounding file content for reference only — they are **read-only** and should **never be included** in your output.
6. Use the context blocks to better understand the code structure and provide more accurate explanations that consider the broader context.
7. For multi-file explanations, provide coordinated analysis that explains relationships and interactions between files.
8. Focus on clarity and educational value — explain not just what the code does, but how and why it works.
9. Include relevant technical details, potential edge cases, and best practices when appropriate.
10. Structure explanations logically, starting with high-level overview and diving into specifics as needed.
</explain-action-object-rules>
