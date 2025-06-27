# Instructions for command: create commit message

<task>
Create a clear, informative commit message that follows conventional commit standards and accurately describes the changes made to the codebase.
</task>

## Instructions
1. **Analyze the diff/changes** provided to understand what was modified, added, or removed
2. **Use conventional commit format**: `type(scope): description`
   - **Types**: feature, fix, docs, style, refactor, test, chore, perf, ci, build
   - **Scope**: Optional, indicates the area of change (e.g., auth, ui, api)
   - **Description**: Brief summary in imperative mood (max 50 chars for subject line)

3. **Write the commit message with**:
   - **Subject line**: Concise summary using imperative mood ("Add", "Fix", "Update")
   - **Body** (if needed): Explain what and why, not how (wrap at 72 characters)
   - **Footer** (if applicable): Reference issues, breaking changes

## Guidelines
- Use present tense, imperative mood ("Add feature" not "Added feature")
- Capitalize the first letter of the subject line
- No period at the end of the subject line
- Keep subject line under 50 characters
- Separate subject from body with a blank line
- Focus on the business impact and reasoning for the change
- Include breaking change notes if applicable

## Examples
```
feature(auth): add OAuth2 integration for user login

Implement OAuth2 authentication flow to improve user experience
and security. Supports Google and GitHub providers.

Closes #123
```

```
fix(ui): resolve button alignment issue in mobile view

The submit button was overlapping with form fields on screens
smaller than 768px. Adjusted margin and padding values.
```

```
docs: update installation instructions

Add missing dependency requirements and clarify setup steps
for new contributors.
```

## Output Format
Provide only the commit message in the specified format, ready to use with `git commit -m`.
