## Fix action examples


### Example 1

    <example>
      <input>
        <modify-action-object>
          <selection>
        def add_umbers(a, b):
            return a + b
        print(add_numbers(2))
          </selection>
        </modify-action-object>
      </input>

      <output>
        def add_numbers(a, b):
            return a + b
        print(add_numbers(2, 3))
      </output>
    </example>

### Example 2

    <example>
      <input>
        <modify-action-object>
          <selection>
        function isEven(n) {
            return n % 2 = 0;
        }
          </selection>
        </modify-action-object>
      </input>

      <output>
        function isEven(n) {
            return n % 2 === 0;
        }
      </output>
    </example>

### Example 3

    <example>
      <input>
        <modify-action-object>
          <selection>
        func divide(a, b int) int {
            return a / b
        }
          </selection>
        </modify-action-object>
      </input>

      <output>
        func divide(a, b int) int {
            if b == 0 {
                return 0
            }
            return a / b
        }
      </output>
    </example>
