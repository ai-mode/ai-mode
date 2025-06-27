## Modify action examples

### Example 1

    <example>
      <user-query>
        Add input validation so `divide(a, b)` raises `ValueError` if `b` is zero.
      </user-query>

      <input>
        <modify-action-object>
          <selection>
    def divide(a, b):
        return a / b
          </selection>
        </modify-action-object>
      </input>

      <output>
    def divide(a, b):
        if b == 0:
            raise ValueError("Division by zero is not allowed")
        return a / b
      </output>
    </example>


### Example 2

    <example>
      <user-query>
        Switch manual index loop to `range` and add error when slice is empty.
      </user-query>

      <input>
        <modify-action-object>
          <selection>
    func Avg(nums []float64) float64 {
        total := 0.0
        for i := 0; i < len(nums); i++ {
            total += nums[i]
        }
        return total / float64(len(nums))
    }
          </selection>
        </modify-action-object>
      </input>

      <output>
    func Avg(nums []float64) (float64, error) {
        if len(nums) == 0 {
            return 0, fmt.Errorf("cannot compute average of empty slice")
        }
        total := 0.0
        for _, n := range nums {
            total += n
        }
        return total / float64(len(nums)), nil
    }
      </output>
    </example>
