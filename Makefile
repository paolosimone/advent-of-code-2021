SRC_DIR	= AdventOfCode
TEST_DIR	= AdventOfCode.Tests

.PHONY: run
run:
	dotnet run -c Release --project $(SRC_DIR) $(day)

.PHONY: test
test:
	dotnet test $(TEST_DIR)

.PHONY: test-day
test-day:
	dotnet test --filter $(day) $(TEST_DIR)

.PHONY: format
format: 
	fantomas -r $(SRC_DIR)
	dotnet fsharplint lint $(SRC_DIR)
	fantomas -r $(TEST_DIR)
	dotnet fsharplint lint $(TEST_DIR)