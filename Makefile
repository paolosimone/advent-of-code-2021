SRC_DIR	= AdventOfCode
TEST_DIR	= AdventOfCode.Tests

.PHONY: run
run:
	dotnet run -c Release --project $(SRC_DIR) $(day)

opts = ""
.PHONY: test
test:
	dotnet test $(opts) $(TEST_DIR)

.PHONY: format
format: 
	fantomas -r $(SRC_DIR)
	dotnet fsharplint lint $(SRC_DIR)
	fantomas -r $(TEST_DIR)
	dotnet fsharplint lint $(TEST_DIR)