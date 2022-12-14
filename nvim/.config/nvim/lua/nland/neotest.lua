local status_ok, neotest = pcall(require, "neotest")
if not status_ok then
  return
end

local status_ok_jest, _ = pcall(require, "neotest-jest")
if not status_ok_jest then
  return
end

neotest.setup({
  adapters = {
    require("neotest-jest")({
      jestCommand = "npm test --",
      jestConfigFile = "custom.jest.config.ts",
      env = { CI = true },
      cwd = function(path)
        return vim.fn.getcwd()
      end,
    }),
  },
})
