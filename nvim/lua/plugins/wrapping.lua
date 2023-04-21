local setup, wrapping = pcall(require, "wrapping")
if not setup then
  return
end

wrapping.setup()
