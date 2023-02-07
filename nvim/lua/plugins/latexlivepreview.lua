local latexlivepreview_setup, latexlivepreview = pcall(require, "latexlivepreview")
if not latexlivepreview_setup then
	return
end

latexlivepreview.setup()
