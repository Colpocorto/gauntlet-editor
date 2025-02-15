Dim fso
Set fso = WScript.CreateObject("Scripting.Filesystemobject")

Dim folder
Set folder = fso.GetFolder(".")

Dim files
Set files = folder.Files

For Each f In Files
	WScript.Echo f.Name
Next
