#!/usr/bin/env dotnet fsi

open System
open System.Diagnostics
open System.IO
open System.Net.Http

let scriptUrl = "https://git.io/tusk"
let scriptPath = Path.Combine(Path.GetTempPath(), "install-tusk.sh")

// Download installer script
let client = new HttpClient()
let response = client.GetAsync(scriptUrl).Result
response.EnsureSuccessStatusCode() |> ignore
let script = response.Content.ReadAsStringAsync().Result

// Write to temp file
File.WriteAllText(scriptPath, script)

// Execute script
let proc = new Process()
proc.StartInfo.FileName <- "bash"
proc.StartInfo.Arguments <- $"{scriptPath} -b tools"
proc.StartInfo.UseShellExecute <- false
proc.Start() |> ignore
proc.WaitForExit()

// Cleanup
File.Delete(scriptPath)

if proc.ExitCode = 0 then
    printfn "✓ Tusk installed successfully"
else
    printfn "✗ Failed to install tusk"
    exit 1
