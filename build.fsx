// include Fake libs
#r "./packages/build/FAKE/tools/FakeLib.dll"
#r "System.IO.Compression.FileSystem"

open System
open System.IO
open Fake
open Fake.NpmHelper
open Fake.ReleaseNotesHelper
open Fake.Git

let gitName = "fable-github-client"
let gitOwner = "YoloDev"
let gitHome = sprintf "https://github.com/%s" gitOwner

// Filesets
let projects =
  !! "src/**/*.fsproj"

let testProjects =
  !! "test/**/*.fsproj"

let testDir = FullName "./test"
let jsCompiler = FullName "./test/build.js"

// --------------------------------------------------------------------------------------
// Install

let dotnetSDKPath = FullName "./dotnetsdk"
let dotnetcliVersion = "1.0.1"
let dotnetExePath = dotnetSDKPath </> (if isWindows then "dotnet.exe" else "dotnet")

let runDotnet workingDir args =
  printfn "CWD: %s" workingDir
  // printfn "dotnet %s" args
  let result =
    ExecProcess (fun info ->
      info.FileName <- dotnetExePath
      info.WorkingDirectory <- workingDir
      info.Arguments <- args) TimeSpan.MaxValue
  match result with
  | 0 -> ()
  | _ -> failwithf "Command failed: dotnet %s" args

let runYarn workingDir args =
  printfn "CWD: %s" workingDir
  // printfn "yarn %s" args
  let result =
    ExecProcess (fun info ->
      info.FileName <- "yarn"
      info.WorkingDirectory <- workingDir
      info.Arguments <- args) TimeSpan.MaxValue
  match result with
  | 0 -> ()
  | _ -> failwithf "Command failed: yarn %s" args

Target "InstallDotNetCore" (fun _ ->
  let correctVersionInstalled = 
    try
      let processResult = 
        ExecProcessAndReturnMessages (fun info ->
          info.FileName <- dotnetExePath
          info.WorkingDirectory <- Environment.CurrentDirectory
          info.Arguments <- "--version") (TimeSpan.FromMinutes 30.)
      
      processResult.Messages |> separated "" = dotnetcliVersion
    with
    | _ -> false
  
  match correctVersionInstalled with
  | true -> tracefn "dotnetcli %s already installed" dotnetcliVersion
  | false ->
    CleanDir dotnetSDKPath
    let archiveFileName =
      match isWindows, isLinux with
      | true, _ -> sprintf "dotnet-dev-win-x64.%s.zip" dotnetcliVersion
      | _, true -> sprintf "dotnet-dev-ubuntu-x64.%s.tar.gz" dotnetcliVersion
      | _, _    -> sprintf "dotnet-dev-osx-x64.%s.tar.gz" dotnetcliVersion
    
    let downloadPath =
      sprintf "https://dotnetcli.azureedge.net/dotnet/Sdk/%s/%s" dotnetcliVersion archiveFileName
    
    let localPath = Path.Combine(dotnetSDKPath, archiveFileName)
    tracefn "Installing '%s' to '%s'" downloadPath localPath

    use webclient = new Net.WebClient()
    webclient.DownloadFile(downloadPath, localPath)

    match isWindows with
    | true  -> Compression.ZipFile.ExtractToDirectory(localPath, dotnetSDKPath)
    | false ->
      let assertExitCodeZero = function
        | 0 -> ()
        | x -> failwithf "Command failed with exit code %i" x
      
      Shell.Exec("tar", sprintf """-xvf "%s" -C "%s" """ localPath dotnetSDKPath)
      |> assertExitCodeZero
    
    tracefn "dotnet cli path - %s" dotnetSDKPath
    
    System.IO.Directory.EnumerateFiles dotnetSDKPath
    |> Seq.iter (fun path -> tracefn " - %s" path)

    System.IO.Directory.EnumerateDirectories dotnetSDKPath
    |> Seq.iter (fun path -> tracefn " - %s%c" path System.IO.Path.DirectorySeparatorChar)
)

let forAll projects args =
  projects
  |> Seq.iter (fun s ->
    let dir = IO.Path.GetDirectoryName s
    runDotnet dir args
  )

let forAllProjects = forAll projects
let forAllTests = forAll testProjects

Target "Install" (fun _ -> forAllProjects "restore")

// --------------------------------------------------------------------------------------
// Build

Target "Build" (fun _ -> forAllProjects "build")

let release = LoadReleaseNotes "RELEASE_NOTES.md"

Target "Meta" (fun _ ->
  [ "<Project xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">"
    "<PropertyGroup>"
    "<Description>Fable GitHub Client</Description>"
    sprintf "<PackageProjectUrl>http://%s.github.io/%s</PackageProjectUrl>" gitOwner gitName
    sprintf "<PackageLicenseUrl>https://raw.githubusercontent.com/%s/%s/master/LICENSE.md</PackageLicenseUrl>" gitOwner gitName
    sprintf "<RepositoryUrl>%s/%s</RepositoryUrl>" gitHome gitName
    "<PackageTags>fable;github</PackageTags>"
    "<Authors>Alxandr</Authors>" 
    sprintf "<Version>%s</Version>" (string release.SemVer)
    "</PropertyGroup>"
    "</Project>" ]
  |> WriteToFile false "Meta.props"
)

// --------------------------------------------------------------------------------------
// Test

Target "TestInstall" (fun _ -> forAllTests "restore")

Target "TestBuild" (fun _ -> 
  forAllTests "build"
  testProjects
  |> Seq.iter (fun proj ->
    let dir = IO.Path.GetDirectoryName proj
    let args = sprintf "fable node-run \"%s\" -- \"%s\"" jsCompiler <| FullName proj
    runDotnet dir args
  )
)

Target "Test" (fun _ ->
  let avaArgs = "test"
  runYarn testDir avaArgs
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "Package" (fun _ -> forAllProjects "pack")

Target "PublishNuget" (fun _ -> forAllProjects "push")

Target "Publish" DoNothing

// TODO: Temp
Target "GenerateDocs" DoNothing
Target "ReleaseDocs" DoNothing

// Build order
"Meta"
  ==> "InstallDotNetCore"
  ==> "Install"
  ==> "Build"
  ==> "Package"

"Build"
  ==> "GenerateDocs"
  ==> "ReleaseDocs"

"Build"
  ==> "TestInstall"
  ==> "TestBuild"
  ==> "Test"
//"TestBuild" ==> "Test"

"Publish"
  <== [ "Build"
        "PublishNuget"
        "ReleaseDocs" ]

// start build
RunTargetOrDefault "Test"