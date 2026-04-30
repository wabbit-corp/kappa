module OracleImportTests

open System
open System.IO
open Kappa.OracleImport
open Xunit

let private writeAllText (path: string) (text: string) =
    Directory.CreateDirectory(Path.GetDirectoryName(path)) |> ignore
    File.WriteAllText(path, text.Replace("\r\n", "\n"))

let private withTempDirectory action =
    let path = Path.Combine(Path.GetTempPath(), $"kappa-oracle-import-{Guid.NewGuid():N}")
    Directory.CreateDirectory(path) |> ignore

    try
        action path
    finally
        if Directory.Exists(path) then
            Directory.Delete(path, true)

let private writeTestsManifest (repoRoot: string) (entries: string list) =
    let filesJson =
        entries
        |> List.map (fun sourcePath ->
            $"""{{"source_path":"{sourcePath}","path":"{sourcePath}","bytes":1,"hash":"x","classification":"test"}}""")
        |> String.concat ","

    let text =
        $$"""
        {
          "kind": "tests_manifest",
          "generated_at": "2026-04-27T00:00:00Z",
          "roots": [],
          "files": [{{filesJson}}]
        }
        """

    writeAllText (Path.Combine(repoRoot, "tests", "manifest.json")) text

[<Fact>]
let ``scanRepo translates a Kotlin branching box fixture`` () =
    withTempDirectory (fun corpusRoot ->
        let repoRoot = Path.Combine(corpusRoot, "repos", "youtrack-youtrack.jetbrains.com-kt")
        let sourcePath = "compiler/testData/codegen/box/branching/if_else.kt"

        writeTestsManifest repoRoot [ sourcePath ]

        writeAllText
            (Path.Combine(repoRoot, "tests", sourcePath))
            """
            // WITH_STDLIB

            import kotlin.test.*

            fun if_else(b: Boolean): Int {
              if (b) return 42
              else   return 24
            }

            fun box(): String {
              val res = if_else(false)
              if (res != 24) return "FAIL: $res"

              return "OK"
            }
            """

        let report = OracleImport.scanRepo corpusRoot "youtrack-youtrack.jetbrains.com-kt" [] None

        let fixture = Assert.Single(report.Fixtures)
        Assert.Equal("kotlin_box_branching_if_else", fixture.AdapterId)
        Assert.Contains("let if_else b =", fixture.MainFileText)
        Assert.Contains("let box () = block", fixture.MainFileText)
        Assert.Contains("f\"FAIL: ${res}\"", fixture.MainFileText)
        Assert.Contains("--! assertEval result \"OK\"", fixture.MainFileText))

[<Fact>]
let ``scanRepo translates a Kotlin null Elvis chain fixture`` () =
    withTempDirectory (fun corpusRoot ->
        let repoRoot = Path.Combine(corpusRoot, "repos", "youtrack-youtrack.jetbrains.com-kt")
        let sourcePath = "compiler/testData/codegen/box/elvis/nullNullOk.kt"

        writeTestsManifest repoRoot [ sourcePath ]

        writeAllText
            (Path.Combine(repoRoot, "tests", sourcePath))
            """
            fun box() = null ?: null ?: "OK"
            """

        let report = OracleImport.scanRepo corpusRoot "youtrack-youtrack.jetbrains.com-kt" [] None

        let fixture = Assert.Single(report.Fixtures)
        Assert.Equal("kotlin_box_null_elvis_chain", fixture.AdapterId)
        Assert.Contains("import std.prelude.(type Option, ctor None)", fixture.MainFileText)
        Assert.Contains("(leftNone ?: rightNone) ?: \"OK\"", fixture.MainFileText))

[<Fact>]
let ``scanRepo translates a Scala guarded Option match fixture`` () =
    withTempDirectory (fun corpusRoot ->
        let repoRoot = Path.Combine(corpusRoot, "repos", "github-scala-scala3")
        let sourcePath = "tests/patmat/i8708.scala"

        writeTestsManifest repoRoot [ sourcePath ]

        writeAllText
            (Path.Combine(repoRoot, "tests", sourcePath))
            """
            object Main {
              def foo(x: Option[Int]): Int = x match {
                case Some(n) if n % 2 == 0 => n
                case None => 0
              }

              def main(args: Array[String]): Unit = println(foo(Some(1)))
            }
            """

        let report = OracleImport.scanRepo corpusRoot "github-scala-scala3" [] None

        let fixture = Assert.Single(report.Fixtures)
        Assert.Equal("scala_option_guard_non_exhaustive", fixture.AdapterId)
        Assert.Contains("case Some n if n % 2 == 0 -> n", fixture.MainFileText)
        Assert.Contains("""--! assertDiagnosticMatch exhaustive|non-exhaustive|guard""", fixture.MainFileText))

[<Fact>]
let ``emitFixtures writes generated fixture directories and summary`` () =
    withTempDirectory (fun corpusRoot ->
        let repoRoot = Path.Combine(corpusRoot, "repos", "youtrack-youtrack.jetbrains.com-kt")
        let sourcePath = "compiler/testData/codegen/box/branching/when2.kt"

        writeTestsManifest repoRoot [ sourcePath ]

        writeAllText
            (Path.Combine(repoRoot, "tests", sourcePath))
            """
            // WITH_STDLIB

            import kotlin.test.*

            fun when2(i: Int): Int {
              when (i) {
                0 -> return 42
                else -> return 24
              }
            }

            fun box(): String {
              val res = when2(0)
              if (res != 42) return "FAIL $res"

              return "OK"
            }
            """

        let report = OracleImport.scanRepo corpusRoot "youtrack-youtrack.jetbrains.com-kt" [] None
        let outputRoot = Path.Combine(corpusRoot, "generated")
        let writtenDirectories = OracleImport.emitFixtures outputRoot report

        let writtenDirectory = Assert.Single(writtenDirectories)
        let mainFilePath = Path.Combine(writtenDirectory, "main.kp")
        let summaryPath = Path.Combine(outputRoot, "summary.json")

        Assert.True(File.Exists(mainFilePath), $"expected emitted fixture at {mainFilePath}")
        Assert.True(File.Exists(summaryPath), $"expected summary at {summaryPath}")
        Assert.Contains("when2", File.ReadAllText(mainFilePath))
        Assert.Contains("when2.kt", File.ReadAllText(summaryPath)))
