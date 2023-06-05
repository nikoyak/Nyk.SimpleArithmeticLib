module Changelog

open System
open Fake.Core
open Fake.IO

let isEmptyChange =
    function
    | Changelog.Change.Added s
    | Changelog.Change.Changed s
    | Changelog.Change.Deprecated s
    | Changelog.Change.Fixed s
    | Changelog.Change.Removed s
    | Changelog.Change.Security s
    | Changelog.Change.Custom(_, s) -> String.IsNullOrWhiteSpace s.CleanedText

let tagFromVersionNumber versionNumber = sprintf "v%s" versionNumber

let failOnEmptyChangelog (latestEntry: Changelog.ChangelogEntry) =
    let isEmpty =
        (latestEntry.Changes
            |> Seq.forall isEmptyChange)
        || latestEntry.Changes
            |> Seq.isEmpty

    if isEmpty then
        failwith
            "No changes in CHANGELOG. Please add your changes under a heading specified in https://keepachangelog.com/"

let mkLinkReference (newVersion: SemVerInfo) (changelog: Changelog.Changelog) gitHubRepoUrl =
    if
        changelog.Entries
        |> List.isEmpty
    then
        // No actual changelog entries yet: link reference will just point to the Git tag
        sprintf
            "[%s]: %s/releases/tag/%s"
            newVersion.AsString
            gitHubRepoUrl
            (tagFromVersionNumber newVersion.AsString)
    else
        let versionTuple version =
            (version.Major, version.Minor, version.Patch)
        // Changelog entries come already sorted, most-recent first, by the Changelog module
        let prevEntry =
            changelog.Entries
            |> List.skipWhile (fun entry ->
                entry.SemVer.PreRelease.IsSome
                && versionTuple entry.SemVer = versionTuple newVersion
            )
            |> List.tryHead

        let linkTarget =
            match prevEntry with
            | Some entry ->
                sprintf
                    "%s/compare/%s...%s"
                    gitHubRepoUrl
                    (tagFromVersionNumber entry.SemVer.AsString)
                    (tagFromVersionNumber newVersion.AsString)
            | None ->
                sprintf
                    "%s/releases/tag/%s"
                    gitHubRepoUrl
                    (tagFromVersionNumber newVersion.AsString)

        sprintf "[%s]: %s" newVersion.AsString linkTarget

let mkReleaseNotes changelog (latestEntry: Changelog.ChangelogEntry) gitHubRepoUrl =
    let linkReference = mkLinkReference latestEntry.SemVer changelog gitHubRepoUrl

    if String.isNullOrEmpty linkReference then
        latestEntry.ToString()
    else
        // Add link reference target to description before building release notes, since in main changelog file it's at the bottom of the file
        let description =
            match latestEntry.Description with
            | None -> linkReference
            | Some desc when desc.Contains(linkReference) -> desc
            | Some desc -> sprintf "%s\n\n%s" (desc.Trim()) linkReference

        { latestEntry with
            Description = Some description
        }
            .ToString()

let getVersionNumber envVarName ctx =
    let args = ctx.Context.Arguments

    let verArg =
        args
        |> List.tryHead
        |> Option.defaultWith (fun () -> Environment.environVarOrDefault envVarName "")

    if SemVer.isValid verArg then
        verArg
    elif
        verArg.StartsWith("v")
        && SemVer.isValid verArg.[1..]
    then
        let target = ctx.Context.FinalTarget

        Trace.traceImportantfn
            "Please specify a version number without leading 'v' next time, e.g. \"./build.sh %s %s\" rather than \"./build.sh %s %s\""
            target
            verArg.[1..]
            target
            verArg

        verArg.[1..]
    elif String.isNullOrEmpty verArg then
        let target = ctx.Context.FinalTarget

        Trace.traceErrorfn
            "Please specify a version number, either at the command line (\"./build.sh %s 1.0.0\") or in the %s environment variable"
            target
            envVarName

        failwith "No version number found"
    else
        Trace.traceErrorfn
            "Please specify a valid version number: %A could not be recognized as a version number"
            verArg

        failwith "Invalid version number"
