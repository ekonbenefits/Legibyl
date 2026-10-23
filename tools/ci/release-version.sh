#!/usr/bin/env bash
# The version the build stamps (Directory.Build.props), as the step's "version" output. For a tag
# (REF_TYPE, REF_NAME), fails unless it names that version: v<version>, or v<version>-<suffix> for
# a prerelease.
set -euo pipefail
version=$(dotnet msbuild src/Legibyl.Cli/Legibyl.Cli.fsproj -getProperty:Version | tr -d '\r')
if [ "${REF_TYPE:-}" = tag ]; then
    case "$REF_NAME" in
        "v$version" | "v$version"-?*) ;;
        *)
            echo "::error title=Version::The tag $REF_NAME doesn't name the version the build stamps, $version: tag v$version (or v$version-<suffix> for a prerelease)."
            exit 1
            ;;
    esac
fi
echo "Version $version"
echo "version=$version" >> "${GITHUB_OUTPUT:-/dev/null}"
