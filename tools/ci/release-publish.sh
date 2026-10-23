#!/usr/bin/env bash
# Publishes the GUI and CLI for RID, self-contained, and packs them into artifacts/release/dist as
# VERSION's downloads. Windows' are left unpacked there (gui, cli), to be signed first.
set -euo pipefail
: "${RID:?}" "${VERSION:?}"
out=artifacts/release/out
dist=artifacts/release/dist
rm -rf "$out" "$dist"
mkdir -p "$out" "$dist"

# The layout models have to be in each (#230).
check_models() {
    for model in pp-doclayoutv3 table-transformer-structure-recognition; do
        if [ ! -d "$1/models/$model" ]; then
            echo "::error title=Publish::$1 has no models/$model."
            exit 1
        fi
    done
}

publish() {
    dotnet publish "$1" -c Release -r "$RID" --self-contained true -o "$2"
    check_models "$2"
}

publish src/Legibyl.Cli/Legibyl.Cli.fsproj "$out/cli"

case "$RID" in
    win-*)
        publish src/Legibyl.Gui/Legibyl.Gui.fsproj "$out/gui"
        mv "$out/gui" "$out/cli" "$dist"/
        ;;
    osx-*)
        dotnet msbuild src/Legibyl.Gui/Legibyl.Gui.fsproj -t:BundleApp -restore -p:Configuration=Release \
            -p:RuntimeIdentifier="$RID" -p:SelfContained=true -p:PublishDir="$PWD/$out/macos/"
        check_models "$out/macos/Legibyl.app/Contents/MacOS"
        hdiutil create -volname Legibyl -srcfolder "$out/macos/Legibyl.app" -ov -format UDZO "$dist/Legibyl-$VERSION-$RID.dmg"
        ;;
    linux-*)
        publish src/Legibyl.Gui/Legibyl.Gui.fsproj "$out/Legibyl-$VERSION-$RID"
        tar -C "$out" -czf "$dist/Legibyl-$VERSION-$RID.tar.gz" "Legibyl-$VERSION-$RID"
        ;;
    *)
        echo "::error title=Publish::$RID isn't a runtime Legibyl is released for."
        exit 1
        ;;
esac

if [ -d "$out/cli" ]; then
    mv "$out/cli" "$out/legibyl-cli-$VERSION-$RID"
    tar -C "$out" -czf "$dist/legibyl-cli-$VERSION-$RID.tar.gz" "legibyl-cli-$VERSION-$RID"
fi
ls -la "$dist"
