OUTPUT=output
mkdir -p $OUTPUT
cp _build/lib_test/test.native $OUTPUT/irmin-test.exe
cp _build/bin/main.native $OUTPUT/irmin.exe
cp "/usr/x86_64-w64-mingw32/sys-root/mingw/bin/libgmp-10.dll" $OUTPUT
cp "/usr/x86_64-w64-mingw32/sys-root/mingw/bin/zlib1.dll" $OUTPUT
