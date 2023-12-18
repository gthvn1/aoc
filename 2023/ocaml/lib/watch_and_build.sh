[[ -z "$1" ]] && ( echo "file is missing"; exit 1 )

while ! inotifywait -e close_write,modify "$1"; do
  ocamlopt -o $1.exe $1
  clear;
  echo "Build done";
  echo "= Build done; Running ===============================";
  ./$1.exe;
  echo "= $(date) ===================";
done
