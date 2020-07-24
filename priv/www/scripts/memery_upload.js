function upload() {
  fr = new FileReader();
  fr.onload = function() {
    console.log("loaded file");
    console.log(fr.result);
    console.log(document.getElementById("file").files[0].name);
  };
  fr.readAsBinaryString(document.getElementById("file").files[0]);
}
