function upload() {
  var fr = new FileReader();
  fr.onload = function() {
    /* TODO do some basic validation */
    name = document.getElementById("name").value;
    description = document.getElementById("description").value;
    tags = document.getElementById("tags").value;
    file = document.getElementById("file").value;
    data = btoa(this.result);
    form = {
      "name": name,
      "description": description,
      "tags": tags.split(" "),
      "filename": file,
      "data": data
    }
    fetch('/api/meme', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(form)
    })
    .then(response => response.json())
    .then(data => alert("SUCCESS!"))
    .catch((error) => {
      alert("ERROR!");
    });
  };
  fr.readAsBinaryString(document.getElementById("file").files[0]);
}
