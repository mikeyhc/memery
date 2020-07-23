function foreach(fun, list) {
  for (var i = 0; i < list.length; i++) {
    fun(list[i]);
  }
}

fetch('./api/memes')
  .then(response => response.json())
  .then(data => {
    var meme_element = document.getElementById("memes");
    foreach(meme => meme_element.appendChild(build_html_meme(meme)), data);
  });

function build_html_meme(meme) {
  console.log(meme);
  var meme_holder = document.createElement("section");
  meme_holder.className = "meme-holder";

  var figure = document.createElement("figure");

  var img = document.createElement("img");
  img.src = meme.path;
  img.className = "meme";

  var figcaption = document.createElement("figcaption");
  figcaption.innerHTML = meme.name;

  figure.appendChild(img);
  figure.appendChild(figcaption);
  meme_holder.appendChild(figure);

  return meme_holder;
}
