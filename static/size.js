function tt(res) {
  const snip = document.getElementById("snip" + res.dataset.docid);
  const par = res.parentElement;
  if (snip) {
    snip.classList.add("active-tooltip");
    snip.style.left = par.style.left;
    const top = par.style.top;
    // need to chop off the "px"
    snip.style.top = +top.substring(0, top.length - 2) + 16;
  }
}

function untt(res) {
  const snip = document.getElementById("snip" + res.dataset.docid);
  if (snip) {
    snip.classList.remove("active-tooltip");
  }
}

function saveWindowSize() {
  document.cookie = "size=" + window.innerWidth + "," + window.innerHeight + ";path=/";

}

window.onload = saveWindowSize;
window.onresize = saveWindowSize;
