function tt(res) {
  const snip = document.getElementById("snip" + res.dataset.docid);
  const r = res.getBoundingClientRect();
  if (snip) {
    snip.classList.add("active-tooltip");
    snip.style.left = r.left + window.scrollX;
    snip.style.top = r.top + r.height + window.scrollY;
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
